library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(cowplot)
library(ggrepel)
library(plyr)
library(superheat)
library(ggpubr)

### GET DATA

# https://data.gov.il/dataset/covid-19/resource/57410611-936c-49a6-ac3c-838171055b1f/download/vaccinated-per-day-2021-02-02.csv
vac_data = read.csv('~/Documents/covid_analyses/vaccinated-per-day-2021-02-06.csv',header = T,row.names = NULL,stringsAsFactors = F)

### 

rossman = read.csv('~/Documents/covid_analyses/Fig2_national_3rd_lockdown_y_vectors_daily.csv',header = T,row.names = NULL,stringsAsFactors = F)
rossman$date = as.Date(rossman$X)

rossman_cases = rossman
rossman_cases$amount = (rossman[,'y_pos_3_national_60.']+rossman[,'y_pos_3_national_0.59'])

rossman_hosp = rossman
rossman_hosp$amount = (rossman[,'y_hosped_3_national_60.']+rossman[,'y_hosped_3_national_0.59'])

rossman_severe = rossman
rossman_severe$amount = (rossman[,'y_sevhosped_3_national_60.']+rossman[,'y_sevhosped_3_national_0.59'])


### ORGANZIE DATA

A = vac_data$age_group %in% c('60-69','70-79','80-89','90+')
vac_1dose_old = aggregate(as.numeric(vac_data$first_dose[A]),by=list(vac_data$VaccinationDate[A]),sum,na.rm=T)
vac_2dose_old = aggregate(as.numeric(vac_data$second_dose[A]),by=list(vac_data$VaccinationDate[A]),sum,na.rm=T)

A = !(vac_data$age_group %in% c('60-69','70-79','80-89','90+'))
vac_1dose_yng = aggregate(as.numeric(vac_data$first_dose[A]),by=list(vac_data$VaccinationDate[A]),sum,na.rm=T)
vac_2dose_yng = aggregate(as.numeric(vac_data$second_dose[A]),by=list(vac_data$VaccinationDate[A]),sum,na.rm=T)

vac_1dose = aggregate(as.numeric(vac_data$first_dose),by=list(vac_data$VaccinationDate),sum,na.rm=T)
vac_2dose = aggregate(as.numeric(vac_data$second_dose),by=list(vac_data$VaccinationDate),sum,na.rm=T)

colnames(vac_1dose_yng) = c('Date','Counts')
colnames(vac_2dose_yng) = colnames(vac_1dose_yng) 
colnames(vac_2dose_old) = colnames(vac_1dose_yng) 
colnames(vac_1dose_old) = colnames(vac_1dose_yng) 
colnames(vac_1dose) = colnames(vac_1dose_yng) 
colnames(vac_2dose) = colnames(vac_1dose_yng) 

cohort_size = 9200000

getExpectedCounts = function(vac_dose,counts_data,date1,date2,days_shift,days_boundary,beta=1,cohort_size,create_plot=NULL) {
  date1 = as.Date(date1)
  date2 = as.Date(date2)
  
  A = vac_dose$Date >= date1 & vac_dose$Date<=date2-days_shift
  m = matrix(0,as.numeric(date2-date1-days_shift)+1,as.numeric(date2-date1-days_shift)+1)
  for (i in 1:(as.numeric(date2-date1-days_shift)+1)) {
    m[min(i,as.numeric(date2-date1-days_shift)+1):min(ncol(m),(i+days_boundary-1)),i] = 1
  }
  vac_with_cert = colSums(vac_dose$Count[A]*t(m))
  B = counts_data$date >= date1+days_shift & counts_data$date<=date2
  positive_risk = counts_data$amount[B]/cohort_size
  
  if (!is.null(create_plot)) {
    colnames(m) = as.character(as.Date(seq(date1+days_shift,date2,by=1),origin='1970-01-01'))
    rownames(m) = colnames(m)
    pdf(paste0('~/Documents/covid_analyses/',create_plot,'.pdf'), height = 8, width = 8)
    superheat(t(m)[nrow(m):1,],scale=F,pretty.order.rows = F,pretty.order.cols=F,
              yt=colSums(vac_dose$Count[A]*t(m)),yr=positive_risk*100,
              yt.axis.name='Vaccinated',yr.axis.name='Incidence',
              yt.plot.type = 'scatterline',yr.plot.type = 'scatterline',
              left.label.text.size=2,bottom.label.text.size=2,bottom.label.size=0.21,
              left.label.size=0.21,heat.pal = c("white", "#542788"),
              bottom.label.text.angle=90,legend = FALSE)
    dev.off()
  }
  
  
  #vac_with_cert = cumsum(vac_dose$Count[A])
  
  expected_counts = sum(vac_with_cert*positive_risk*beta)
  expected_counts
}

runAnalysisForBeta = function(observed,vac1,vac2,counts_data,cohort_size=9200000) {
  d = data.frame(beta=seq(0.01,12,by=0.01))
  se = d
  
  for (i in 1:1200) {
    d$Dose1.0[i] = 1-observed[1]/getExpectedCounts(vac1,counts_data,'2020-12-20','2021-02-05',0,14,i/100,cohort_size=cohort_size)
    d$Dose1.14[i] = 1-observed[2]/getExpectedCounts(vac1,counts_data,'2020-12-20','2021-02-05',14,7,i/100,cohort_size=cohort_size)
    d$Dose2.0[i] = 1-observed[3]/getExpectedCounts(vac2,counts_data,'2021-01-10','2021-02-05',0,7,i/100,cohort_size=cohort_size)
    d$Dose2.7[i] = 1-observed[4]/getExpectedCounts(vac2,counts_data,'2021-01-10','2021-02-05',7,20,i/100,cohort_size=cohort_size)
  }
  se$Dose1.0.SE = sqrt(d$Dose1.0*(1-d$Dose1.0)/observed[1])
  se$Dose1.14.SE = sqrt(d$Dose1.14*(1-d$Dose1.14)/observed[2])
  se$Dose2.0.SE = sqrt(d$Dose2.0*(1-d$Dose2.0)/observed[3])
  se$Dose2.7.SE = sqrt(d$Dose2.7*(1-d$Dose2.7)/observed[4])
  list(d,se)
}

getExpectedCounts(vac_1dose_old,rossman_cases,'2020-12-20','2021-02-05',0,14,1,cohort_size = cohort_size,create_plot ='group1')
getExpectedCounts(vac_1dose_old,rossman_cases,'2020-12-20','2021-02-05',14,7,1,cohort_size=cohort_size,create_plot ='group2')
getExpectedCounts(vac_2dose_old,rossman_cases,'2021-01-10','2021-02-05',0,7,1,cohort_size=cohort_size,create_plot ='group3')
getExpectedCounts(vac_2dose_old,rossman_cases,'2021-01-10','2021-02-05',7,20,1,cohort_size=cohort_size,create_plot ='group4')

res = runAnalysisForBeta(moh$Cases60plus,vac_1dose_old,vac_2dose_old,rossman_cases)
df = melt(res[[1]], id.vars ='beta')
colnames(df)[3] = c('Cases60plus')
x = melt(res[[2]], id.vars ='beta')
df$Cases60plus.SE = x$value

res = runAnalysisForBeta(moh$Cases60min,vac_1dose_yng,vac_2dose_yng,rossman_cases)
x = melt(res[[1]], id.vars ='beta')
df$Cases60minus = x$value
x = melt(res[[2]], id.vars ='beta')
df$Cases60minus.SE = x$value

res = runAnalysisForBeta(moh$Hosp60plus+moh$Hosp60min,vac_1dose_old,vac_2dose_old,rossman_hosp)
x = melt(res[[1]], id.vars ='beta')
df$Hosp = x$value
x = melt(res[[2]], id.vars ='beta')
df$Hosp.SE = x$value

res = runAnalysisForBeta(moh$Severe60plus+moh$Severe60min,vac_1dose_old,vac_2dose_old,rossman_severe)
x = melt(res[[1]], id.vars ='beta')
df$Severe = x$value
x = melt(res[[2]], id.vars ='beta')
df$Severe.SE = x$value



df$variable = mapvalues(df$variable,from=c('Dose1.0', 'Dose1.14', 'Dose2.0', 'Dose2.7'), 
                        to=c('1st dose 0-13','1st dose 14-20','2nd dose 0-6','2nd dose 7+'))
colnames(df)[2] = 'Group'

temp = df[df$Group=='1st dose 0-13',]
beta.low =  temp[min(which(temp$Cases60plus>=0)),'beta']
beta.hi =  temp[min(which(temp$Cases60plus>=0.25)),'beta']
p1 = ggplot(df,aes(x=beta,y=Cases60plus,color=Group))+
  geom_ribbon(aes(ymin=Cases60plus-Cases60plus.SE, ymax=Cases60plus+Cases60plus.SE,fill=Group),alpha=0.3, colour = NA)+
  geom_line()+
  theme_classic()+
  scale_color_brewer(palette='Set1')+scale_fill_brewer(palette='Set1')+
  geom_vline(xintercept = 1,linetype='dashed')+
  geom_vline(xintercept = beta.low,linetype='dashed')+
  geom_vline(xintercept = beta.hi,linetype='dashed')+
  geom_label_repel(data=subset(df,beta==1),aes(label=paste0(round(100*Cases60plus),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
  geom_label_repel(data=subset(df,beta==beta.low),aes(label=paste0(round(100*Cases60plus),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
  geom_label_repel(data=subset(df,beta==beta.hi),aes(label=paste0(round(100*Cases60plus),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
  ggtitle('Positive cases in >60yo')+ylab('1-Observed/Expected')+ylim(c(-0.1,1))+
  scale_x_continuous(limits = c(beta.low*0.9,1.1),breaks=c(beta.low,1,beta.hi,1))+
  xlab(expression(beta))#

temp = df[df$Group=='1st dose 0-13',]
beta.low =  temp[min(which(temp$Cases60minus>=0)),'beta']
beta.hi =  temp[min(which(temp$Cases60minus>=0.25)),'beta']
p2 = ggplot(df,aes(x=beta,y=Cases60minus,color=Group))+
  geom_ribbon(aes(ymin=Cases60minus-Cases60minus.SE, ymax=Cases60minus+Cases60minus.SE,fill=Group),alpha=0.3, colour = NA)+
  geom_line()+
  theme_classic()+
  scale_color_brewer(palette='Set1')+
  geom_vline(xintercept = 1,linetype='dashed')+
  geom_vline(xintercept = beta.low,linetype='dashed')+
  geom_vline(xintercept = beta.hi,linetype='dashed')+
  geom_label_repel(data=subset(df,beta==1),aes(label=paste0(round(100*Cases60minus),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
  geom_label_repel(data=subset(df,beta==beta.low),aes(label=paste0(round(100*Cases60minus),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
  geom_label_repel(data=subset(df,beta==beta.hi),aes(label=paste0(round(100*Cases60minus),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
  scale_x_continuous(limits = c(beta.low*0.9,beta.hi*1.1),breaks=c(beta.low,1,beta.hi))+
  ggtitle('Positive cases in <60yo')+ylab('1-Observed/Expected')+ylim(c(0,1))+xlab(expression(beta))
#+theme(legend.position = "none")

temp = df[df$Group=='1st dose 0-13',]
beta.low =  temp[min(which(temp$Hosp>=0)),'beta']
beta.hi =  temp[min(which(temp$Hosp>=0.25)),'beta']
p3 = ggplot(df,aes(x=beta,y=Hosp,color=Group))+
  geom_ribbon(aes(ymin=Hosp-Hosp.SE, ymax=Hosp+Hosp.SE,fill=Group),alpha=0.3, colour = NA)+
  geom_line()+
  theme_classic()+
  scale_color_brewer(palette='Set1')+
  geom_vline(xintercept = 1,linetype='dashed')+
  geom_vline(xintercept = beta.low,linetype='dashed')+
  geom_vline(xintercept = beta.hi,linetype='dashed')+
  geom_label_repel(data=subset(df,beta==1),aes(label=paste0(round(100*Hosp),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
  geom_label_repel(data=subset(df,beta==beta.low),aes(label=paste0(round(100*Hosp),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
  geom_label_repel(data=subset(df,beta==beta.hi),aes(label=paste0(round(100*Hosp),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
  ggtitle('Hospitalization cases')+ylab('1-Observed/Expected')+ylim(c(-0.1,1))+
  scale_x_continuous(limits = c(1,beta.hi*1.1),breaks=c(1,beta.low,beta.hi))+
  xlab(expression(beta))

temp = df[df$Group=='1st dose 0-13',]
beta.low =  temp[min(which(temp$Severe>=0)),'beta']
beta.hi =  temp[min(which(temp$Severe>=0.25)),'beta']
p4 = ggplot(df,aes(x=beta,y=Severe,color=Group))+
  geom_ribbon(aes(ymin=Severe-Severe.SE, ymax=Severe+Severe.SE,fill=Group),alpha=0.3, colour = NA)+
  geom_line()+
  theme_classic()+
  scale_color_brewer(palette='Set1')+geom_vline(xintercept = 6,linetype='dashed')+
  geom_vline(xintercept = 1,linetype='dashed')+
  geom_vline(xintercept = beta.low,linetype='dashed')+
  geom_vline(xintercept = beta.hi,linetype='dashed')+
  geom_label_repel(data=subset(df,beta==1),aes(label=paste0(round(100*Severe),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
  geom_label_repel(data=subset(df,beta==beta.low),aes(label=paste0(round(100*Severe),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
  geom_label_repel(data=subset(df,beta==beta.hi),aes(label=paste0(round(100*Severe),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
  ggtitle('Severe cases')+ylab('1-Observed/Expected')+ylim(c(-0.1,1))+
  scale_x_continuous(limits = c(1,beta.hi*1.1),breaks=c(1,beta.low,beta.hi))+
  xlab(expression(beta))
#+theme(legend.position = "none")
#scale_x_continuous(limits = c(0.75,1.5),breaks=c(0.75,1,1.25,1.5))

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom",labels=c('A','B','C','D'))

#plot_grid(p1+theme(legend.position = "none"),p2+theme(legend.position = "none"),p3+theme(legend.position = "none"),p4+theme(legend.position = "none"),ncol=2)
ggsave('~/Documents/covid_analyses/figure1.2.5.2021.pdf',width=6.8,height = 5.5)

ggsave('~/Documents/covid_analyses/figure1.2.5.2021.p1.pdf',p1,width=5,height = 3.2)
ggsave('~/Documents/covid_analyses/figure1.2.5.2021.p2.pdf',p2,width=5,height = 3.2)
ggsave('~/Documents/covid_analyses/figure1.2.5.2021.p3.pdf',p3,width=5,height = 3.2)
ggsave('~/Documents/covid_analyses/figure1.2.5.2021.p4.pdf',p4,width=5,height = 3.2)

x = df[df$beta==1,]
ggplot(x,aes(x=Group,y=Cases60plus,fill=Group)) + geom_col(position='dodge', color="black")+theme_classic()+ylab('1-Observed/Expected')+
  geom_text(aes(label=paste0(round(100*Cases60plus),'%')), vjust=1.6, color="white",position=position_dodge(0.9), size=6.5)+
  scale_fill_brewer(palette='Set1')+ggtitle('Estimated reduction in cases (60+)')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave('~/Documents/covid_analyses/bars.beta1.2.5.2021.60plus.pdf',width=5,height = 6)

