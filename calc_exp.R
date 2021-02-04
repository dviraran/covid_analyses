library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(cowplot)
library(ggrepel)

### GET DATA

# https://data.gov.il/dataset/covid-19/resource/57410611-936c-49a6-ac3c-838171055b1f/download/vaccinated-per-day-2021-02-02.csv
vac_data = read.csv('~/Documents/covid_analyses/vaccinated-per-day-2021-02-02.csv',header = T,row.names = NULL,stringsAsFactors = F)
#  https://raw.githubusercontent.com/erasta/CovidDataIsrael/master/out/csv/infectedPerDate.csv
infected_data = read.csv('~/Documents/covid_analyses/infectedPerDate.csv',header = T,row.names = NULL,stringsAsFactors = F)
infected_data$date = as.Date(infected_data$date)

#  https://raw.githubusercontent.com/erasta/CovidDataIsrael/master/out/csv/patientsPerDate.csv
patients_data = read.csv('~/Downloads/patientsPerDate.csv',header = T,row.names = NULL,stringsAsFactors = F)
patients_data$date = as.Date(patients_data$date)
patients_data$amount = patients_data$serious_critical_new

moh = read.csv('~/Documents/covid_analyses/moh_data.csv',header = T,row.names = 1,stringsAsFactors = F)

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

getExpectedCounts = function(vac_dose,counts_data,date1,date2,days_shift,days_boundary,beta=0.5,cohort_size) {
  date1 = as.Date(date1)
  date2 = as.Date(date2)
  
  A = vac_dose$Date >= date1-days_shift & vac_dose$Date<=date2-days_shift
  m = matrix(0,as.numeric(date2-date1)+1,as.numeric(date2-date1)+1)
  for (i in 1:(as.numeric(date2-date1)+1)) {
    m[min(i,as.numeric(date2-date1)+1):min(ncol(m),(i+days_boundary-1)),i] = 1
  }
  vac_with_cert = colSums(vac_dose$Count[A]*t(m))
  #vac_with_cert = cumsum(vac_dose$Count[A])
  
  B = counts_data$date >= date1 & counts_data$date<=date2
  
  positive_risk = counts_data$amount[B]/cohort_size
  
  expected_counts = sum(vac_with_cert*positive_risk*beta)
  expected_counts
}

runAnalysisForBeta = function(counts_use,vac1,vac2,counts_data,cohort_size=9200000) {
  d = data.frame(beta=seq(0.26,2,by=0.01))
  se = d
  
  for (i in 1:175) {
    d$Dose1.0[i] = 1-counts_use[1]/getExpectedCounts(vac1,counts_data,'2020-12-20','2021-01-31',0,14,i/100+0.25,cohort_size=cohort_size)
    d$Dose1.14[i] = 1-counts_use[2]/getExpectedCounts(vac1,counts_data,'2021-01-03','2021-01-31',14,7,i/100+0.25,cohort_size=cohort_size)
    d$Dose2.0[i] = 1-counts_use[3]/getExpectedCounts(vac2,counts_data,'2021-01-10','2021-01-31',0,7,i/100+0.25,cohort_size=cohort_size)
    d$Dose2.7[i] = 1-counts_use[4]/getExpectedCounts(vac2,counts_data,'2021-01-17','2021-01-31',7,15,i/100+0.25,cohort_size=cohort_size)
  }
  se$Dose1.0.SE = sqrt(d$Dose1.0*(1-d$Dose1.0)/counts_use[1])
  se$Dose1.14.SE = sqrt(d$Dose1.14*(1-d$Dose1.14)/counts_use[2])
  se$Dose2.0.SE = sqrt(d$Dose2.0*(1-d$Dose2.0)/counts_use[3])
  se$Dose2.7.SE = sqrt(d$Dose2.7*(1-d$Dose2.7)/counts_use[4])
  list(d,se)
}

res = runAnalysisForBeta(moh$Cases60plus,vac_1dose_old,vac_2dose_old,infected_data)
df = melt(res[[1]], id.vars ='beta')
colnames(df)[3] = c('Cases60plus')
x = melt(res[[2]], id.vars ='beta')
df$Cases60plus.SE = x$value

res = runAnalysisForBeta(moh$Cases60min,vac_1dose_yng,vac_2dose_yng,infected_data)
x = melt(res[[1]], id.vars ='beta')
df$Cases60minus = x$value
x = melt(res[[2]], id.vars ='beta')
df$Cases60minus.SE = x$value

res = runAnalysisForBeta(moh$Severe60plus+moh$Severe60min,vac_1dose,vac_2dose,patients_data,cohort_size/3)
x = melt(res[[1]], id.vars ='beta')
df$Severe = x$value
x = melt(res[[2]], id.vars ='beta')
df$Severe.SE = x$value

p1 = ggplot(df,aes(x=beta,y=Cases60plus,color=variable))+
  geom_ribbon(aes(ymin=Cases60plus-Cases60plus.SE, ymax=Cases60plus+Cases60plus.SE,fill=variable),alpha=0.3, colour = NA)+
  geom_line()+
  theme_classic()+
  scale_color_brewer(palette='Set1')+geom_vline(xintercept = 1,linetype='dashed')+
  geom_vline(xintercept = 0.75,linetype='dashed')+geom_vline(xintercept = 0.5,linetype='dashed')+
  geom_label_repel(data=subset(df,beta==1),aes(label=paste0(round(100*Cases60plus),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
  geom_label_repel(data=subset(df,beta==0.75),aes(label=paste0(round(100*Cases60plus),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
  geom_label_repel(data=subset(df,beta==0.5),aes(label=paste0(round(100*Cases60plus),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
  ggtitle('Positive cases in >60yo')+theme(legend.position = "none")+ylab('1-Observed/Expected')+ylim(c(-0.1,1))+xlim(c(0.4,1.2))

p2 = ggplot(df,aes(x=beta,y=Cases60minus,color=variable))+
  geom_ribbon(aes(ymin=Cases60minus-Cases60minus.SE, ymax=Cases60minus+Cases60minus.SE,fill=variable),alpha=0.3, colour = NA)+
  geom_line()+
  theme_classic()+
  scale_color_brewer(palette='Set1')+geom_vline(xintercept = 1.25,linetype='dashed')+
  geom_vline(xintercept = 1,linetype='dashed')+geom_vline(xintercept = 0.75,linetype='dashed')+
  geom_label_repel(data=subset(df,beta==1),aes(label=paste0(round(100*Cases60minus),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
  geom_label_repel(data=subset(df,beta==1.25),aes(label=paste0(round(100*Cases60minus),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
  geom_label_repel(data=subset(df,beta==0.75),aes(label=paste0(round(100*Cases60minus),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
  scale_x_continuous(limits = c(0.75,1.5),breaks=c(0.75,1,1.25,1.5))+
  ggtitle('Positive cases in <60yo')+theme(legend.position = "none")+ylab('')+ylim(c(0,1))

p3 = ggplot(df,aes(x=beta,y=Severe,color=variable))+
  geom_ribbon(aes(ymin=Severe-Severe.SE, ymax=Severe+Severe.SE,fill=variable),alpha=0.3, colour = NA)+
  geom_line()+
  theme_classic()+
  scale_color_brewer(palette='Set1')+geom_vline(xintercept = 2,linetype='dashed')+
  geom_vline(xintercept = 1,linetype='dashed')+geom_vline(xintercept = 1.5,linetype='dashed')+
  geom_label_repel(data=subset(df,beta==1),aes(label=paste0(round(100*Severe),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
  geom_label_repel(data=subset(df,beta==1.5),aes(label=paste0(round(100*Severe),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
  geom_label_repel(data=subset(df,beta==2),aes(label=paste0(round(100*Severe),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
  ggtitle('Severe cases')+theme(legend.position = "none")+ylab('')+ylim(c(-0.1,1))+xlim(c(1,2))
#scale_x_continuous(limits = c(0.75,1.5),breaks=c(0.75,1,1.25,1.5))


plot_grid(p1,p2,p3,ncol=3)
ggsave('~/Documents/covid_analyses/figure1.pdf',width=9,height = 3.2)






if (FALSE) {
  p1 = ggplot(df,aes(x=beta,y=Cases60plus,color=variable))+
    geom_ribbon(aes(ymin=Cases60plus-Cases60plus.SE, ymax=Cases60plus+Cases60plus.SE,fill=variable),alpha=0.3, colour = NA)+
    geom_line()+
    theme_classic()+
    scale_color_brewer(palette='Set1')+geom_vline(xintercept = 1,linetype='dashed')+
    geom_vline(xintercept = 0.75,linetype='dashed')+geom_vline(xintercept = 0.5,linetype='dashed')+
    geom_label_repel(data=subset(df,beta==1),aes(label=paste0(round(100*Cases60plus),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
    geom_label_repel(data=subset(df,beta==0.75),aes(label=paste0(round(100*Cases60plus),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
    geom_label_repel(data=subset(df,beta==0.5),aes(label=paste0(round(100*Cases60plus),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
    ggtitle('Positive cases in >60yo')+theme(legend.position = "none")+ylab('1-Observed/Expected')+ylim(c(-0.1,1))+xlim(c(0.4,1.2))
  
  p2 = ggplot(df,aes(x=beta,y=Cases60minus,color=variable))+
    geom_ribbon(aes(ymin=Cases60minus-Cases60minus.SE, ymax=Cases60minus+Cases60minus.SE,fill=variable),alpha=0.3, colour = NA)+
    geom_line()+
    theme_classic()+
    scale_color_brewer(palette='Set1')+geom_vline(xintercept = 1.25,linetype='dashed')+
    geom_vline(xintercept = 1,linetype='dashed')+geom_vline(xintercept = 0.75,linetype='dashed')+
    geom_label_repel(data=subset(df,beta==1),aes(label=paste0(round(100*Cases60minus),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
    geom_label_repel(data=subset(df,beta==1.25),aes(label=paste0(round(100*Cases60minus),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
    geom_label_repel(data=subset(df,beta==0.75),aes(label=paste0(round(100*Cases60minus),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
    scale_x_continuous(limits = c(0.75,1.5),breaks=c(0.75,1,1.25,1.5))+
    ggtitle('Positive cases in <60yo')+theme(legend.position = "none")+ylab('')+ylim(c(0,1))
  
  p3 = ggplot(df,aes(x=beta,y=Severe,color=variable))+
    geom_ribbon(aes(ymin=Severe-Severe.SE, ymax=Severe+Severe.SE,fill=variable),alpha=0.3, colour = NA)+
    geom_line()+
    theme_classic()+
    scale_color_brewer(palette='Set1')+geom_vline(xintercept = 2,linetype='dashed')+
    geom_vline(xintercept = 1,linetype='dashed')+geom_vline(xintercept = 1.5,linetype='dashed')+
    geom_label_repel(data=subset(df,beta==1),aes(label=paste0(round(100*Severe),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
    geom_label_repel(data=subset(df,beta==1.5),aes(label=paste0(round(100*Severe),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
    geom_label_repel(data=subset(df,beta==2),aes(label=paste0(round(100*Severe),'%')),nudge_x=0.04, nudge_y=0.04, size=2)+
    ggtitle('Severe cases')+theme(legend.position = "none")+ylab('')+ylim(c(-0.1,1))+xlim(c(1,2))
  #scale_x_continuous(limits = c(0.75,1.5),breaks=c(0.75,1,1.25,1.5))
  
  
  save(p1,p2,p3,file='~/Documents/VaccineEffectIsrael/data/plot.rda')
}
