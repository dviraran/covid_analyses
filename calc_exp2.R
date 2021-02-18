library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(cowplot)
library(ggrepel)
library(plyr)
library(superheat)
library(ggpubr)

### GET DATA

# https://data.gov.il/dataset/covid-19/resource/57410611-936c-49a6-ac3c-838171055b1f/download/vaccinated-per-day-2021-02-09.csv
vac_data = read.csv('~/Documents/covid_analyses/vaccinated-per-day-2021-02-16.csv',header = T,row.names = NULL,stringsAsFactors = F)
#vac_data[470,] = c('2021-02-10','20-29',0,0)
#vac_data[469,] = c('2021-02-10','80-89',0,0)

death = read.csv('~/Documents/covid_analyses/deadPatientsPerDate.csv',header = T,row.names = NULL,stringsAsFactors = F)
death$date = as.Date(death$date)

moh = read.csv('~/Documents/covid_analyses/moh_data.2.10.2021.csv',header = T,row.names = 1,stringsAsFactors = F)

### 

rossman = read.csv('~/Documents/covid_analyses/Fig2_national_3rd_lockdown_y_vectors_daily.csv',header = T,row.names = NULL,stringsAsFactors = F)
rossman$date = as.Date(rossman$X)
rossman = rossman[rossman$date>='2020-12-20',]

rossman_cases_60plus = rossman
rossman_cases_60plus$amount = (rossman[,'y_pos_3_national_60.'])
rossman_cases_60minus = rossman
rossman_cases_60minus$amount = (rossman[,'y_pos_3_national_0.59'])

rossman_hosp_60plus = rossman
rossman_hosp_60plus$amount = (rossman[,'y_hosped_3_national_60.'])
rossman_hosp_60minus = rossman
rossman_hosp_60minus$amount = (rossman[,'y_hosped_3_national_0.59'])

rossman_severe_60plus = rossman
rossman_severe_60plus$amount = (rossman[,'y_sevhosped_3_national_60.'])
rossman_severe_60min = rossman
rossman_severe_60min$amount = (+rossman[,'y_sevhosped_3_national_0.59'])

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

last.date='2021-02-10'

getCounts(vac_1dose_old,rossman_cases,'2020-12-20',last.date,0,14,cohort_size = cohort_size,create_plot ='group1')
getCounts(vac_1dose_old,rossman_cases,'2020-12-20',last.date,14,7,cohort_size=cohort_size,create_plot ='group2')
getCounts(vac_2dose_old,rossman_cases,'2021-01-10',last.date,0,7,cohort_size=cohort_size,create_plot ='group3')
getCounts(vac_2dose_old,rossman_cases,'2021-01-10',last.date,7,7,cohort_size=cohort_size,create_plot ='group4')
getCounts(vac_2dose_old,rossman_cases,'2021-01-10',last.date,14,18,cohort_size=cohort_size,create_plot ='group5')

res = runAnalysisForBeta(moh$Cases60plus,vac_1dose_old,vac_2dose_old,rossman_cases_60plus,cohort_size=1428000)
df = melt(res[[1]], id.vars ='beta')
colnames(df)[3] = c('Cases60plus')
x = melt(res[[2]], id.vars ='beta')
df$Cases60plus.low = x$value[grepl('low',x$variable)]
df$Cases60plus.hi = x$value[grepl('hi',x$variable)]

res = runAnalysisForBeta(moh$Cases60min,vac_1dose_yng,vac_2dose_yng,rossman_cases_60minus,cohort_size=7539000)
x = melt(res[[1]], id.vars ='beta')
df$Cases60minus = x$value
x = melt(res[[2]], id.vars ='beta')
df$Cases60minus.low = x$value[grepl('low',x$variable)]
df$Cases60minus.hi = x$value[grepl('hi',x$variable)]

res = runAnalysisForBeta(moh$Hosp60plus,vac_1dose_old,vac_2dose_old,rossman_hosp_60plus,cohort_size=1428000)
x = melt(res[[1]], id.vars ='beta')
df$Hosp60plus = x$value
x = melt(res[[2]], id.vars ='beta')
df$Hosp60plus.low = x$value[grepl('low',x$variable)]
df$Hosp60plus.hi = x$value[grepl('hi',x$variable)]

res = runAnalysisForBeta(moh$Hosp60min,vac_1dose_yng,vac_2dose_yng,rossman_hosp_60minus,cohort_size=7539000)
x = melt(res[[1]], id.vars ='beta')
df$Hosp60minus = x$value
x = melt(res[[2]], id.vars ='beta')
df$Hosp60minus.low = x$value[grepl('low',x$variable)]
df$Hosp60minus.hi = x$value[grepl('hi',x$variable)]

res = runAnalysisForBeta(moh$Severe60plus,vac_1dose_old,vac_2dose_old,rossman_severe_60plus,cohort_size=1428000)
x = melt(res[[1]], id.vars ='beta')
df$Severe60plus = x$value
x = melt(res[[2]], id.vars ='beta')
df$Severe60plus.low = x$value[grepl('low',x$variable)]
df$Severe60plus.hi = x$value[grepl('hi',x$variable)]


res = runAnalysisForBeta(moh$Severe60min,vac_1dose_yng,vac_2dose_yng,rossman_severe_60min,cohort_size=7539000)
x = melt(res[[1]], id.vars ='beta')
df$Severe60minus = x$value
x = melt(res[[2]], id.vars ='beta')
df$Severe60minus.low = x$value[grepl('low',x$variable)]
df$Severe60minus.hi = x$value[grepl('hi',x$variable)]
# 
# res = runAnalysisForBeta(moh$Deceased,vac_1dose_old,vac_2dose_old,death)
# x = melt(res[[1]], id.vars ='beta')
# df$Deceased = x$value
# x = melt(res[[2]], id.vars ='beta')
# df$Severe.low = x$value[grepl('low',x$variable)]
# df$Severe.hi = x$value[grepl('hi',x$variable)]

df$variable = mapvalues(df$variable,from=c('Dose2.0', 'Dose2.7', 'Dose2.14', 'Dose1.0', 'Dose1.14'), 
                        to=c('2nd dose 0-6','2nd dose 7-13','2nd dose 14+','1st dose 0-13','1st dose 14-20'))
colnames(df)[2] = 'Group'

df$Group <- factor(df$Group,
                levels = c('1st dose 0-13','1st dose 14-20','2nd dose 0-6','2nd dose 7-13','2nd dose 14+'))

p1 = createPlot(df,'Cases60plus','Positive cases >60')
p2 = createPlot(df,'Cases60minus','Positive cases <60')
p3 = createPlot(df,'Hosp60plus','Hospitalization cases >60')
p4 = createPlot(df,'Hosp60minus','Hospitalization cases <60')
p5 = createPlot(df,'Severe60plus','Severe cases >60')
p6 = createPlot(df,'Severe60minus','Severe cases <60')

#ggarrange(p1, p2, p3, p4,p5,p6, ncol=2, nrow=3, common.legend = TRUE, legend="bottom",labels=c('A','B','C','D'))

ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom",labels=c('A','B'))
ggsave('~/Documents/covid_analyses/figure1.2.10.2021.cases.pdf',width=8,height = 4)
ggsave('~/Documents/covid_analyses/figure1.2.10.2021.cases.jpg',width=8,height = 4)

ggarrange(p3, p4, ncol=2, nrow=1, common.legend = TRUE, legend="bottom",labels=c('A','B'))
ggsave('~/Documents/covid_analyses/figure1.2.10.2021.hosp.pdf',width=8,height = 4)
ggsave('~/Documents/covid_analyses/figure1.2.10.2021.hosp.jpg',width=8,height = 4)

ggarrange(p5, p6, ncol=2, nrow=1, common.legend = TRUE, legend="bottom",labels=c('A','B'))
ggsave('~/Documents/covid_analyses/figure1.2.10.2021.severe.pdf',width=8,height = 4)
ggsave('~/Documents/covid_analyses/figure1.2.10.2021.severe.jpg',width=8,height = 4)


#plot_grid(p1+theme(legend.position = "none"),p2+theme(legend.position = "none"),p3+theme(legend.position = "none"),p4+theme(legend.position = "none"),ncol=2)
ggsave('~/Documents/covid_analyses/figure1.2.10.2021.pdf',width=7,height = 9)

ggsave('~/Documents/covid_analyses/figure1.2.10.2021.p1.pdf',p1,width=5,height = 3.2)
ggsave('~/Documents/covid_analyses/figure1.2.10.2021.p2.pdf',p2,width=5,height = 3.2)
ggsave('~/Documents/covid_analyses/figure1.2.10.2021.p3.pdf',p3,width=5,height = 3.2)
ggsave('~/Documents/covid_analyses/figure1.2.10.2021.p4.pdf',p4,width=5,height = 3.2)
ggsave('~/Documents/covid_analyses/figure1.2.10.2021.p5.pdf',p5,width=5,height = 3.2)

x = df[df$beta==1,]
ggplot(x,aes(x=Group,y=Cases60plus,fill=Group)) + geom_col(position='dodge', color="black")+theme_classic()+ylab('1-Observed/Expected')+
  geom_text(aes(label=paste0(round(100*Cases60plus),'%')), vjust=1.6, color="white",position=position_dodge(0.9), size=6.5)+
  scale_fill_brewer(palette='Set1')+ggtitle('Estimated reduction in cases (60+)')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave('~/Documents/covid_analyses/bars.beta1.2.5.2021.60plus.pdf',width=5,height = 6)

