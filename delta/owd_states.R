library(ggplot2)
library(ggrepel)
library(ggpubr)
library(lubridate)

d = read.csv('https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv')

date1 = '2021-04-01'
date2 = '2021-08-01'
date3 = '2021-08-31'

vac1 = d$people_fully_vaccinated_per_hundred[d$date==date1]
vac2 = d$people_fully_vaccinated_per_hundred[d$date==date2]

death1 = d$total_deaths_per_million[d$date==date1]
death2 = d$total_deaths_per_million[d$date==date2]
death3 = d$total_deaths_per_million[d$date==date3]

cases.total = d$total_cases_per_million[d$date==date2]

#vac[is.na(vac)] = 0
names(vac1) = d$location[d$date==date1]
names(vac2) = d$location[d$date==date2]

names(cases.total) = d$location[d$date==date2]

names(death1) = d$location[d$date==date1]
names(death2) = d$location[d$date==date2]
names(death3) = d$location[d$date==date3]

A = intersect(names(death3),intersect(names(death2),intersect(names(death1),intersect(names(vac1),names(vac2)))))
vac = vac2[A]-vac1[A]
cases.diff = (death3[A]-death2[A])-(death2[A]-death1[A])/4
cases.total = cases.total[A]
vac.march = vac1[A]
vac.total = vac2[A]

august = year(d$date)==2021 & month(d$date)==8 & d$population>1000000
#cases = aggregate(d$new_cases_per_million[august],by=list(d$location[august]),sum,na.rm=T)


cases = aggregate(d$new_deaths_per_million[august],by=list(d$location[august]),sum,na.rm=T)
rownames(cases) = cases[,1]


#cases = d$new_cases_smoothed_per_million[d$date==date3]


A = intersect(names(vac),rownames(cases))

#A =intersect(A,d$location[d$date==date2 & !is.na(d$people_fully_vaccinated_per_hundred) 
#                            & d$people_fully_vaccinated_per_hundred>0])
vac = vac[A]
cases = cases[A,2]
vac.march = vac.march[A]
vac.total = vac.total[A]
cases.diff = cases.diff[A]
cases.total = cases.total[A]

df = data.frame(Cases=cases,VacRate=vac,Country=A,VaxedMarch=vac.march,VaxedTotal=vac.total,ProtectedTotal = vac.total+cases.total/1000)
df = df[!(df$Country %in% c('Africa','Asia','Europe','European Union','International','World','North America','South America','Palestine','Oceania')),]

ggplot(df,aes(VacRate,Cases,label=Country))+
  geom_point(aes(fill=VaxedMarch,size=VaxedMarch),alpha=0.5,pch=21)+ geom_text_repel()+theme_classic()+
  geom_smooth(method = "lm",se=F)+ stat_cor(method = "pearson", label.x = 40, label.y = 150)+
  xlab('% of population vaccinated Apr-Jul 2021')+ylab('Deaths per 1 million on Aug 2021')+
  ylim(c(0,170))+ scale_fill_continuous(limits=c(0, 60), breaks=seq(0, 60, by=10))+
  guides(fill= guide_legend(title='% vaccinated\nuntil March'), 
         size=guide_legend(title='% vaccinated\nuntil March'))+ 
  scale_size_continuous(limits=c(0, 60), breaks=seq(0, 60, by=10))

ggsave('~/Documents/covid/fig.countries.png',width=7,height=6)

ggplot(subset(df,VaxedTotal>33),aes(VaxedMarch,Cases,label=Country))+
  geom_point(aes(fill=VaxedTotal,size=VaxedTotal),alpha=0.5,pch=21)+ geom_text_repel()+theme_classic()+
  geom_smooth(method = "lm",se=F)+ stat_cor(method = "pearson", label.x = 40, label.y = 0)+
  xlab('% of population vaccinated untul March 2021')+ylab('Deaths per 1M on Aug 2021')+ylim(c(0,80))


