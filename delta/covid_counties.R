library(ggplot2)
library(ggrepel)

## https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh
vac = read.csv('COVID-19_Vaccinations_in_the_United_States_County.csv')

## https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv
cases = read.csv('us-counties.csv')

## https://static.usafacts.org/public/data/covid-19/covid_county_population_usafacts.csv
pop = read.csv('covid_county_population_usafacts.csv')

vac$Date = as.Date(vac$Date,format = '%m/%d/%Y')
date1 = '2021-03-31'
date2 = '2021-07-31'
date3 = '2021-08-31'
v1 = data.frame(fips=vac$FIPS[vac$Date==date1],vac.march=vac$Series_Complete_Pop_Pct[vac$Date==date1])
v2 = data.frame(fips=vac$FIPS[vac$Date==date2],vac.july=vac$Series_Complete_Pop_Pct[vac$Date==date2])
d1 = data.frame(fips=cases$fips[cases$date==date3],death.aug=cases$deaths[cases$date==date3])
d2 = data.frame(fips=cases$fips[cases$date==date2],death.july=cases$deaths[cases$date==date2])
v = merge(v1,v2,by = 'fips')
d = merge(d1,d2,by = 'fips')

v$vac.diff = v$vac.july-v$vac.march
d$death.diff= d$death.aug-d$death.july
m = merge(v,d,by = 'fips')
m = merge(m,pop,by.x = 'fips','by.y' = 'countyFIPS')
m = m[m$death.diff>0 & m$vac.diff>0 & !is.na(m$death.diff) & !is.na(m$vac.diff) & m$population>100000,]
m$death.diff = 1000000*m$death.diff/m$population

m$label = paste0(m$County.Name,', ',m$State)
ggplot(m,aes(vac.diff,death.diff,label=label))+
  geom_point()+theme_classic()+geom_text_repel(data=dplyr::filter(m,vac.diff>50 | death.diff>300),max.overlaps = 10,size=3)+
  geom_smooth(method = "lm",se=F)+ stat_cor(method = "pearson", label.x = 40, label.y = 600)+
  xlab('% of population vaccinated Apr-Jul 2021')+ylab('Deaths per 1 million on Aug 2021')
ggsave('~/Documents/covid_analyses/fig2.counties.png',width=7,height = 6)


