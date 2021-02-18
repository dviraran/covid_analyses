library(PropCIs)

getCounts = function(vac_dose,counts_data,date1,date2,days_shift,days_boundary,cohort_size,create_plot=NULL) {
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
              yt=colSums(vac_dose$Count[A]*t(m)),
              yt.axis.name='Vaccinated',yr.axis.name='Incidence',
              yt.plot.type = 'scatterline',yr.plot.type = 'scatterline',
              left.label.text.size=2,bottom.label.text.size=2,bottom.label.size=0.21,
              left.label.size=0.21,heat.pal = c("white", "#542788"),
              bottom.label.text.angle=90,legend = FALSE)
    dev.off()
  }
  
  vac_with_cert
  #vac_with_cert = cumsum(vac_dose$Count[A])
  
}

getExpectedCounts = function(counts,beta) {
  expected_counts = sum(counts[[1]]*counts[[2]]*beta)
  expected_counts
}


solve_quadratic<-function(a,b,c){
  det<-sqrt((b^2-4*a*c))
  numerator<-(-b+det)
  denom<-(2*a)
  pos <- numerator/denom
  numerator<-(-b-det)
  neg <- numerator/denom
  result<-c(pos,neg)
  return(result)
}

calculateEffectiveness = function(observed, counts,beta,cohort_size,vac_dose,counts_data,date1,date2,days_shift,days_boundary,p2=NULL) {
  #Solve[p == 1 - o/((x*b*(s1+s2/(1-p)))/s),p]
  date1 = as.Date(date1)
  date2 = as.Date(date2)
 
  counts = c(rep(0,length(vac_dose$Counts)-length(counts)),counts)

  
  v =  c(vac_dose$Counts)
  #v = v[1:length(counts)]
  
  B = counts_data$date >= date1+days_shift & counts_data$date<=date2
  
  o = observed
  n = cumsum(v)[B]
  x = counts[B]
  h = cohort_size
  b = beta
  
  s = counts_data$amount[B]
  
  if (is.null(p2)) {
    p = optimize(function(p) { (1-p-o/sum(b*s*x/(h+n*(-1+b-b*p))))^2},interval=seq(0,1,by=0.01))
  } else {
    p = optimize(function(p) { (1-p-o/sum(b*s*x/(h+n*(-1+b-b*p2))))^2},interval=seq(0,1,by=0.01))
  }

  p$minimum
}

calculateCI = function(obs,p) {
  p[is.na(p) & p<0] = 0
  ci = matrix(NA,length(p),2)
  for (i in 1:length(p)) {
    ci[i,] = exactci(obs/(1-p[i])-obs,obs/(1-p[i]),0.95)$conf.int
    
  }
  ci 
}

runAnalysisForBeta = function(observed,vac1,vac2,counts_data,cohort_size=9200000,last.date='2021-02-10') {
  d = data.frame(beta=seq(0.01,12,by=0.01))
  se = d
  
  g1 = getCounts(vac1,counts_data,'2020-12-20',last.date,0,14,cohort_size=cohort_size)
  g2 = getCounts(vac1,counts_data,'2020-12-20',last.date,14,7,cohort_size=cohort_size)
  g3 = getCounts(vac2,counts_data,'2021-01-10',last.date,0,7,cohort_size=cohort_size)
  g4 = getCounts(vac2,counts_data,'2021-01-10',last.date,7,7,cohort_size=cohort_size)
  g5 = getCounts(vac2,counts_data,'2021-01-10',last.date,14,16,cohort_size=cohort_size)
  
  for (i in 1:1200) {
    d$Dose2.0[i] = calculateEffectiveness(observed[3],g3,i/100,cohort_size,vac2,counts_data,'2021-01-10',last.date,0,7)
    d$Dose2.7[i] = calculateEffectiveness(observed[4],g4,i/100,cohort_size,vac2,counts_data,'2021-01-10',last.date,7,7)
    d$Dose2.14[i] = calculateEffectiveness(observed[5],g5,i/100,cohort_size,vac2,counts_data,'2021-01-10',last.date,14,16)
    
    d$Dose1.0[i] = calculateEffectiveness(observed[1],g1,i/100,cohort_size,vac2,counts_data,'2020-12-20',last.date,0,14,d$Dose2.7[i]) 
    d$Dose1.14[i] = calculateEffectiveness(observed[2],g2,i/100,cohort_size,vac2,counts_data,'2020-12-20',last.date,14,7,d$Dose2.7[i])

    #   expected= getExpectedCounts(vac2,counts_data,'2021-01-10',last.date,14,16,i/100,cohort_size=cohort_size)
  }
  ci = calculateCI(observed[3],d$Dose2.0)  
  se$Dose2.0.low =ci[,1]
  se$Dose2.0.hi = ci[,2]
  ci = calculateCI(observed[4],d$Dose2.7)  
  se$Dose2.7.low =ci[,1]
  se$Dose2.7.hi = ci[,2]
  ci = calculateCI(observed[5],d$Dose2.14)  
  se$Dose2.14.low = ci[1]
  se$Dose2.14.hi = ci[2]
  ci = calculateCI(observed[1],d$Dose1.0)
  se$Dose1.0.low =ci[,1]
  se$Dose1.0.hi = ci[,2]
  ci = calculateCI(observed[2],d$Dose1.14)  
  se$Dose1.14.low =ci[,1]
  se$Dose1.14.hi = ci[,2]

  list(d,se)
}

createPlot = function(df,field,tit) {
  df$Cases = df[,field]
  df$Cases.low = df[,paste0(field,'.low')]
  df$Cases.hi = df[,paste0(field,'.hi')]
  
  temp = df[df$Group=='1st dose 0-13',]
  beta.low =temp[min(which(temp$Cases>=0.01)),'beta']
  #beta.hi =  temp[min(which(temp$Cases60plus>=0.25)),'beta']
  ggplot(df,aes(x=beta,y=Cases,color=Group))+
    geom_ribbon(aes(ymin=Cases.low, ymax=Cases.hi,fill=Group),alpha=0.3, colour = NA)+
    geom_line()+
    theme_classic()+
    scale_color_brewer(palette='Set1')+
    scale_fill_brewer(palette='Set1')+
    geom_vline(xintercept = beta.low*0.9,linetype='dashed')+
    geom_vline(xintercept = beta.low*1.1,linetype='dashed')+
    geom_label_repel(data=subset(df,abs(beta-round(beta.low*90)/100)<0.009),aes(label=paste0(round(100*Cases),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
    geom_label_repel(data=subset(df,abs(beta-round(beta.low*110)/100)<0.009),aes(label=paste0(round(100*Cases),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
    geom_vline(xintercept = beta.low,linetype='dashed')+
    #geom_vline(xintercept = beta.low,linetype='dashed')+
    geom_label_repel(data=subset(df,beta==beta.low),aes(label=paste0(round(100*Cases),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
    #geom_label_repel(data=subset(df,beta==beta.low),aes(label=paste0(round(100*Cases60plus),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
    ggtitle(tit)+ylab('1-Observed/Expected')+ylim(c(-0.1,1))+
    scale_x_continuous(limits = c(beta.low*0.85,beta.low*1.15),breaks=c(beta.low*0.9,beta.low,beta.low,beta.low*1.1))+
    xlab(expression(beta))
}
