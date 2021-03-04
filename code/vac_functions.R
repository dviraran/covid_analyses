library(PropCIs)

getCounts = function(vac_dose,counts_data,date1,date2,days_shift,days_boundary,cohort_size,create_plot=NULL,tit=NULL) {
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
    dist.up = colSums(vac_dose$Count[A]*t(m))
    m = m*8
    m[(nrow(m)-7):nrow(m),]=m[(nrow(m)-7):nrow(m),]/2
    m[(nrow(m)-3):nrow(m),]=m[(nrow(m)-3):nrow(m),]/2
    
    m[(nrow(m)-1):nrow(m),]=m[(nrow(m)-1):nrow(m),]/2
    
    #png(paste0('~/Documents/covid_analyses/plots/',create_plot,'.png'), height = 1600, width = 1600)
    s = superheat(t(m)[nrow(m):1,],scale=F,pretty.order.rows = F,pretty.order.cols=F,
              yt=dist.up,
              yt.axis.name='Vaccinated',yr.axis.name='Incidence',
              yt.plot.type = 'scatterline',yr.plot.type = 'scatterline',
              left.label.text.size=2,bottom.label.text.size=2,bottom.label.size=0.21,
              left.label.size=0.21,heat.pal = c("white", "#542788"),
              bottom.label.text.angle=90,legend = FALSE,
              smooth.heat = TRUE,grid.hline = F,grid.vline = F,
              title = tit)
   # dev.off()
    s
  } else {
    
    vac_with_cert
  }
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

calculateEffectiveness = function(observed,counts,beta,cohort_size,vac_dose,counts_data,p2=NULL,group) {
  #Solve[p == 1 - o/((x*b*(s1+s2/(1-p)))/s),p]
  
  counts = c(rep(0,length(vac_dose$Counts)-length(counts)),counts)
  
  v =  vac_dose$Counts
  
  o = observed
  n = cumsum(v)
  x = counts
  h = cohort_size
  b = beta
  s = counts_data$amount
  
  if (is.character(group)) {
    if (group=='cases') {
      remove_days = 2
    } else if (group=='hosp') {
      remove_days = 4
    } else if (group=='severe') {
      remove_days = 8
    }
  } else if (is.numeric(group)) {
    remove_days = group
  }
  
  ## remove last days of vaccined.
  x[(length(x)-(remove_days-1)):length(x)] = 0
  
  
  if (is.null(p2)) {
    p = optimize(function(p) { (1-p-o/sum(b*s*x/(h+n*(-1+b-b*p))))^2},interval=seq(-0.5,1,by=0.01))
  } else {
    p = optimize(function(p) { (1-p-o/sum(b*s*x/(h+n*(-1+b-b*p2))))^2},interval=seq(-0.5,1,by=0.01))
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

runAnalysis14days = function(observed,vac1,vac2,counts_data,cohort_size,last.date='2021-02-22',beta=1,p2=0.81,dash=2) {
  d=c()
  g5 = getCounts(vac2,counts_data,'2021-01-10',last.date,14,30,cohort_size=cohort_size)
  for (j in 1:26) {
    d[j] = calculateEffectiveness(observed[5],g5,beta,cohort_size,vac2,counts_data,p2,j-1)
  }
  df = data.frame(Shift=0:25,VE=d)
  print(df)
  df
  #ggplot(df,aes(Shift,VE))+geom_line()+geom_point()+theme_classic()+ylim(0,1)+geom_vline(xintercept=dash,linetype='dashed')
}

runAnalysisForBeta = function(observed,vac1,vac2,counts_data,cohort_size,group,last.date='2021-02-22') {
  d = data.frame(beta=seq(0.01,2,by=0.01))
  se = d
  
  g1 = getCounts(vac1,counts_data,'2020-12-20',last.date,0,14,cohort_size=cohort_size)
  g2 = getCounts(vac1,counts_data,'2020-12-20',last.date,14,7,cohort_size=cohort_size)
  g3 = getCounts(vac2,counts_data,'2021-01-10',last.date,0,7,cohort_size=cohort_size)
  g4 = getCounts(vac2,counts_data,'2021-01-10',last.date,7,7,cohort_size=cohort_size)
  g5 = getCounts(vac2,counts_data,'2021-01-10',last.date,14,30,cohort_size=cohort_size)
  
  for (i in 1:200) {
    d$Dose2.0[i] = calculateEffectiveness(observed[3],g3,i/100,cohort_size,vac2,counts_data,NULL,group)
    d$Dose2.7[i] = calculateEffectiveness(observed[4],g4,i/100,cohort_size,vac2,counts_data,d$Dose2.0[i],group)
    d$Dose2.14[i] = calculateEffectiveness(observed[5],g5,i/100,cohort_size,vac2,counts_data,d$Dose2.0[i],group)
    
    d$Dose1.0[i] = calculateEffectiveness(observed[1],g1,i/100,cohort_size,vac2,counts_data,d$Dose2.0[i],group) 
    d$Dose1.14[i] = calculateEffectiveness(observed[2],g2,i/100,cohort_size,vac2,counts_data,d$Dose2.0[i],group)
    
    #   expected= getExpectedCounts(vac2,counts_data,'2021-01-10',last.date,14,16,i/100,cohort_size=cohort_size)
  }
  ci = calculateCI(observed[3],d$Dose2.0)  
  se$Dose2.0.low =ci[,1]
  se$Dose2.0.hi = ci[,2]
  ci = calculateCI(observed[4],d$Dose2.7)  
  se$Dose2.7.low =ci[,1]
  se$Dose2.7.hi = ci[,2]
  
  ci = calculateCI(observed[5],d$Dose2.14)  
  se$Dose2.14.low = ci[,1]
  se$Dose2.14.hi = ci[,2]
  ci = calculateCI(observed[1],d$Dose1.0)
  se$Dose1.0.low =ci[,1]
  se$Dose1.0.hi = ci[,2]
  ci = calculateCI(observed[2],d$Dose1.14)  
  se$Dose1.14.low =ci[,1]
  se$Dose1.14.hi = ci[,2]
  
  list(d,se)
}

createPlotBars = function(df) {
  #x = melt(df,id.vars = c('beta','Group'))
  min.vals = apply(df[,seq(3,ncol(df),by=3)],2,FUN=function(x) min(which(x>0.01 & df$Group=='1st dose 0-13')))
  x = matrix(NA,length(min.vals),5)
  xlow = matrix(NA,length(min.vals),5)
  xhi = matrix(NA,length(min.vals),5)
  for (i in 1:length(min.vals)) {
    beta = df[min.vals[i],'beta']
    x[i,] = df[df$beta==beta,colnames(df)==names(min.vals)[i]]
    xlow[i,] = df[df$beta==beta,which(colnames(df)==names(min.vals)[i])+1]
    xhi[i,] = df[df$beta==beta,which(colnames(df)==names(min.vals)[i])+2]
  }
  
  rownames(x) = names(min.vals)
  colnames(x) = unique(df$Group)
  x = x[,c(4:5,1:3)]
  xlow = xlow[,c(4:5,1:3)]
  xhi = xhi[,c(4:5,1:3)]
  x = x[c(1,3,5,2,4,6),]
  xlow = xlow[c(1,3,5,2,4,6),]
  xhi = xhi[c(1,3,5,2,4,6),]
  x = melt(x)
  xlow = melt(xlow)
  xhi = melt(xhi)
  
  x$Years = '60+'
  x$Years[grepl('minus',x$Var1)] = '60-'
  x$Group = 'Positive cases'
  x$Group[grepl('Hosp',x$Var1)] = 'Hospitalizations'
  x$Group[grepl('Severe',x$Var1)] = 'Severe'
  x$Years = factor(x$Years,levels=c('60+','60-'))
  x$Group = factor(x$Group,levels=c('Positive cases','Hospitalizations','Severe'))
  
  ggplot(x,aes(y=value,x=Var2,fill=Var2))+
    geom_bar(stat="identity", color="black", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=xlow$value, ymax=xhi$value), width=.2,
                  position=position_dodge(.9)) +
    theme_classic()+facet_grid(Years~Group)+ylab('Vaccine Effectiveness')+xlab('')+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    geom_text(aes(label=paste0(round(100*value),'%')), vjust=1.6, color="white",position=position_dodge(0.9), size=2)+
    scale_fill_brewer(palette='Set1')+theme(legend.position = "none")+
    theme(strip.text.x = element_text(face="bold"),
          strip.text.y = element_text(face="bold"),
          strip.background = element_rect(fill='gray'))
  
  
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
    geom_vline(xintercept = beta.low*0.8,linetype='dashed')+
    geom_vline(xintercept = beta.low*1.2,linetype='dashed')+
    geom_label_repel(data=subset(df,abs(beta-round(beta.low*80)/100)<0.009),aes(label=paste0(round(100*Cases),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
    geom_label_repel(data=subset(df,abs(beta-round(beta.low*120)/100)<0.009),aes(label=paste0(round(100*Cases),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
    geom_vline(xintercept = beta.low,linetype='dashed')+
    #geom_vline(xintercept = beta.low,linetype='dashed')+
    geom_label_repel(data=subset(df,beta==beta.low),aes(label=paste0(round(100*Cases),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
    #geom_label_repel(data=subset(df,beta==beta.low),aes(label=paste0(round(100*Cases60plus),'%')),nudge_x=0.04, nudge_y=0.04, size=2,show.legend=F)+
    ggtitle(tit)+ylab('1-Observed/Expected')+ylim(c(-0.1,1))+
    scale_x_continuous(limits = c(beta.low*0.75,beta.low*1.25),breaks=c(beta.low*0.8,beta.low,beta.low,beta.low*1.2))+
    xlab(expression(beta))
}


createMatrix = function(date1,date2,p = c(0,40,60,83,94),H=1428000,cases,vac,beta) {
  date1 = as.Date('2020-12-20')
  date2 = as.Date(date2)
  N = as.numeric(date2-date1)+1
  
  m = matrix(NA,N,N)
  for (i in 1:N) {
    m[min(i,N):min(ncol(m),(i+14-1)),i] = p[1]
  }
  
  for (i in 1:(N-14)) {
    m[min(i+14,N):min(N,i+14+7-1),i] = p[2]
  }
  for (i in 1:(N-21)) {
    m[min(i+21,N):min(N,i+21+7-1),i] = p[3]
  }
  for (i in 1:(N-28)) {
    m[min(i+28,N):min(N,i+28+7-1),i] = p[4]
  }
  for (i in 1:(N-35)) {
    m[min(i+35,N):N,i] = p[5]
  }
  
  beta=1
  
  S = cases[1:N,'amount']
  V = vac$Counts
  
  M = m
  for (i in 1:N) {
    M[i,] = V[i]*(1-m[,i]/100)
  }
  
  d = S/(colSums(M,na.rm=T)*beta+(H-cumsum(V)))
  
  df = data.frame(Date=as.Date(seq(date1+3,date2-3,by=1)),NoVaccine=zoo::rollmean(d*H,k=7),WithVaccine=zoo::rollmean(S,k=7))
  df = melt(df,id.vars = 'Date')
  colnames(df) = c('Date','Scenario','Cases')
  ggplot(df,aes(Date,Cases,color=Scenario))+geom_line(size=2)+
    theme_classic()+ylab('Severe cases (60+)')+ylim(c(0,260))
  # 
  # colnames(m) = as.character(as.Date(seq(date1,date2,by=1),origin='1970-01-01'))
  # rownames(m) = colnames(m)
  # superheat(t(m)[nrow(m):1,],scale=F,pretty.order.rows = F,pretty.order.cols=F,
  #           left.label.text.size=2,bottom.label.text.size=2,bottom.label.size=0.21,
  #           left.label.size=0.21,heat.pal = c("white", "#542788"),
  #           bottom.label.text.angle=90,legend = FALSE) 
  
  
  
}
