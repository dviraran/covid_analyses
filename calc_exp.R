
d = read.csv('vac_with_cert_analysis.csv',header = T,row.names = NULL,stringsAsFactors = F)

A = d$Date >= '17/01/2021' & d$Date<='30/01/2021'
cohort_size = 1320000
positive_risk = d$Positive[A]/cohort_size

B = d$Date >= '10/01/2021' & d$Date<='23/01/2021'
vac_with_cert = cumsum(d$WithCert[B])

sum(vac_with_cert*positive_risk)
