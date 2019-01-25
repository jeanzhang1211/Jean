BioC_mirror: https://bioconductor.org
Using Bioconductor 3.7 (BiocInstaller 1.30.0), R 3.5.1 (2018-07-02).
Installing package(s) ‘fitdistrplus’
library(readr)
GOOG <- read_csv("Downloads/GOOGL.csv")
View(GOOG)
install.packages("fitdistrplus")
pt1=GOOG[,6]
pt1=as.matrix(pt1)
pt1=pt1[,1]
pt=tail(pt1,250)
pt1=head(pt1,250)
rt=log(pt/pt1)
quantile(rt,0.05)
q005=quantile(rt,0.05)
r=rt[rt<q005]
Er=mean(r)
plotdist(rt)
descdist(rt)
fitdist(rt,"logis")
fitdist(rt,"norm")
install.packages("goftest")
library(goftest)
ad.test(rt,"pnorm")
ad.test(rt,"plogis",fit$estimate[1],fit$estimate[2])
m=fit$estimate[1]
s=fit$estimate[2]
f=function(x){x*dlogis(x,m,s)}
c=qlogis(0.05,m,s)
E=integrate(f,-Inf,c)$value
Fr=plogis(c,m,s)
Erc=E/Fr
Erc



