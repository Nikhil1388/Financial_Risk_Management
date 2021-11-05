library(quantmod)
NSE<-getSymbols("^NSEI",src="yahoo",auto.assign=FALSE)
NSE<-NSE$NSEI.Adjusted
NSE<-na.omit(NSE)
names(NSE)<-"Adjusted Close Price"

logret<-diff(log(NSE))[-1]
names(logret)<-"Return"
ret<-exp(logret)-1
names(ret)<-"Return"

round(head(logret,2),6)
#             Return
#2007-09-18 0.011404
#2007-09-19 0.040130

round(head(ret,2),6)
#             Return
#2007-09-18 0.011469
#2007-09-19 0.040946


logret.w<-apply.weekly(logret,sum)
ret.w<-exp(logret.w)-1
logret.m<-apply.weekly(logret,sum)
ret.m<-exp(logret.m)-1
logret.q<-apply.quarterly(logret,sum)
ret.q<-exp(logret.q)-1
logret.y<-apply.yearly(logret,sum)
ret.y<-exp(logret.y)-1

mu<-mean(logret)
sig<-sd(logret)

rvec<-as.vector(logret)

VaR1<-qnorm(0.05,mu,sig)
ES1<-mu-sig*dnorm(qnorm(0.05,0,1),0,1)/0.05

round(VaR1,6)
round(ES1,6)

rvec2<-rnorm(100000,mu,sig)
alpha<-0.05
VaR2<-quantile(rvec2,alpha)
ES2<-mean(rvec2[rvec2<VaR2])

round(VaR2,6)
round(ES2,6)

RNGkind(sample.kind="Rounding")
set.seed(123789)
rvec3<-sample(rvec,100000,replace=TRUE)
VaR3<-quantile(rvec3,alpha)
ES3<-mean(rvec3[rvec3<VaR3])

round(VaR3,6)
round(ES3,6)

library(moments)
skewness(rvec)
kurtosis(rvec)
jarque.test(rvec)

library(MASS)
t.fit<-fitdistr(rvec,"t")

library(metRology)
RNGkind(sample.kind="Rounding")
set.seed(123789)

rvec4<-rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR4<-quantile(rvec4,alpha)
ES4<-mean(rvec4[rvec4<VaR4])

round(VaR4,6)
round(ES4,6)

#Calculating 10 lays horizon VaR and ES using simulation from raw log returns data as well as rescaled t-distribution

rvec5<-rep(0,100000)
for(i in 1:10) {
  rvec5<-rvec5+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
rvec<-as.vector(logret)
VaR5<-quantile(rvec5,alpha)
ES5<-mean(rvec5[rvec5<VaR5])

round(VaR5,6)
#-0.068106
round(ES5,6)
#-0.103947

rvec6<-rep(0,100000)
RNGkind(sample.kind="Rounding")
set.seed(123789)
for( i in 1:10){
  rvec6<-rvec6 + sample(as.vector(logret),100000,replace=TRUE)
}
VaR6<-quantile(rvec6,alpha)
ES6<-mean(rvec6[rvec6<VaR6])

round(VaR6,6)
#-0.072571
round(ES6,6)
#-0.100025

rvec7<-rep(0,100000)
posn<-seq(from=1,to=length(rvec)-9,by=1)
rpos<-sample(posn,100000,replace=TRUE)
for( i in 1:10) {
  rvec7<-rvec7 + rvec[rpos]
  rpos<-rpos+1
}

VaR7<-quantile(rvec7,alpha)
ES7<-mean(rvec7[rvec7<VaR7])

round(VaR7,6)
#-0.065531
round(ES7,6)
#-0.111232

acf(logret)
acf(abs(logret))

library(rugarch)
uspec <-ugarchspec(variance.model = list(model="sGARCH",garchOrder =c(1,1)),
                   mean.model = list(armaOrder=c(0,0),include.mean=TRUE),
                   distribution.model = "std")

garch.fit<-ugarchfit(uspec,data=rvec)

round(garch.fit@fit$coef,6)
#      mu    omega   alpha1    beta1    shape 
#0.000782 0.000002 0.080127 0.910769 7.190696 

acf(garch.fit@fit$z)
acf(abs(garch.fit@fit$z))

RNGkind(sample.kind = "Rounding")
set.seed(123789)
boot.garch = ugarchboot(garch.fit,
                        method ="Partial",
                        sampling="raw",
                        n.ahead=1,
                        n.bootpred=100000,
                        solver="solnp"
)

rvec8<-boot.garch@fseries
VaR8<-quantile(rvec8,alpha)
ES8<-mean(rvec8[rvec8<VaR8])

round(VaR8,6)
#-0.014467

round(ES8,6)
#-0.019764

n2020<-length(logret["2007-09-18/2020-12-31"])
roll.garch<-ugarchroll(spec=uspec,
                       data=logret,
                       n.ahead=1,
                       forecast.length=1,
                       n.start=n2020,
                       refit.every=1,
                       refit.window="recursive",
                       calculate.VaR = TRUE,
                       VaR.alpha=0.05,
                       keep.coef=TRUE)
