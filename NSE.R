library(quantmod)

#Extracting the NIFTY50 Index data from "Yahoo Finance"
NSE<-getSymbols("^NSEI",src="yahoo",auto.assign=FALSE)

#Keeping only the Adjusted CLose Price Column for Returns Calculation
NSE<-NSE$NSEI.Adjusted

NSE<-na.omit(NSE)
names(NSE)<-"Adjusted Close Price"

#Calculating Log Daily Returns
logret<-diff(log(NSE))[-1]
names(logret)<-"Return"

#Calculating Discrete Daily Returns
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

#we can use apply.weekly, apply.monthly, apply.quarterly, apply.yearly functions to calculate log weekly,monthly, quarterly and yearly returns respectively.
logret.w<-apply.weekly(logret,sum)
round(head(logret.w,2),6)
#             Return
#2007-09-21 0.073521
#2007-09-28 0.037290

ret.w<-exp(logret.w)-1
round(head(ret.w,2),6)
#            Return
#2007-09-21 0.076291
#2007-09-28 0.037995

logret.m<-apply.weekly(logret,sum)
ret.m<-exp(logret.m)-1
logret.q<-apply.quarterly(logret,sum)
ret.q<-exp(logret.q)-1
logret.y<-apply.yearly(logret,sum)
ret.y<-exp(logret.y)-1

mu<-mean(logret)
sig<-sd(logret)

rvec<-as.vector(logret)

#Estimating VaR and ES at 95% Confidence Level using Normal Distribution
VaR1<-qnorm(0.05,mu,sig)
ES1<-mu-sig*dnorm(qnorm(0.05,0,1),0,1)/0.05

round(VaR1,6)
# -0.022843
round(ES1,6)
# -0.028748

#Using rnorm function to generate a random sample for Simulation from Normal distribution with the above calculated mean and Standard Deviation
rvec2<-rnorm(100000,mu,sig)
alpha<-0.05
VaR2<-quantile(rvec2,alpha)
ES2<-mean(rvec2[rvec2<VaR2])

round(VaR2,6)
# -0.022792 
round(ES2,6)
# -0.028619

#Using raw log returns data for Simulation
RNGkind(sample.kind="Rounding")
set.seed(123789)
rvec3<-sample(rvec,100000,replace=TRUE)
VaR3<-quantile(rvec3,alpha)
ES3<-mean(rvec3[rvec3<VaR3])

round(VaR3,6)
# -0.020503
round(ES3,6)
# -0.033971

library(moments)

#Calculating the skewness and kurtosis from moments library of the log return data to check whether the Normal distribution is a perfect fit for the data or not.
skewness(rvec)
# -0.2504776
kurtosis(rvec)
# 16.90522
jarque.test(rvec)
#Jarque-Bera Normality Test

#data:  rvec
#JB = 27855, p-value < 2.2e-16
#alternative hypothesis: greater

#Since the skewness is negative which means the data is left skewed, as well as the kurtosis is also very big whereas the skewness for normal distribution is 0 whereas the 
#coefficient of kurtosis is 3 and the JB value is also quite big, which shows that the Normal Distribution is not a perfect fit for the data.

library(MASS)
# We know that standard normal distribution is a specific case of Student-T Distribution when the degree of freedoms becomes infinite. So, instead of using the Normal 
# Distribution, using rescaled Student-T Distribution which can manage leptokurtosis because of adjustable degrees of freedom parameter.

t.fit<-fitdistr(rvec,"t")
#Making Student-T Distribution as per our data.
library(metRology)
RNGkind(sample.kind="Rounding")
set.seed(123789)

#Using the estimated paramters from the T-Distribution for simulation
rvec4<-rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR4<-quantile(rvec4,alpha)
ES4<-mean(rvec4[rvec4<VaR4])

round(VaR4,6)
# -0.019558 
round(ES4,6)
# -0.034111

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

#Taking continuous 10-days data as a block for 10-day VaR Calculation
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

#Using the acf function to look for auto-correlation coefficient at different lags in the Time-Series data
acf(logret)

#Coming to the concept of Volatility Clustering,
#Volatility bursts are a type of absolute autocorrelation.
#So, it makes sense to look for auto correlation in absolute log returns data.
acf(abs(logret))

#Here, we can observe that the correlation coefficients are significant which implies that there can be significant amount of Volatility Clustering in our data. Consequently,
# to deal with such circumstances, GARCH models are used in practice in Risk Management which account for "Volatility Clustering".

library(rugarch)
#Fitting the Specifications we want in our GARCH model in uspec such as distribution model as Student-T Distribution,etc. Basically, we are using GARCH-(1,1)-t Model here.
uspec <-ugarchspec(variance.model = list(model="sGARCH",garchOrder =c(1,1)),
                   mean.model = list(armaOrder=c(0,0),include.mean=TRUE),
                   distribution.model = "std")

garch.fit<-ugarchfit(uspec,data=rvec)

round(garch.fit@fit$coef,6)
#      mu    omega   alpha1    beta1    shape 
#0.000782 0.000002 0.080127 0.910769 7.190696 

#Looking for whether GARCH Model has checked for Volatility Clustering.
acf(garch.fit@fit$z)
acf(abs(garch.fit@fit$z))

#Using ugarchboot function for Simulation of 100000 outcomes to calculate for VaR and ES.
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

#Calculating Discrete returns from VaR
round((exp(VaR8)-1),6)
#-0.014363 

# Let us assume an investor invests Rs.10,00,000 in a NIFTY50 Index Fund, so according to historical GARCH predicted VaR, the maximum amount that the investor can lose  
# on the next trading session within 95% Confidence Level = 10,00,000 * -0.014363 = 14363/-Rs.

#Preparing VaR data from 2021-01-01 upto 2021-11-04 using Ugarchroll function:-
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


data_1<-roll.garch@forecast$VaR
res<-row.names.data.frame(roll.garch@forecast$VaR)
data_1<-cbind.data.frame(data_1,res)

head(data_1,3)
#             alpha(5%)     realized        res
#2021-01-04 -0.01538109  0.010752533 2021-01-04
#2021-01-05 -0.01542801  0.004701312 2021-01-05
#2021-01-06 -0.01493887 -0.003757181 2021-01-06
# The alpha(5%) column tells us what was the predicted VaR according to GARCH model and the realized column tells about the actual log returns observed on that day.


#Plotting VaR at 95% Confidence Level and Observed Log Returns Data from "2021-01-01" to "2021-11-04". 
ggplot(data_1)  + 
  geom_bar(aes(x=res, y=data_1[,2]),stat="identity", fill="cyan",colour="#006000")+
  geom_line(aes(x=res, y=data_1[,1]),stat="identity",color="red", lwd = 2)+
  labs(title= "VaR at 95% Confidence Level on investing in Nifty 50 Index Fund",
       x="Months",y="Log Returns")

length(data_1$res)
#  208
sum(data_1[,1]>data_1[,2])
#  9

# So, we can observe that out of 208 trading days from "2021-01-01" to "2021-11-04", there are only 9 trading days(4.32%) at which observed losses have breached the losses 
# as predicted by the GARCH model at 95% Confidence Level. So, GARCH model is really a good estimator of VaR and ES which takes Volatility Clustering into account and provides
# much accurate results in comparison to Normal Distributions.
