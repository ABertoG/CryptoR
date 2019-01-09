################################################################################################
############################### SIMULATION DU PRIX DU BITCOIN ##################################
################################################################################################

library(combinat)
library(fitdistrplus)
library(logspline)
library(ggplot2)
library(PerformanceAnalytics)
library(xts)
library(QRM)

##############################################################
######################## IMPORTATION #########################
##############################################################

setwd("/Users/Alexandra/Documents/Crypto/Model/Data")

daily<-read.csv("BTCUSD Day Coindesk.csv",header=TRUE,sep=',')

print(daily)
tail(daily)

D<-data.frame(daily)
D$Date<-as.Date(D$Date,format = "%d/%m/%y")
summary(D)

D$LLow=log(D$Low)
summary(D$Low)
summary(D$LLow)

D$Return<-NA
D$LReturn<-NA

head(D)

for(i in 1:(nrow(D)-1)){
	D$Return[i+1]=(D$Low[i+1]-D$Low[i])/D$Low[i]
	D$LReturn[i+1]=(D$LLow[i+1]-D$LLow[i])/D$LLow[i]
}

D<-D[-1,]
head(D)

summary(D)


plot(D$Date,D$Return,type="l")
plot(D$Date,D$LReturn,type="l")


##########################################################
######################## ANALYSE #########################
##########################################################


Returns<-ts(D$Return,frequency=365.25,start=c(2010,200))
plot(Returns)

summary(D$LReturn)
LReturns<-ts(D$LReturn,frequency=365.25,start=c(2010,200))
plot(LReturns)

LReturns2010<-window(LReturns,start=c(2010,200),end=c(2010,365.25))
LReturns2011<-window(LReturns,start=c(2011,1),end=c(2011,365.25))
LReturns2012<-window(LReturns,start=c(2012,1),end=c(2012,365.25))
LReturns2013<-window(LReturns,start=c(2013,1),end=c(2013,365.25))
LReturns2014<-window(LReturns,start=c(2014,1),end=c(2014,365.25))
LReturns2015<-window(LReturns,start=c(2015,1),end=c(2015,365.25))
LReturns2016<-window(LReturns,start=c(2016,1),end=c(2016,365.25))
LReturns2017<-window(LReturns,start=c(2017,1),end=c(2017,365.25))
LReturns2018<-window(LReturns,start=c(2018,1))

par(mfrow=c(3, 3))

hist(LReturns2010,ylim=c(0,150),col="gray", border="white")
d <-density(LReturns2010)
lines(d, col="red")

hist(LReturns2011,col="gray", border="white")
d <-density(LReturns2011)
lines(d, col="red")

hist(LReturns2012,col="gray", border="white")
d <-density(LReturns2012)
lines(d, col="red")

hist(LReturns2013,col="gray", border="white")
d <-density(LReturns2013)
lines(d, col="red")

hist(LReturns2014,col="gray", border="white")
d <-density(LReturns2014)
lines(d, col="red")

hist(LReturns2015,col="gray", border="white")
d <-density(LReturns2015)
lines(d, col="red")

hist(LReturns2016,col="gray", border="white")
d <-density(LReturns2016)
lines(d, col="red")

hist(LReturns2017,col="gray", border="white")
d <-density(LReturns2017)
lines(d, col="red")

hist(LReturns2018,col="gray", border="white")
d <-density(LReturns2018)
lines(d, col="red")

#2012 is very different from other returns => lets take the returns from 2013

LReturnsClean<-window(LReturns,start=c(2013,1))
summary(LReturnsClean)

SLReturnsClean<-window(LReturns,start=c(2014,1))

BTLReturnsClean<-window(LReturns,start=c(2013,1),end=c(2016,365))
BTSLReturnsClean<-window(LReturns,start=c(2014,1),end=c(2016,365))

plot(BTSLReturnsClean)

plotdist(as.vector(LReturnsClean),histo = TRUE, demp = TRUE)
descdist(as.vector(LReturnsClean)) #Nothing 



fNLR <- fitdist(as.vector(LReturnsClean), "norm")    ## note: it is "norm" not "normal"
#class(fNLR)
summary(fNLR)
plot(fNLR)  #Not normal but lets save the parameters
muNLR<-fNLR$estimate[1]
sigmaNLR<-fNLR$estimate[2]

fSNLR <- fitdist(as.vector(SLReturnsClean), "norm")    ## note: it is "norm" not "normal"
plot(fSNLR)  #Not normal but lets save the parameters
muSNLR<-fSNLR$estimate[1]
sigmaSNLR<-fSNLR$estimate[2]

fBTNLR <- fitdist(as.vector(BTLReturnsClean), "norm")    ## note: it is "norm" not "normal"
#class(fNLR)
summary(fBTNLR)
plot(fBTNLR)  #Not normal but lets save the parameters
muBTNLR<-fBTNLR$estimate[1]
sigmaBTNLR<-fBTNLR$estimate[2]

fBTSNLR <- fitdist(as.vector(BTSLReturnsClean), "norm")    ## note: it is "norm" not "normal"
#class(fNLR)
summary(fBTSNLR)
plot(fBTSNLR)  #Not normal but lets save the parameters
muBTSNLR<-fBTSNLR$estimate[1]
sigmaBTSNLR<-fBTSNLR$estimate[2]

#Lets check how stable the normal fit along the years
#fNLR2013 <- fitdist(as.vector(LReturns2013), "norm")    ## note: it is "norm" not "normal"
#class(fNLR)
#summary(fNLR2013)

#fNLR2014 <- fitdist(as.vector(LReturns2014), "norm")    ## note: it is "norm" not "normal"
#class(fNLR)
#summary(fNLR2014)

#fNLR2015 <- fitdist(as.vector(LReturns2015), "norm")    ## note: it is "norm" not "normal"
#class(fNLR)
#summary(fNLR2015)

#fNLR2016 <- fitdist(as.vector(LReturns2016), "norm")    ## note: it is "norm" not "normal"
#class(fNLR)
#summary(fNLR2016)

#fNLR2017 <- fitdist(as.vector(LReturns2017), "norm")    ## note: it is "norm" not "normal"
#class(fNLR)
#summary(fNLR2017)

#center
#LReturnsCC<-LReturnsClean-median(LReturnsClean)
#summary(LReturnsCC)
#plotdist(as.vector(LReturnsCC),histo = TRUE, demp = TRUE)
#descdist(as.vector(LReturnsCC)) #Nothing 
#fNLRC <- fitdist(as.vector(LReturnsCC), "norm")    ## note: it is "norm" not "normal"
#plot(fNLRC)
#summary(fNLRC)

#Other functions

#fNLR <- fitdist(as.vector(LReturnsClean), "logis") 
#plot(fNLR)

#fNLR <- fitdist(as.vector(LReturnsClean), "cauchy") 
#plot(fNLR)

#Fit Student
#fit.st(LReturnsClean)

#No match :( Lets also try resampling? 



#################################################################
############# SIMULATION WITH LOG RETURN SAMPLING ##############
#################################################################

Sampling<-function(Last,Returns,S,L){

LBTC<-matrix(0,nrow=S,ncol=L)
BTC<-matrix(0,nrow=S,ncol=L)
Last<-log(Last)
LBTCs[,1]<-Last
#LBTC2[,1]<-Last
#LBTC5[,1]<-Last
#BTC[,1]=exp(LBTC[1,1])

for(j in 1:S){

#Sampled Ln Returns
LR<-sample(as.vector(Returns),L)
#LR2<-LR/2
#LR5<-LR/5

for(i in 1:(L-1)){

LBTC[j,i+1]<-LBTC[j,i]+LBTC[j,i]*LR[i]
#LBTC2[j,i+1]<-LBTC2[j,i]+LBTC2[j,i]*LR2[i]
#LBTC5[j,i+1]<-LBTC5[j,i]+LBTC5[j,i]*LR5[i]

}

if (j == S) {
	
		BTC=exp(LBTC)
	
		par(mfrow=c(1,2))
	
		matplot(t(BTC),type="l",main="BTC Projection",xlab="Days",ylab="USD",ylim=c(0,quantile(BTC[,L],0.75)))
		#matplot(t(BTC),type="l",ylim=c(0,1000000))
		
		quant<-function(x){quantile(x,c(0.05,0.25,0.5,0.75,0.95))}
		
		QBTC<-apply(BTC,2,quant)
		matplot(t(QBTCs),type="l",main="BTC Projection Quantiles",,xlab="Days",ylab="USD")
		}
}}

summary(BTC)

S=100000 #Number of scenarios
L=365 #Projection lenght in import seasonality (day)
Last<-tail(D$LLow,n=1)

BTCsampling13<-Sampling(Last,LReturnsClean,S,L)
BTCs13<-BTC
BTCs13q<-QBTC

BTCsampling2014<-Sampling(Last,SLReturnsClean,S,L)
BTCs14<-BTC
BTCs14q<-QBTC

BTCsamplingBT2013<-Sampling(Last,BTLReturnsClean,S,L)
BTCsBT13<-BTC
BTCsBT13q<-QBTC

BTCsamplingBT2014<-Sampling(Last,BTSLReturnsClean,S,L)
BTCsBT14<-BTC
BTCsBT14q<-QBTC

##############################################################
################ SIMULATION WITH NORMAL DIST #################
##############################################################

#S=100 #Number of scenarios
#L=365 #Projection lenght in import seasonality (day)


#Normal Log Return



summary(as.vector(LRn))

LRn<-matrix(0,nrow=S,ncol=L)
LBTCn<-matrix(0,nrow=S,ncol=L)
LBTCn[,1]<-Last

for(j in 1:S){

#j=1	
LRn[j,]<-rnorm(N,mean=muNLR,sd=sigmaNLR)	
	
for(i in 1:(L-1)){
	
#i=2
LBTCn[j,i+1]<-LBTCn[j,i]*(1+LRn[j,i])

#head(LRn)
#head(LBTCn)
}
}

#head(LBTCs)

summary(LBTCn)
#summary(LBTCs2)
matplot(t(LRn),type="l")

BTCn=exp(LBTCn)
summary(BTCn)

QBTCn<-apply(BTCn,2,quant)

summary(QBTCn[,ncol(QBTCn)])
summary(QBTCs[,ncol(QBTCs)])

matplot(t(QBTCn),type="l",ylim=c(0,100000),col=c("yellow","orange","red","orange","yellow"))

plot(density(QBTCn), type="l")


#VaR mais:
#Dans la réalité risque de baisse à 0 !!!! Voir risque par rapport aux autres cryptos : proba de faillite

##############################################################
###################### BACKTESTING ###########################
##############################################################




#### TS

library(TTR)

ComReturns<-decompose(Returns)
plot(ComReturns$seasonal)

HWReturns<-HoltWinters(Returns,beta=FALSE, gamma=FALSE,l.start=Returns[1])

plot(HWReturns)

library(forecast)

FHWReturns<-forecast(HWReturns,h=365)
plot(FHWReturns)
