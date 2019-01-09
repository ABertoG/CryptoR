
library(jsonlite)
library(anytime)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(qwraps2)
options(qwraps2_markup = "markdown")



###############
#ETHSCAN API

APIKEY=#Put your ETHScan API key


###############
#Aidrop

Token<-"TRX"

###############
#Lets import the data relating to a contract or transaction the airdrop
X="0xdc3a100ea6ef430b0db2d97f075b458a5be9edd0" #Address from which the tokens to be aidropped where first distributed
API=paste("http://api.etherscan.io/api?module=account&action=tokentx&address=",X,sep="")

Raw <- fromJSON(API)
Raw<-Raw$result
Clean<-Raw


###############
#The data needs to be formatted for the analysis

Clean$blockNumber<-as.numeric(Clean$blockNumber)

Clean$timeStamp<-as.numeric(Clean$timeStamp)
Date<-assertDate(Clean$timeStamp)
Date<-as.Date(Date)
Clean$timeStamp<-Date

Clean$nonce<-as.numeric(Clean$nonce)
Clean$value<-as.numeric(Clean$value)
Clean$transactionIndex<-as.numeric(Clean$transactionIndex)
Clean$gas<-as.numeric(Clean$gas)
Clean$gasPrice<-as.numeric(Clean$gasPrice)
Clean$gasUsed<-as.numeric(Clean$gasUsed)
Clean$cumulativeGasUsed<-as.numeric(Clean$cumulativeGasUsed)
Clean$confirmations<-as.numeric(Clean$confirmations)

Clean$quantity<-(Clean$value / 10^6)

summary(Clean)
Date<-"2018-04-28" #Date of Aidrop prep
PreAirdrop<- subset(Clean,timeStamp == Date )

summary(PreAirdrop)

Name<-paste(Token,"PreAirdrop.csv",sep="")
File<-paste(Directory,Name,sep="")
write.csv(PreAirdrop,file=File)

###############
#Analysis

#How many tokens where sent?
TotalSent=sum(PreAirdrop$quantity) #Same as announced

#How many unique adresses
SourceAddresses<-unique(PreAirdrop$to)


###############
#Lets import the data relating to the 100 source addresses of the airdrop
options(timeout= 99999999999999) 
AllData=data.frame()
start=c(1)
end=c(1)

for (i in 82:length(SourceAddresses)){

  start[i]=Sys.time()
  print(i)
  
  X=SourceAddresses[i]
  
  API=paste("http://api.etherscan.io/api?module=account&action=tokentx&address=",X,"&apikey=",APIKEY,sep="")

  Raw <- fromJSON(API)
  Raw<-Raw$result
  Clean<-Raw

  ###############
  #The data needs to be formatted for the analysis
  
  Clean$blockNumber<-as.numeric(Clean$blockNumber)
  Clean$timeStamp<-as.numeric(Clean$timeStamp)
  Date<-assertDate(Clean$timeStamp)
  Date<-as.Date(Date)
  Clean$timeStamp<-Date
  Clean$nonce<-as.numeric(Clean$nonce)
  Clean$value<-as.numeric(Clean$value)
  Clean$transactionIndex<-as.numeric(Clean$transactionIndex)
  Clean$gas<-as.numeric(Clean$gas)
  Clean$gasPrice<-as.numeric(Clean$gasPrice)
  Clean$gasUsed<-as.numeric(Clean$gasUsed)
  Clean$cumulativeGasUsed<-as.numeric(Clean$cumulativeGasUsed)
  Clean$confirmations<-as.numeric(Clean$confirmations)
  Clean$quantity<-(Clean$value / 10^6)
  summary(Clean)

  AllData<-rbind(AllData,Clean)
  
  end[i]=Sys.time()
  
  print((end[i]-start[i])/60)
  }

TotalTime=(end[100]-start[1])/60

summary(AllData)

Name<-"TRXAirdrop2.csv"

File<-paste(Directory,Name,sep="")
write.csv(AllData,file=File)


###############
#Analysis

#Lets take a subset at the date of the Airdrop
Dates<-c("2018-04-28","2018-04-29","2018-04-30","2018-05-01","2018-05-02","2018-05-03","2018-05-04","2018-05-05","2018-05-06","2018-05-07","2018-05-08") #Date of Aidrop prep
#Date<-"2018-05-01" #Date of Aidrop
  
#Airdrop <- subset(AllData,subset = timeStamp %in% Dates )
Airdrop <- AllData[ AllData$timeStamp >= "2018-04-28",]
Airdrop <- Airdrop[ Airdrop$timeStamp <= "2018-05-07",]
summary(Airdrop)

A<-hist(Airdrop$timeStamp,"days")

#How many tokens where sent?
TotalSent=sum(Airdrop$quantity)
#sum(Clean$quantity)

#How many tokens per recipients
PerRecipient=mean(Airdrop$quantity)
MinPerRecipient=min(Airdrop$quantity)
MaxPerRecipient=max(Airdrop$quantity)

#How many unique adresses
UniqueRecipient=length(unique(Airdrop$to))


#SourceAddresses<-unique(Airdrop$from)
#ContractAddresses<-unique(Airdrop$contractAddress)

#Airdrop <- read.csv(file="~/Documents/Work/Dr Garrick Hileman/Airdrops/Chain Analysis/ETH/TRXAirdrop.csv", header=TRUE) 
#summary(Airdrop)
#Lets get only the columns we need
Airdrop2<-data.frame(Airdrop$timeStamp,Airdrop$from,Airdrop$to,Airdrop$contractAddress,Airdrop$quantity,Airdrop$blockNumber)

head(Airdrop2)
summary(Airdrop2)

#Hmisc::describe(Airdrop2)
#Hmisc::describeBy(Airdrop2,Airdrop2$Airdrop.contractAddress)

#library(psych)
#psych::describeBy(Airdrop2,Airdrop2$Airdrop.contractAddress)

#library(skimr)
#skim(Airdrop2)

#library(compareGroups)
#compareGroups(contractAddress ~ ., data = Airdrop2)

library(arsenal)
Table<-tableby()


Airdrop3 <- Airdrop2[ Airdrop$from!= "0xdc3a100ea6ef430b0db2d97f075b458a5be9edd0",]
summary(Airdrop3)
dim(Airdrop3)

TotalSent=sum(Airdrop3$Airdrop.quantity)
UniqueRecipient=length(unique(Airdrop3$Airdrop.to))
length(unique(Airdrop3$Airdrop.contractAddress))

count(Airdrop3,Airdrop.contractAddress)



Airdrop4 <- Airdrop3[Airdrop3$Airdrop.contractAddress=="0xf230b790e05390fc8295f4d3f60332c93bed42e2",]
summary(Airdrop4)
dim(Airdrop4)
sum(Airdrop4$Airdrop.quantity)
length(unique(Airdrop4$Airdrop.to))

Name<-"TRXAirdropLight.csv"

File<-paste(Directory,Name,sep="")
write.csv(Airdrop4,file=File)
