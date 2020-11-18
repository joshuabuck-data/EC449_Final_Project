###########################################################################
#Project: EC 449 Final Project
#Description: Reading in US Exchange Rates Data from Oct 2018 - Oct 2020. 
#             Creating Graphs to compare 2018-2019 o0 2019-2020.
#Date: November 2020
#Author: Joshua Buck
###########################################################################
library(reshape)
library(dplyr)
library(ggplot2)
library(grDevices)
library(readxl)
setwd("~/Documents/School/7th Semester/EC 449/Final")

  ###### Reading in Data [(Header in Data); (missing data = ND)]######
US18_20 <- read.csv("FRB_H10-3.csv", header= T, sep = ",",dec = ".", stringsAsFactors = F, na.strings = "ND")
USX <- as.data.frame(matrix(nrow = 523))
FFR <- read.csv("DFF.csv",stringsAsFactors = F, col.names = c("Date","FFRate")) #Federal Funds Rate Data#

  ####### Shortening Names of variables and reformatting variables so they all express US$/Foregin$ ######
USX$Date <-as.Date(US18_20$Time.Period,"%m/%d/%y")
USX$Euro <- US18_20$SPOT.EXCHANGE.RATE...EURO.AREA
USX$NewZealand <- US18_20$NEW.ZEALAND....SPOT.EXCHANGE.RATE..US..NZ..RECIPROCAL.OF.RXI_N.B.NZ.
USX$UK <- US18_20$UNITED.KINGDOM....SPOT.EXCHANGE.RATE..US..POUND..1.RXI_N.B.UK.
USX$Sweden <- 1/(US18_20$SWEDEN....SPOT.EXCHANGE.RATE..KRONOR.US..)
USX$Korea <- 1/(US18_20$KOREA....SPOT.EXCHANGE.RATE..WON.US..)
USX$Japan <- 1/(US18_20$JAPAN....SPOT.EXCHANGE.RATE..YEN.US..)
USX$China <- 1/(US18_20$CHINA....SPOT.EXCHANGE.RATE..YUAN.US..P.R..)

FFR$Date <- as.Date(FFR$Date)

USX<- USX %>% select(Date:China)%>% na.omit
USX <- left_join(USX,FFR, by ="Date")


###### Transposing Data ######
USXM <- melt(USX, id = "Date")
USXM <- USXM %>% rename(Country=variable, ExchangeRate=value)

###### Creating Percent Change Variable ######
#USXM <- USXM %>% mutate(PercentChange=NA)
#w<-0
#z<-0
#for (i in 1:(dim(USXM)[1])){
#  if(as.character(USXM[i,2]) == "FFRate"){
#    USXM$PercentChange[i] <- (USXM[i,3])
#  } else if(identical(as.character(USXM[i,2]),z) == F){
#      USXM$PercentChange[i] <- (0)
#  } else{
#      w <- USXM[(i-1),3]
#      USXM$PercentChange[i] <- (USXM[i,3] - w)/w
#    } 
#  z<- as.character(USXM[i,2])
#}

USXM <- USXM %>% mutate(PercentChange2=NA)
w<-0
for (i in 1:(dim(USXM)[1])){
  if(as.character(USXM[i,2])=="Euro"){
      w<-0.8824568
      USXM$PercentChange2[i] <- ((USXM[i,3] - w)/w)
  } else if(as.character(USXM[i,2])=="NewZealand"){
      w<-1.531394
      USXM$PercentChange2[i] <- ((USXM[i,3] - w)/w)
  } else if(as.character(USXM[i,2])=="UK"){
      w<-0.7825338
      USXM$PercentChange2[i] <- ((USXM[i,3] - w)/w)
  } else if(as.character(USXM[i,2])=="Sweden"){
      w<-0.1093350
      USXM$PercentChange2[i] <- ((USXM[i,3] - w)/w)
  } else if(as.character(USXM[i,2])=="Korea"){
      w<-0.0008765932
      USXM$PercentChange2[i] <- ((USXM[i,3] - w)/w)
  } else if(as.character(USXM[i,2])=="Japan"){
      w<-0.008860535
      USXM$PercentChange2[i] <- ((USXM[i,3] - w)/w)
  } else if(as.character(USXM[i,2])=="China"){
      w<-0.1433959
      USXM$PercentChange2[i] <- ((USXM[i,3] - w)/w)
  } else{
      USXM$PercentChange2[i] <- (USXM[i,3])
}}

USXM <- USXM %>% mutate(Year = date())
for (i in 1:(dim(USXM)[1])){
  if (USXM$Date[i] > "2019-10-31"){
    USXM$Year[i] <- "2019-2020"
    } else {
      USXM$Year[i] <- "2018-2019"
    }
}


  ###### Splitting Data for 2018-2019 and 2019-2020 ######
US18X <- subset(USXM ,Date < "2019-10-31")
US19X <- subset(USXM ,Date > "2019-10-31")
  ###### Using GGplots to create common format for Exchange Rate Plots ######
XFull <- ggplot()+ylab("Exchange Rate (US$/Foreign Currency)")+theme(legend.title = element_blank())
XFull1 <- XFull+ scale_x_date(date_breaks = "1 month",date_labels = ("%b\n%Y"))
XFull2 <- XFull+ scale_x_date(date_breaks = "2 month",date_labels = ("%b\n%Y"))

  ###### Using GGplots to Graph all EX for 7 countries for 2018-2020 Combined ######
X1820EX <- XFull2+geom_line(data=USXM,aes(x=Date, y=ExchangeRate,color = Country))
X1820EX <-X1820EX+ggtitle("Federal Funds Rate &\nUS Exchange Rates 2018 - 2020")
 ###### Using GGplots to Graph EX for all 7 countries for 2018-2019 Seperately ######
X18Split <- XFull1+geom_line(data=US18X,aes(x=Date, y=ExchangeRate,color=Country))
X18Split <- X18Split+ggtitle("Federal Funds Rate &\nUS Exchange Rates 2018 - 2019")
X18Split <- X18Split+facet_grid(vars(Country),scales = "free",space = "free_x")
  ###### Using GGplots to Graph EX for all 7 countries for 2019-2020 Seperately ######
X19Split <- XFull1+ geom_line(data=US19X,aes(x=Date, y=ExchangeRate, color=Country))
X19Split <- X19Split+ggtitle("Federal Funds Rate &\nUS Exchange Rates 2019 - 2020")
X19Split <- X19Split+facet_grid(vars(Country),scales = "free",space = "free_x")
  ###### Using GGplots to Graph EX for all 7 countries for 2018-2020 Combined Grid ######
X1820Grid <- XFull2+ geom_line(data=USXM,aes(x=Date, y=ExchangeRate, color=Country))
X1820Grid <- X1820Grid+ggtitle("Federal Funds Rate & US Exchange Rates 2019 - 2020")
X1820Grid <- X1820Grid+facet_grid(rows = vars(Country),cols = vars(Year),scales = "free",space = "free_x")

  ###### Using GGplots to create common format for Percent Change Plots ######
XP <- ggplot()+theme(legend.title = element_blank())+ylab("Percent Change in Exchange Rates")
XP1 <- XP+scale_x_date(date_breaks = "1 month",date_labels = ("%b\n%Y"))
XP2<- XP+scale_x_date(date_breaks = "2 month",date_labels = ("%b\n%Y"))
  ###### Using GGplots to Graph all PC for 7 countries for 2018-2020 Combined ######
PC1820 <- XP2+geom_line(data=USXM,aes(x=Date, y=PercentChange2,color = Country))+scale_y_continuous(breaks = seq((-0.75),2.5,0.25))
PC1820 <-PC1820+ggtitle("Federal Funds Rate & Percent Change\nin US Exchange Rates 2018 - 2020")
  ###### Using GGplots to Graph PC for all 7 countries for 2018-2019 Seperately ######
PC18Split <- XP+geom_line(data=US18X,aes(x=Date, y=PercentChange2,color=Country))
PC18Split <- PC18Split+ggtitle("Federal Funds Rate & Percent Change\nin US Exchange Rates 2018 - 2019")
PC18Split <- PC18Split+facet_grid(vars(Country),scales = "free",space = "free_x")
  ###### Using GGplots to Graph PC for all 7 countries for 2019-2020 Seperately ######
PC19Split <- XP+ geom_line(data=US19X,aes(x=Date, y=PercentChange2, color=Country))
PC19Split <- PC19Split+ggtitle("Federal Funds Rate & Percent Change\nin US Exchange Rates 2019 - 2020")
PC19Split <- PC19Split+facet_grid(vars(Country),scales = "free",space = "free_x")

FFR1<- ggplot()+ylab("Federal Funds Rate")+theme(legend.position = "none")
FFR1 <- FFR1+scale_x_date(date_breaks = "2 month",date_labels = ("%b\n%Y"))+ggtitle("Federal Funds Rate 2018-2020")
FFR1820 <-FFR1+geom_line(data=FFR,aes(x=Date, y=FFRate, color="red"))



  ###### Using GRDevices to print all Plots ######
png("2018-2020_All_CountriesEX.png",1600,1450,res=220)
X1820EX
dev.off()
png("2018-2020_All_CountriesExGrid.png",1600,1450,res=220)
X1820Grid
dev.off()
png("2018-2019_All_CountriesEX.png",1600,1450,res=220)
X18Split
dev.off()
png("2019-2020_All_CountriesEX.png",1600,1450,res=220)
X19Split
dev.off()

png("2018-2020_All_CountriesPC.png",1600,1450,res=220)
PC1820
dev.off()
png("2018-2019_All_CountriesPC.png",1600,1450,res=220)
PC18Split
dev.off()
png("2019-2020_All_CountriesPC.png",1600,1450,res=220)
PC19Split
dev.off()

png("2018-2020_FFR.png",1600,1450,res=220)
FFR1820
dev.off()