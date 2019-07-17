---
title: "demo"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
#import all necessary libraries
library(plyr)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(sqldf)

#loading token files

token_qtum = read.table(file = "D:/my_subjects_sem2/cuneyt/networkqtumTX.txt", header = F, sep = " ")
token_zrx = read.table(file = "D:/my_subjects_sem2/cuneyt/networkzrxTX.txt", header = F, sep = " ")
token_veros= read.table(file = "D:/my_subjects_sem2/cuneyt/networkverosTX.txt", header = F, sep = " ")


#loading prices files




#outliers

outlier_qtum <- token_qtum [which (token_qtum$TokenAmount >= 1.0e+26), ]
outlier_zrx <- token_zrx [which (token_zrx$TokenAmount>=1e+27), ]
outlier_veros <- token_veros [which (token_veros$TokenAmount >= 8e+13), ]



#outliers print
outlier_qtum
outlier_zrx
outlier_veros

#filternig the outliers


filtered_qtum  <- token_qtum [which (token_qtum$V4 < 1.0e+26), ]
filtered_zrx <- token_zrx [which (token_zrx$V4 < 1e+27), ]
filtered_veros   <- token_veros [which (token_veros$V4 < 8e+13), ]

head(filtered_qtum)
head(filtered_zrx)
head(filtered_veros)

#rename column names

library(sqldf)

com_qtum<-sqldf("SELECT t.Unixdate,p.Close, p.Open, t.TokenAmount FROM filtered_qtum t NATURAL JOIN prices_qtum p WHERE t.Unixdate = p.Date")

com_zrx<-sqldf("SELECT t.Unixdate,p.Close, p.Open, t.TokenAmount FROM filtered_zrx t NATURAL JOIN prices_zrx p WHERE t.Unixdate = p.Date")

com_veros<-sqldf("SELECT t.Unixdate,p.Close, p.Open, t.TokenAmount FROM filtered_veros t NATURAL JOIN prices_veros p WHERE t.Unixdate = p.Date")

#ddply

df_qtum <- as.data.frame(cbind(filtered_qtum$FromNodeId, filtered_qtum$ToNodeId))
df_zrx <- as.data.frame(cbind(filtered_zrx$FromNodeId, filtered_zrx$ToNodeId))
df_veros <- as.data.frame(cbind(filtered_veros$FromNodeId, filtered_veros$ToNodeId))

head(df_qtum)
head(df_zrx)
head(df_veros)

# pair wise frequency

counts_qtum <- ddply(df_qtum,.(df_qtum$V1,df_qtum$V2),nrow)
counts_zrx <- ddply(df_zrx,.(df_zrx$V1,df_zrx$V2),nrow)
counts_veros <- ddply(df_veros,.(df_veros$V1,df_veros$V2),nrow)

head(counts_qtum)
head(counts_zrx)
head(counts_veros)

#Create data frame for ID and frequency
x_qtum <- data.frame(counts_qtum$V1)
x_zrx <- data.frame(counts_zrx$V1)
x_veros <- data.frame(counts_veros$V1)

head(x_qtum)
head(x_zrx)
head(x_veros)

#reanme above dataframe coloumns
names(x_qtum) <- c("pairCount")
names(x_zrx) <- c("pairCount")
names(x_veros) <- c("pairCount")

head(x_qtum)
head(x_zrx)
head(x_veros)

# frequency plotting

FrequencyOfFrequency_qtum<-as.data.frame(table(x_qtum$pairCount))
colnames(FrequencyOfFrequency_qtum)<-c("No_of_Transactions","Frequency")

FrequencyOfFrequency_zrx<-as.data.frame(table(x_zrx$pairCount))
colnames(FrequencyOfFrequency_zrx)<-c("No_of_Transactions","Frequency")

FrequencyOfFrequency_veros<-as.data.frame(table(x_veros$pairCount))
colnames(FrequencyOfFrequency_veros)<-c("No_of_Transactions","Frequency")

#plot graph

#for qtum
logis_qtum<-fitdist(FrequencyOfFrequency_qtum[,2], "logis")
#unif_qtum<-fitdist(FrequencyOfFrequency_qtum[,2], "pois")
weibull_qtum<-fitdist(FrequencyOfFrequency_qtum[,2], "weibull")
nbinom_qtum<-fitdist(FrequencyOfFrequency_qtum[,2], "nbinom")
#norm_qtum<-fitdist(FrequencyOfFrequency_qtum[,2], "norm")

hist(FrequencyOfFrequency_qtum[,2], breaks = 500, prob = TRUE, main = "")
curve(dnorm(x,logis_qtum$estimate[1], logis_qtum$estimate[2]), col="red", lwd = 2, add = T)
#curve(dnorm(x,unif_qtum$estimate[1], unif_qtum$estimate[2]), col="green", lwd = 2, add = T)
curve(dnorm(x,weibull_qtum$estimate[1], weibull_qtum$estimate[2]), col="green", lwd = 2, add = T)
curve(dnorm(x,nbinom_qtum$estimate[1], nbinom_qtum$estimate[2]), col="yellow", lwd = 2, add = T)
#curve(dnorm(x,norm_qtum$estimate[1], norm_qtum$estimate[2]), col="brown", lwd = 2, add = T, n =300)

#for zrx
logis_zrx<-fitdist(FrequencyOfFrequency_zrx[,2], "logis")
#unif_zrx<-fitdist(FrequencyOfFrequency_zrx[,2], "unif")
weibull_zrx<-fitdist(FrequencyOfFrequency_zrx[,2], "weibull")
nbinom_zrx<-fitdist(FrequencyOfFrequency_zrx[,2], "nbinom")
#norm_zrx<-fitdist(FrequencyOfFrequency_zrx[,2], "norm")

hist(FrequencyOfFrequency_zrx[,2], breaks = 500, prob = TRUE, main = "")
curve(dnorm(x,logis_zrx$estimate[1], logis_zrx$estimate[2]), col="red", lwd = 2, add = T)
#curve(dnorm(x,unif_zrx$estimate[1], unif_zrx$estimate[2]), col="green", lwd = 2, add = T)
curve(dnorm(x,weibull_zrx$estimate[1], weibull_zrx$estimate[2]), col="orange", lwd = 2, add = T)
curve(dnorm(x,nbinom_zrx$estimate[1], nbinom_zrx$estimate[2]), col="black", lwd = 2, add = T)
#curve(dnorm(x,norm_zrx$estimate[1], norm_zrx$estimate[2]), col="brown", lwd = 2, add = T, n =300)


#for veros
logis_veros<-fitdist(FrequencyOfFrequency_veros[,2], "logis")
#unif_veros<-fitdist(FrequencyOfFrequency_veros[,2], "unif")
weibull_veros<-fitdist(FrequencyOfFrequency_veros[,2], "weibull")
nbinom_veros<-fitdist(FrequencyOfFrequency_veros[,2], "nbinom")
#norm_veros<-fitdist(FrequencyOfFrequency_veros[,2], "norm")

hist(FrequencyOfFrequency_veros[,2], breaks = 500, prob = TRUE, main = "")
curve(dnorm(x,logis_veros$estimate[1], logis_veros$estimate[2]), col="red", lwd = 2, add = T)
#curve(dnorm(x,unif_veros$estimate[1], unif_veros$estimate[2]), col="green", lwd = 2, add = T)
curve(dnorm(x,weibull_veros$estimate[1], weibull_veros$estimate[2]), col="orange", lwd = 2, add = T)
curve(dnorm(x,nbinom_veros$estimate[1], nbinom_veros$estimate[2]), col="black", lwd = 2, add = T)
#curve(dnorm(x,norm_veros$estimate[1], norm_veros$estimate[2]), col="brown", lwd = 2, add = T, n =300)


N_pos_qtum <- fitdist(FrequencyOfFrequency_qtum[,2], distr = "pois")
plot(N_pos_qtum)

N_pos_zrx <- fitdist(FrequencyOfFrequency_zrx[,2], distr = "pois")
plot(N_pos_zrx)

N_pos_veros <- fitdist(FrequencyOfFrequency_veros[,2], distr = "pois")
plot(N_pos_veros)

# QUESTION 2 REGRESSION 
#qtum data import
qtumPrice = read.table(file="D:/my_subjects_sem2/cuneyt/qtum.txt", header=F,sep="\t")
colnames(qtumPrice) <- c("Date","Open_Price","High_Price","Low_Price", "Close_Price" ,"Volume","Market Cap") 
colnames(filtered_qtum)<-c("FromNodeId", "ToNodeId", "Unixdate", "TokenAmount")

#Change Date Formats
qtumPrice$Date<-as.Date(qtumPrice$Date,format= "%m/%d/%Y")
qtumPrice$Date<- as.Date(as.POSIXct(qtumPrice$Date, origin="1970-01-01"))
filtered_qtum$Unixdate<- as.Date(as.POSIXct(filtered_qtum$Unixdate, origin="1970-01-01")) 

#Print qtum Data
head(qtumPrice)
head(filtered_qtum)

#zrx data imports
zrxPrice = read.table(file="D:/my_subjects_sem2/cuneyt/zrx.txt", header=F,sep="\t")
colnames(zrxPrice) <- c("Date","Open_Price","High_Price","Low_Price", "Close_Price" ,"Volume","Market Cap") 
colnames(filtered_zrx)<-c("FromNodeId", "ToNodeId", "Unixdate", "TokenAmount")

#Change Date Formats
zrxPrice$Date<-as.Date(zrxPrice$Date,format= "%m/%d/%Y")
zrxPrice$Date<- as.Date(as.POSIXct(zrxPrice$Date, origin="1970-01-01"))
filtered_zrx$Unixdate<- as.Date(as.POSIXct(filtered_zrx$Unixdate, origin="1970-01-01")) 

#Print zrx Data
head(zrxPrice)
head(filtered_zrx)


#veros data imports
verosPrice = read.table(file="D:/my_subjects_sem2/cuneyt/veros.txt", header=F,sep="\t")
colnames(verosPrice) <- c("Date","Open_Price","High_Price","Low_Price", "Close_Price" ,"Volume","Market Cap") 
colnames(filtered_veros)<-c("FromNodeId", "ToNodeId", "Unixdate", "TokenAmount")

#Change Date Formats
verosPrice$Date<-as.Date(verosPrice$Date,format= "%m/%d/%Y")
verosPrice$Date<- as.Date(as.POSIXct(verosPrice$Date, origin="1970-01-01"))
filtered_veros$Unixdate<- as.Date(as.POSIXct(filtered_veros$Unixdate, origin="1970-01-01"))  

#Print veros Data
head(verosPrice)
head(filtered_veros)

#---Active Buyer---

#qtum
buys.distribution.qtum  <- filtered_qtum %>% group_by(filtered_qtum[, 2]) %>% summarise(n = n()) %>% ungroup 
colnames(buys.distribution.qtum) <- c("BuyerId","Frequency_of_buys")
sorted_qtum_Buyer <- buys.distribution.qtum[order(-buys.distribution.qtum$Frequency_of_buys),]

#most active buyerId and no of times the buyer bought the token
head(sorted_qtum_Buyer,1)


#zrx
buys.distribution.zrx  <- filtered_zrx %>% group_by(filtered_zrx[, 2]) %>% summarise(n = n()) %>% ungroup 
colnames(buys.distribution.zrx) <- c("BuyerId","Frequency_of_buys")
sorted_zrx_Buyer <- buys.distribution.zrx[order(-buys.distribution.zrx$Frequency_of_buys),]

#most active buyerId and no of times the buyer bought the token
head(sorted_zrx_Buyer,1)


#veros
buys.distribution.veros <- filtered_veros %>% group_by(filtered_veros[, 2]) %>% summarise(n = n()) %>% ungroup 
colnames(buys.distribution.veros) <- c("BuyerId","Frequency_of_buys")
sorted_veros_Buyer <- buys.distribution.veros[order(-buys.distribution.veros$Frequency_of_buys),]

#most active buyerId and no of times the buyer bought the token
head(sorted_veros_Buyer,1)

#----Active Seller----

#qtum
seller.distribution.qtum  <- filtered_qtum %>% group_by(filtered_qtum[, 1]) %>% summarise(n = n()) %>% ungroup
names(seller.distribution.qtum) <- c("SellerId","Frequency_of_sells")
sorted_qtum_Seller <- seller.distribution.qtum[order(-seller.distribution.qtum$Frequency_of_sells),]

#most active sellerId and no of times the buyer bought the token
head(sorted_qtum_Seller,1)


#zrx
seller.distribution.zrx  <- filtered_zrx %>% group_by(filtered_zrx[, 1]) %>% summarise(n = n()) %>% ungroup
names(seller.distribution.zrx) <- c("SellerId","Frequency_of_sells")
sorted_zrx_Seller <- seller.distribution.zrx[order(-seller.distribution.zrx$Frequency_of_sells),]

#most active sellerId and no of times the buyer bought the token
head(sorted_zrx_Seller,1)


#veros
seller.distribution.veros  <- filtered_veros %>% group_by(filtered_veros[, 1]) %>% summarise(n = n()) %>% ungroup
names(seller.distribution.veros) <- c("SellerId","Frequency_of_sells")
sorted_veros_Seller <- seller.distribution.veros[order(-seller.distribution.veros$Frequency_of_sells),]

#most active sellerId and no of times the buyer bought the token
head(sorted_veros_Seller,1)


#---Regression for qtum---
filtered_qtum <- filtered_qtum[order(-filtered_qtum$TokenAmount),]
qtum_Regression<-sqldf("SELECT d.Unixdate,p.High_Price, p.Low_Price, p.Open_Price, p.Close_Price, d.TokenAmount FROM filtered_qtum d NATURAL JOIN qtumPrice p WHERE d.Unixdate = p.Date LIMIT 10000")

head(qtum_Regression)

#filtered_qtum <- filtered_qtum[order(-filtered_qtum$TokenAmount),]
#qtum_Regression<-sqldf("SELECT d.Unixdate,p.Low_Price, p.Open_Price, d.TokenAmount FROM filtered_qtum d NATURAL JOIN qtumPrice p WHERE #d.Unixdate = p.Date order by d.TokenAmount desc LIMIT 90000")
#head(qtum_Regression)

qtum_Regression <- qtum_Regression[order(-qtum_Regression$TokenAmount, -qtum_Regression$Open_Price),]

head(qtum_Regression)

lm.fit.qtum= lm(Low_Price ~ TokenAmount+Open_Price, data= qtum_Regression)
summary(lm.fit.qtum)

plot(lm.fit.qtum)

#---Regression for zrx---


#---Regression for veros---
filtered_veros <- filtered_veros[order(-filtered_veros$TokenAmount),]
veros_Regression<-sqldf("SELECT d.Unixdate,p.High_Price, p.Low_Price, p.Open_Price, p.Close_Price, d.TokenAmount FROM filtered_veros d NATURAL JOIN qtumPrice p WHERE d.Unixdate = p.Date LIMIT 10000")

head(veros_Regression)

#filtered_qtum <- filtered_qtum[order(-filtered_qtum$TokenAmount),]
#qtum_Regression<-sqldf("SELECT d.Unixdate,p.Low_Price, p.Open_Price, d.TokenAmount FROM filtered_qtum d NATURAL JOIN qtumPrice p WHERE #d.Unixdate = p.Date order by d.TokenAmount desc LIMIT 90000")
#head(qtum_Regression)

veros_Regression <- veros_Regression[order(-veros_Regression$TokenAmount, -veros_Regression$Open_Price),]

head(veros_Regression)

lm.fit.veros= lm(Low_Price ~ TokenAmount+Open_Price, data= veros_Regression)
summary(lm.fit.veros)

plot(lm.fit.veros)



