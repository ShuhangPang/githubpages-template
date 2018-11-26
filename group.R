library(tidyverse)
library(dplyr)
library(lubridate)
library(corrplot)
library(reshape2)
library(plyr)
library(broom) 
library(ggplot2)

#read the data solar_faro_pv
pv_2014 <- read.csv("~/Desktop/GROUP PROJECT/sunlab-faro-pv-2014.csv", sep=";")
pv_2015 <- read.csv("~/Desktop/GROUP PROJECT/sunlab-faro-pv-2015.csv", sep=";")
pv_2016 <- read.csv("~/Desktop/GROUP PROJECT/sunlab-faro-pv-2016.csv", sep=";")
pv_2017 <- read.csv("~/Desktop/GROUP PROJECT/sunlab-faro-pv-2017.csv", sep=";")

head(pv_2014)
#remove the data about the A
pv_B_2014 = pv_2014[,c(1,14:25)]
pv_B_2015 = pv_2015[,c(1,14:25)]
pv_B_2016 = pv_2016[,c(1,14:25)]
pv_B_2017 = pv_2017[,c(1,14:25)]

#combine the 4 data togather
data1 = full_join(pv_B_2014,pv_B_2015)
data2= full_join(pv_B_2016,pv_B_2017)
total_pv_B=full_join(data1,data2)

#disposal the date: extract the year, month from a date-time variable
total_pv_B$Datetime <- ymd_hms(total_pv_B$Datetime)

total_pv_B$Date <- as_date(total_pv_B$Datetime)
total_pv_B <- total_pv_B[,c(1,14,2:13)]

total_pv_B$Year <- year(total_pv_B$Datetime)
total_pv_B <- total_pv_B[,c(1:2,15,3:14)]
head(total_pv_B)

total_pv_B$Month <- month(total_pv_B$Datetime,label = TRUE)
total_pv_B <- total_pv_B[,c(1:3,16,4:15)]

total_pv_B$MDay <- mday(total_pv_B$Datetime)
total_pv_B <- total_pv_B[,c(1:4,17,5:16)]

total_pv_B$WDay <- wday(total_pv_B$Datetime, label = TRUE, abbr = FALSE)
total_pv_B <- total_pv_B[,c(1:5,18,6:17)]

total_pv_B$Hour <- hour(total_pv_B$Datetime)
total_pv_B <- total_pv_B[,c(1:6,19,7:18)]

total_pv_B$Minute <- minute(total_pv_B$Datetime)
total_pv_B <- total_pv_B[,c(1:7,20,8:19)]

total_pv_B$Second <- second(total_pv_B$Datetime)
total_pv_B <- total_pv_B[,c(1:8,21,9:20)]

summary(total_pv_B)
#find the correlation
feature_correlations <- cor(total_pv_B[,c(19,10:12)])
corrplot(feature_correlations, method="circle")

#linear regression
pv_output <- lm(Hour ~ B_Vertical...Voltage.DC..V.+B_Vertical...Current.DC..A.+B_Vertical...Power.DC..W.+B_Optimal...Voltage.DC..V.+
                  B_Horizontal...Current.DC..A.,data=total_pv_B)
summary(pv_output)

#read the data solar_faro_meteo
meteo_2014 <- read.csv("~/Desktop/GROUP PROJECT/sunlab-faro-meteo-2014.csv", sep=";")
meteo_2015 <- read.csv("~/Desktop/GROUP PROJECT/sunlab-faro-meteo-2015.csv", sep=";")
meteo_2016 <- read.csv("~/Desktop/GROUP PROJECT/sunlab-faro-meteo-2016.csv", sep=";")
meteo_2017 <- read.csv("~/Desktop/GROUP PROJECT/sunlab-faro-meteo-2017.csv", sep=";")

#combine the 4 data togather
colnames(meteo_2016)[4]<-c("Diffuse.Radiation..W.m2.")
data3 = full_join(meteo_2014,meteo_2015)
data4= full_join(meteo_2016,meteo_2017)
total_meteo=full_join(data3,data4)
tail(total_meteo)

#disposal the date: extract the year, month from a date-time variable
total_meteo$Datetime <- ymd_hms(total_meteo$Datetime)

total_meteo$Date <- as_date(total_meteo$Datetime)
total_meteo <- total_meteo[,c(1,10,2:9)]

total_meteo$Year <- year(total_meteo$Datetime)
total_meteo <- total_meteo[,c(1:2,11,3:10)]

total_meteo$Month <- month(total_meteo$Datetime,label = TRUE)
total_meteo<- total_meteo[,c(1:3,12,4:11)]

total_meteo$MDay <- mday(total_meteo$Datetime)
total_meteo <- total_meteo[,c(1:4,13,5:12)]

total_meteo$WDay <- wday(total_meteo$Datetime, label = TRUE, abbr = FALSE)
total_meteo <- total_meteo[,c(1:5,14,6:13)]

total_meteo$Hour <- hour(total_meteo$Datetime)
total_meteo <- total_meteo[,c(1:6,15,7:14)]

total_meteo$Minute <- minute(total_meteo$Datetime)
total_meteo <- total_meteo[,c(1:7,16,8:15)]

total_meteo$Second <- second(total_meteo$Datetime)
total_meteo <- total_meteo[,c(1:8,17,9:16)]

#plot the scatter to find the realtionship(if linear regression) ？how to change unknown to numeric
total_meteo[is.na(total_meteo)] <- 0
total_meteo$Ambient.Temperature..ºC.1 <-as.numeric(total_meteo$Ambient.Temperature..ºC.)
p <- ggplot(total_meteo,aes(x = Datetime,y = Wind.Direction..º.))+ geom_point()


#find the correlation: hour and arrounding environment
feature_correlations <- cor(total_meteo[,c(7,10:15)])
corrplot(feature_correlations, method="circle")

total_meteo$Ambient.Temperature..ºC. <- as.numeric(total_meteo$Ambient.Temperature..ºC.)
meteo_model <- lm(Hour ~ Ambient.Temperature..ºC.,data=total_meteo)
summary(meteo_model)
mod_output <- tidy(meteo_model)
mod_output
##find the correlation: month and arrounding environment
total_meteo$Datetime1 <- as_date(total_meteo$Datetime)
total_meteo$month<-as.numeric(total_meteo$Month)
total_meteo$Wind.Velocity..m.s.<-as.numeric(total_meteo$Wind.Velocity..m.s.)
feature_correlations <- cor(total_meteo[,c(20,10:15)])
corrplot(feature_correlations, method="circle")

#order the date
total_meteo1<- order(as.Date(total_meteo$Datetime))
#find the same date for the two data and joint them togather to find the relationship
