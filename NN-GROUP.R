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

#remove the data about the A
pv_B_2014 = pv_2014[,c(1,14:25)]
pv_B_2015 = pv_2015[,c(1,14:25)]
pv_B_2016 = pv_2016[,c(1,14:25)]
pv_B_2017 = pv_2017[,c(1,14:25)]

#combine the 4 data togather
data1 = full_join(pv_B_2014,pv_B_2015)
data2= full_join(pv_B_2016,pv_B_2017)
total_pv_B=full_join(data1,data2)

#change the column name
colnames(total_pv_B)[2:13] <- c("Vertical_Voltage(V)","Vertical_Current(A)","Vertical_Power(W)","Optimal_Voltage(V)",
                                "Optimal_Current(A)","Optimal_Power(W)","Horizontal_Voltage(V)","Horizontal_Current(A)",
                                "Horizontal_Power(W)","Vertical_Temperature(ºC)","Optimal_Temperature(ºC)","Horizontal_Temperature(ºC)")

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

#change the column name
colnames(total_meteo)[2:9] <- c("Ambient_Temperature(ºC)","Global_Radiation(W.m2)","Diffuse_Radiation(W.m2)","Ultraviolet(W.m2)",
                                "Wind_Velocity(m.s)","Wind_Direction(º)","Precipitation(mm)","Atmospheric_pressure(hPa)")


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

#combine two data together
pv_meteo=left_join(total_pv_B,total_meteo)

#Neural Networks
library(neuralnet)
head(pv_meteo)

pv_meteo$Month <- as.numeric(pv_meteo$Month)
pv_meteo$WDay <- as.numeric(pv_meteo$WDay)
pv_meteo$`Ambient_Temperature(ºC)`<-as.numeric(pv_meteo$`Ambient_Temperature(ºC)`)
pv_meteo$`Wind_Velocity(m.s)`<-as.numeric(pv_meteo$`Wind_Velocity(m.s)`)

pv_meteo_reduce <- pv_meteo[c(3:8,15,20,22:29)]

net <- neuralnet(pv_meteo_reduce$`Optimal_Power(W)`~pv_meteo_reduce$Year+pv_meteo_reduce$Month+pv_meteo_reduce$MDay
                 +pv_meteo_reduce$WDay+pv_meteo_reduce$Hour+pv_meteo_reduce$Minute+pv_meteo_reduce$`Optimal_Temperature(ºC)`,
                  pv_meteo_reduce, hidden=15, threshold=0.005, learningrate = 0.1, algorithm = "rprop+", 
                 err.fct = "sse", act.fct = "logistic")
print(net)   
plot(net) 

#test the nn
#testdata <- matrix(c(variables),1,3,byrow = T) #建立测试集
#net.results <- compute(net, testdata) #利用训练好的模型进行预测
#ls(net.results)
#print(net.results$net.result)

#increase the variables
pv_meteo_reduce = na.omit(pv_meteo_reduce)
net1 <- neuralnet(pv_meteo_reduce$`Optimal_Power(W)` ~ pv_meteo_reduce$Year+pv_meteo_reduce$Month+pv_meteo_reduce$MDay+pv_meteo_reduce$WDay+pv_meteo_reduce$Hour+pv_meteo_reduce$Minute+
                    pv_meteo_reduce$`Optimal_Temperature(ºC)`+ pv_meteo_reduce$`Ambient_Temperature(ºC)`+
                   pv_meteo_reduce$`Global_Radiation(W.m2)`+ pv_meteo_reduce$`Diffuse_Radiation(W.m2)`+pv_meteo_reduce$`Ultraviolet(W.m2)`+pv_meteo_reduce$`Wind_Velocity(m.s)`+
                   pv_meteo_reduce$`Wind_Direction(º)` + pv_meteo_reduce$`Precipitation(mm)`+pv_meteo_reduce$`Atmospheric_pressure(hPa)`,
                 pv_meteo_reduce, hidden=25, threshold=0.005, learningrate = 0.1, algorithm = "rprop+", linear.output = F,
                 err.fct = "sse", act.fct = "logistic")
print(net1)   
plot(net1) 

