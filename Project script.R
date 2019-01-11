## Group Project  

library(lubridate)
library(tidyverse)
library(stringr)
library(ggplot2)
library(glmnet)


setwd("C:/Users/angus/OneDrive/Documents/Year 4 notes/Data Analysis for Energy Systems/Group Project")


# Data --------------------------------------------------------------------

## Read in weather data

meteo_2017 <- read.csv("sunlab-faro-meteo-2017.csv",sep=";")
meteo_2016 <- read.csv("sunlab-faro-meteo-2016.csv",sep=";")
meteo_2015 <- read.csv("sunlab-faro-meteo-2015.csv",sep=";")
meteo_2014 <- read.csv("sunlab-faro-meteo-2014.csv",sep=";")

## Read in Pv data

pvsunlab_2017 <- read.csv("sunlab-faro-pv-2017.csv",sep=";")
pvsunlab_2016 <- read.csv("sunlab-faro-pv-2016.csv",sep=";")
pvsunlab_2015 <- read.csv("sunlab-faro-pv-2015.csv",sep=";")
pvsunlab_2014 <- read.csv("sunlab-faro-pv-2014.csv",sep=";")




# Renaming the Data -------------------------------------------------------

## Renaming the Weather data

Weather_columns <- c("Datetime","Ambient_Temperature", "Global_Radiation", "Diffuse_Radiation",
                     "UV", "Wind_Velocity","Wind_Direction","Precipitation","Atmospheric_Pressure")


for(i in seq(2,length(meteo_2017))){
  names(meteo_2017)[i] <- Weather_columns[i]
}

for(i in seq(2,length(meteo_2016))){
  names(meteo_2016)[i] <- Weather_columns[i]
}

for(i in seq(2,length(meteo_2015))){
  names(meteo_2015)[i] <- Weather_columns[i]
}

for(i in seq(2,length(meteo_2014))){
   names(meteo_2014)[i] <- Weather_columns[i]
}

#### Renaming the PV Data


## Found that replace_all was faster and could be done in one line, is probably better than the for loop for cleaning the names so could replace that part later.

names(pvsunlab_2017) <- str_replace_all(names(pvsunlab_2017),c("..ºC."="",".DC..A."="",".DC..W."="",".DC..V."="","\\.\\.\\."="_"))

names(pvsunlab_2016) <- str_replace_all(names(pvsunlab_2016),c("..ºC."="",".DC..A."="",".DC..W."="",".DC..V."="","\\.\\.\\."="_"))

names(pvsunlab_2015) <- str_replace_all(names(pvsunlab_2015),c("..ºC."="",".DC..A."="",".DC..W."="",".DC..V."="","\\.\\.\\."="_"))

names(pvsunlab_2014) <- str_replace_all(names(pvsunlab_2014),c("..ºC."="",".DC..A."="",".DC..W."="",".DC..V."="","\\.\\.\\."="_"))


## Make sure columns are numeric
## The ambient temperature and the wind velocity in the weather data is an unknown factor in both

meteo_2017$Ambient_Temperature <- as.numeric(meteo_2017$Ambient_Temperature)
meteo_2017$Wind_Velocity <- as.numeric(meteo_2017$Wind_Velocity)

meteo_2016$Ambient_Temperature <- as.numeric(meteo_2016$Ambient_Temperature)
meteo_2016$Wind_Velocity <- as.numeric(meteo_2016$Wind_Velocity)

meteo_2015$Ambient_Temperature <- as.numeric(meteo_2015$Ambient_Temperature)
meteo_2015$Wind_Velocity <- as.numeric(meteo_2015$Wind_Velocity)

meteo_2014$Ambient_Temperature <- as.numeric(meteo_2014$Ambient_Temperature)
meteo_2014$Wind_Velocity <- as.numeric(meteo_2014$Wind_Velocity)

## Again probably want to write a function to clean these? 


# Manipulating the data ---------------------------------------------------

meteo_2017$Datetime <- ymd_hms(meteo_2017$Datetime)
meteo_2017$Date <- as.Date(as_date(meteo_2017$Datetime))
meteo_2017$Hour <- hour(meteo_2017$Datetime)
meteo_2017$Month <- month(meteo_2017$Datetime)
meteo_2017$Minutes <- minute(meteo_2017$Datetime)

meteo_2016$Datetime <- ymd_hms(meteo_2016$Datetime)
meteo_2016$Date <- as.Date(as_date(meteo_2016$Datetime))
meteo_2016$Hour <- hour(meteo_2016$Datetime)
meteo_2016$Month <- month(meteo_2016$Datetime)
meteo_2016$Minutes <- minute(meteo_2016$Datetime)

meteo_2015$Datetime <- ymd_hms(meteo_2015$Datetime)
meteo_2015$Date <- as.Date(as_date(meteo_2015$Datetime))
meteo_2015$Hour <- hour(meteo_2015$Datetime)
meteo_2015$Month <- month(meteo_2015$Datetime)
meteo_2015$Minutes <- minute(meteo_2015$Datetime)

meteo_2014$Datetime <- ymd_hms(meteo_2014$Datetime)
meteo_2014$Date <- as.Date(as_date(meteo_2014$Datetime))
meteo_2014$Hour <- hour(meteo_2014$Datetime)
meteo_2014$Month <- month(meteo_2014$Datetime)
meteo_2014$Minutes <- minute(meteo_2014$Datetime)


## 2017 Model

meteo_model_2017<- lm(Global_Radiation ~ Ambient_Temperature+Wind_Velocity+Wind_Direction+Precipitation+Atmospheric_Pressure, 
                      data=meteo_2017)
summary(meteo_model_2017)

## 2016 Model

meteo_model_2016<- lm(Global_Radiation ~ Ambient_Temperature+Wind_Velocity+Wind_Direction+Precipitation+Atmospheric_Pressure, 
                      data=meteo_2016)
summary(meteo_model_2016)

## 2015 Model

## NA's in the percipitation and atmospheric pressure

meteo_2015[is.na(meteo_2015)] <- 0  ### change na's to 0???

meteo_model_2015<- lm(Global_Radiation ~ Ambient_Temperature+Wind_Velocity+Wind_Direction+Precipitation+Atmospheric_Pressure, 
                      data=meteo_2015)
summary(meteo_model_2015)

## 2014 Model

meteo_2014[is.na(meteo_2014)] <-0

meteo_model_2014<- lm(Global_Radiation ~ Ambient_Temperature+Wind_Velocity+Wind_Direction+Precipitation+Atmospheric_Pressure, 
                      data=meteo_2014)
summary(meteo_model_2014)

## The model had the greatest dependence on the ambient temperature and the wind velocity, all the other factors
## had smaller contributions to the R-Squared value. Possibly be ok taking atmospheric pressure and precipitation out the model.


# New Grouped Datasets ----------------------------------------------------

## There were some off temperature values I filtered out, should probably set some max and min boundaries for all the variables 
## Re-order these for my ocd's sake 


## 2015 Summary 

nm <- meteo_2015 %>% filter(Ambient_Temperature > -30)
  
monthly_weather_summary_2015 <- nm %>% 
  group_by(Month) %>% 
  summarise(Ambient_Temperature = mean(Ambient_Temperature), Global_Radiation = mean(Global_Radiation),
            Diffuse_Radiation = mean(Global_Radiation),UV = mean(UV), Wind_Velocity = mean(Wind_Velocity),
            Wind_Direction = mean(Wind_Direction), Precipitation = mean(Precipitation), 
            Atmospheric_Pressure =mean(Atmospheric_Pressure))

nm_1_plot <- ggplot(monthly_weather_summary_2015) + geom_point(aes(x = Month, y = Global_Radiation, size = Ambient_Temperature, color = Wind_Velocity))
nm_1_plot

## 2014 Summary

nm2 <- meteo_2014 %>% filter(Ambient_Temperature > -30)

monthly_weather_summary_2014 <- nm2 %>% 
  group_by(Month) %>% 
  summarise(Ambient_Temperature = mean(Ambient_Temperature), Global_Radiation = mean(Global_Radiation),
            Diffuse_Radiation = mean(Global_Radiation),UV = mean(UV), Wind_Velocity = mean(Wind_Velocity),
            Wind_Direction = mean(Wind_Direction), Precipitation = mean(Precipitation), 
            Atmospheric_Pressure =mean(Atmospheric_Pressure))

nm_2_plot <- ggplot(monthly_weather_summary_2014) + geom_point(aes(x = Month, y = Global_Radiation, size = Ambient_Temperature, color = Wind_Velocity))
nm_2_plot

## 2017 Summary

nm3 <- meteo_2017 %>% filter(Ambient_Temperature > -30)

monthly_weather_summary_2017 <- nm3 %>% 
  group_by(Month) %>% 
  summarise(Ambient_Temperature = mean(Ambient_Temperature), Global_Radiation = mean(Global_Radiation),
            Diffuse_Radiation = mean(Global_Radiation),UV = mean(UV), Wind_Velocity = mean(Wind_Velocity),
            Wind_Direction = mean(Wind_Direction), Precipitation = mean(Precipitation), 
            Atmospheric_Pressure =mean(Atmospheric_Pressure))

nm_3_plot <- ggplot(monthly_weather_summary_2017) + geom_point(aes(x = Month, y = Global_Radiation, size = Ambient_Temperature, color = Wind_Velocity))
nm_3_plot

## 2016 Summary

nm4 <- meteo_2016 %>% filter(Ambient_Temperature > -30)

monthly_weather_summary_2016 <- nm4 %>% 
  group_by(Month) %>% 
  summarise(Ambient_Temperature = mean(Ambient_Temperature), Global_Radiation = mean(Global_Radiation),
            Diffuse_Radiation = mean(Global_Radiation),UV = mean(UV), Wind_Velocity = mean(Wind_Velocity),
            Wind_Direction = mean(Wind_Direction), Precipitation = mean(Precipitation), 
            Atmospheric_Pressure =mean(Atmospheric_Pressure))

nm_4_plot <- ggplot(monthly_weather_summary_2016) + geom_point(aes(x = Month, y = Global_Radiation, size = Ambient_Temperature, color = Wind_Velocity))
nm_4_plot

## Could now use select to partition it into monthly data or seasonal?


# Cleaning the Data -------------------------------------------------------

## Removing extreme values by catergory 

## setting min and max ambient temperature should realistically be between 44 and -2 

meteo_2014 <- meteo_2014 %>% filter(Ambient_Temperature < 44, Ambient_Temperature > -5)

## Shouldn't have a negative for radiation or values of over 2,000 really so can set these as boundaries

meteo_2014 <- meteo_2014 %>% filter(Global_Radiation < 2000, Global_Radiation > -2)

## Diffuse is less than global radiation but not neccessarily the largest global radiation values that lead to largest diffuse 
## Max of about 800 
## May not be worth removing small negatives as rest of the data maybe reasonable.

meteo_2014 <- meteo_2014 %>% filter(Diffuse_Radiation < 1000, Diffuse_Radiation > -2)

meteo_2014 <- meteo_2014 %>% filter(UV < 500, UV > -2)

## One of these 3 removed all the data by accident, also is technically speed not velocity as doesn't have direction in one.

meteo_2014 <- meteo_2014 %>% filter(Wind_Velocity < 200, Wind_Velocity > -2)

meteo_2014$Precipitation <- meteo_2014$Precipitation[is.na(meteo_2014$Precipitation)] <- 0

## 2014 is just missing all it's values for percipitation and atmoshperic pressure so should I just remove them.


meteo_2014 <- meteo_2014 %>% filter(Precipitation < 100, Precipitation > -2)

meteo_2014$Atmospheric_Pressure <- meteo_2014$Atmospheric_Pressure[is.na(meteo_2014$Atmospheric_Pressure)] <- 0

meteo_2014 <- meteo_2014 %>% filter(Atmospheric_Pressure < 2000, Atmospheric_Pressure > -2)

## All seem fine now not sure why that happened.


## As the right data source e.g. numeric, factors etc

## Duplicates?

## How would we want to filter this data? obvioulsy some catergories are less important than others.

## Dark sky API


# Penalised regression ----------------------------------------------------

## Need to join and match the output of optimal power B to weather data by the date

pvsunlab_2014$Datetime <- ymd_hms(pvsunlab_2014$Datetime)

combined_2014 <- left_join(pvsunlab_2014,meteo_2014,by="Datetime")

## 262145 observation

combined_2014 <- arrange(combined_2014,Datetime)

combined_2014 <- drop_na(combined_2014) ## only do one

## 248877 observations so lost 13268 observations 

## Get rid of NA's and make a matrix to use

summarise(combined_2014)

## why does it say it has 0 columns and 0 rows?

combined_2014$Ambient_Temperature <- as.numeric(combined_2014$Ambient_Temperature)

## This won't now convert it to a numeric column ??



x_2014 <- as.matrix(combined_2014[,c(26:31,1)])
y_2014 <- as.matrix(combined_2014[,19])



ridge <- glmnet(x_2014,y_2014,family = "gaussian",alpha = 0)
print(ridge)

plot(ridge,xvar ="lambda")

cvfit_2014 <- cv.glmnet(x_2014,y_2014)
plot(cvfit_2014)

cvfit_2014$lambda.min
cvfit_2014$lambda.1se

coef.cv.glmnet(cvfit_2014, s="lambda.1se")
prediction <- predict(cvfit_2014,x_2014,s="lambda.1se")

difference <- prediction - y_2014

summary(combined_2014$B_Optimal_Power)

plot(combined_2014$Datetime,prediction, type = "l")

data.f <- data.frame(combined_2014$Datetime,prediction,y_2014)
ggplot(data.f,aes(combined_2014$Datetime))+ geom_line(aes(y=y_2014),colour = "red")

## Do it by month.


## Never included time in the x variables in the first model but the power output would follow a roughly sinusoidal power output over the day 
## If we therefore included a independent variable for time in the day and made it sinusoidal we could use it in the model
## Would have to be normalised over the time of a day
## Could then ddo the same but for seasonal changes, this maybe more difficult to model need to know climate of area.
## Are there enough coeffecients and are they large enough to require a ridge regression?

g_lm_2014 <- glm(B_Optimal_Power ~ sin(Hour)+ Global_Radiation +UV +Diffuse_Radiation +Ambient_Temperature +Wind_Velocity +Wind_Direction
                , data= combined_2014)
summary(g_lm_2014)

prediction_2 <- predict(g_lm_2014,combined_2014[,c(26:33,35)])

data.f_2 <- data.frame(combined_2014$Datetime,prediction_2,y_2014)
ggplot(data.f_2, aes(combined_2014$Datetime))+geom_line(aes(y=prediction_2),colour = "red")

first_tests <- (prediction_2 - y_2014)

plot(first_tests)



### The first two models pretty similar just get some small random noise. What if we combined all datasets then did ridge regression?

lm_2014 <- lm(B_Optimal_Power ~ sin(Month)+ Global_Radiation +Diffuse_Radiation +Ambient_Temperature +Wind_Velocity +Wind_Direction
                 , data= combined_2014)
summary(lm_2014)

## Adjust amplitude of sin function ##

prediction_3 <- predict(lm_2014,combined_2014[,c(26:33,36)])

prediction_3 <-data.frame(prediction_3)


data.f_3 <- data.frame(combined_2014$Datetime[75000:85000],prediction_3[75000:85000],y_2014[75000:85000])
ggplot(data.f_3,aes(x=combined_2014$Datetime[75000:85000])) +geom_line(aes(y=prediction_3[75000:85000]),colour = "red") +geom_line(aes(y=y_2014[75000:85000]), colour = "blue")  


## investigate some of these dates

may_test <- combined_2014[75000:85000,]

mins<- may_test %>% group_by(Datetime = cut(Datetime, breaks = "5 min")) %>%
  summarise(Ambient_Temperature = mean(Ambient_Temperature), Global_Radiation = mean(Global_Radiation),
            Diffuse_Radiation = mean(Diffuse_Radiation), UV = mean(UV), Wind_Velocity = mean(Wind_Velocity),
            Wind_Direction = mean(Wind_Direction), Precipitation = mean(Precipitation), 
            Atmospheric_Pressure = mean(Atmospheric_Pressure))

