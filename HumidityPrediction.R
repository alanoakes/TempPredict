# load libraries
library(readxl)
library(ggcorrplot)
library(ggplot2)
library(gridExtra)

# load "Historical weather around Szeged, Hungary (from 2006 to 2016)"
# url = "https://www.kaggle.com/budincsevity/szeged-weather"
setwd("~/R/WeatherHistLinReg/")
Data1 <- read.csv("weatherHistory.csv")

# Weather in Szeged 2006-2016: Is there a relationship between humidity and 
# temperature? What about between humidity and apparent temperature? Can you 
# predict the apparent temperature given the humidity?

# Perform Correlation Analysis
names(Data1)
Data2 <- Data1[, c(4,5,6)] # keeping predictor and outcome
names(Data2) <- c("Temp","AppTemp","Humidity")
summary(Data2)
Data3 <- round(cor(Data2),1)
Data3 # view correlation matrix
ggcorrplot(Data3, method = "circle")

# building predictive model using: "Humidity" as predictor and 
# "Temperature" and "Apparaent Temperature" as outcome
TempFit <- lm(Temp ~ I(Humidity - mean(Humidity)), data = Data2)
AppTempFit <- lm(AppTemp ~ I(Humidity - mean(Humidity)), data = Data2)

# Get a 95% confidence interval for the expected house price at the average count
# of convenient store
list("Temperature" = confint(TempFit), 
      "Apparent Temperature" = confint(AppTempFit))

# give temperature predictions for 0.5 humdity
Temperature  <- predict(TempFit, newdata = data.frame(Humidity = 0.5), 
               interval = "prediction")
ApparentTemp <- predict(AppTempFit, newdata = data.frame(Humidity = 0.5), 
               interval = "prediction")
Prediction <- rbind(Temperature,ApparentTemp)
row.names(Prediction) <- c("Temperature","ApparentTemp")
Prediction1<-as.data.frame(Prediction)
Prediction1$fitF <- Prediction1$fit * 1.8 + 32
Prediction1$lwrF <- Prediction1$lwr * 1.8 + 32
Prediction1$uprF  <- Prediction1$upr * 1.8 + 32
Prediction1

# visualize linear regression model
par(mfrow = c(1,2))
plot(Data2$Humidity,Data2$Temp,
     xlab = "Humidity", ylab = "Temperature", col = "grey")
  abline(TempFit,col = "red", lwd = 2)
title(list("Temperature Prediction Given Humidity\nIn Szeged During 2006-2016"
           ,cex=1.5, col="grey40",font = 6),line=-3,outer=T)
plot(Data2$Humidity,Data2$AppTemp,
     xlab = "Humidity", ylab = "Apparent Temperature", col = "grey")
  abline(AppTempFit,col = "red", lwd = 2)