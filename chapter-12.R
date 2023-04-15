########## 12.26##########
rm(list = ls())
data<-read.csv("D:/306/Table 12.7.csv")
data1<-log(data)
model1<-lm(C~I+L+H+A, data=data1)
summary(model1)
plot(model1)
resi<-residuals(model1)
#load car package
#install.packages("carData")
library(car)
#perform Durbin-Watson test
durbinWatsonTest(model1)
#install.packages("randtests")
library(randtests)
Runs<-runs.test(model1$residuals)
acf(model1$residuals)
########### 12.34 #################
rm(list=ls())
library(dplyr)
data<-read.csv("D:/306/Table 12.9.csv")
model1<-lm(Inventories~Sales, data=data)
resi<-residuals(model1)
durbinWatsonTest(model1)
#resi_lag<-lag(resi,k=1)
#resi_sq<-sum(resi^2)
#data1<-data.frame(data,resi,resi_lag,resi_sq)
#rou_hat<-sum(data1$resi*data1$resi_lag, na.rm = TRUE)/sum(resi_sq)
#d<-2*(1-rou_hat)
d<-0.1255528
z<-sqrt(41)*(1-0.5*d)
model2<-lm(log(data1$Inventories)~log(data1$Sales),data=data1)

