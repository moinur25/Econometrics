####10.27####
rm(list = ls())
library(tidyverse)
data<-read.csv("D:/306/Table 10.13.csv")
View(data)
data1<-data%>%mutate(lnGDP=log(data$GDP), lnCPI=log(data$CPI),lnImports=log(data$Imports))
model1<-lm(lnImports~lnGDP+lnCPI, data=data1)
summary(model1)
library(olsrr)
ols_vif_tol(model1)
model2<-lm(lnImports~lnGDP, data=data1)
model3<-lm(lnImports~lnCPI, data=data1)
model4<-lm(lnGDP~lnCPI, data=data1)
ln
####10.30####
rm(list = ls())
data<-read.csv("D:/306/Table 10.15.csv")
data1<-data[-1,]
model1<-lm(HRS~RATE+ERSP+ERNO+NEIN+ASSET+AGE+DEP+SCHOOL, data=data1)
summary(model1)
ols_vif_tol(model1)

