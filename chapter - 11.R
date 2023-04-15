######11.15################
rm(list=ls())
packageNames<-c("tidyverse","stargazer","magrittr","lmtest","sandwich")
for(i in packageNames){
  if(!require(i,character.only = T)){
    install.packages(i,dependencies = T)
    require(i,character.only = T)
  }
}
library(tidyverse)
library(ggplot2)
data<-read.csv("D:/306/Table 11_7.csv")
model1<-lm(MPG~SP+HP+WT, data=data)
plot(model1)
data%<>%mutate(uhat=resid(model1),uhatsq=uhat^2)
ggplot(data = data,mapping = aes(x=MPG,y=uhatsq))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept = 0,col='green')+
  labs(y='residuals',x='mpg')
data%<>%mutate(spsq=SP^2,hpsq=HP^2,wtsq=WT^2,sphp=SP*HP,hpwt=HP*WT,spwt=SP*WT)
model_wt<-lm(uhatsq~SP+HP+WT+spsq+hpsq+wtsq+sphp+hpwt+spwt, data=data)
View(data)
summary(model_wt)
n<-81
R_sq<-summary(model_wt)$r.square
cal_chi<-n*R_sq
tab_chi<-3.32
cal_chi>tab_chi 
model_wls<-lm(formula=MPG~SP+HP+WT,data=data,weight=1/HP)
summary(model_wls)
plot(model_wls$residuals)
############ 11.20 #############
rm(list=ls())
library(tidyverse)
data<-read.csv("D:/306/Table 11.8.csv")
library(dplyr)
data%<>% mutate(ranking = rank(data$Years.in.Rank, ties.method = 'first'))
plot(data$Median,data$Year)
rankingsq<-data$ranking^2
model1<-lm(Median~Year, data=data)
summary(model1)
data$resi1<-model1$residuals
ggplot(data = data, aes(y = resi1, x = Median)) + geom_point(col = 'blue') + 
  geom_abline(slope = 0)
model2<-lm(Median~Year+Year.2, data=data)
summary(model2)
data$resi2<-model2$residuals
ggplot(data = data, aes(y = resi2, x = Median)) + geom_point(col = 'blue') + 
  geom_abline(slope = 0)
#residuals1<-plot(resid(model1))
#residuals2<-plot(resid(model2))
model3<-lm(Median~Year+Year.2, data=data, weights = 1/Year)
#residuals3<-plot(resid(model3))
data$resi3<-model3$residuals
ggplot(data = data, aes(y = resi3, x = Median)) + geom_point(col = 'blue') + 
  geom_abline(slope = 0)

