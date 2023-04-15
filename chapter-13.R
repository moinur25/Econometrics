######### 13.27#########
rm(list=ls())
data<-read.csv("D:/306/Table 7.9.csv")
lnY<-log(data$Y)
model1<-lm(lnY~X2+X3,data = data)
model2<-lm(lnY~X2+X3+X4,data = data)
summary(model1)
Y1<-residuals(model1)
Y2<-Y1^2
Y3<-Y1^3
model3<-lm(lnY~X2+X3+Y1+Y2+Y3,data = data)
summary(model3)
R_sq_old<-summary(model1)$r.square
R_sq_new<-summary(model3)$r.square
F_test<-(((R_sq_new-R_sq_old)/3)/((1-R_sq_new)/(23-5)))
dim(data)

