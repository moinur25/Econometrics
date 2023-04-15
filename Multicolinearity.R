data<-read.csv("D:/306/Table 10_18.csv")
data1<-data[,-1]
View(data1)
str(data1)
data1$LACTIC1<-log(data1$LACTIC1)
data$LACTIC1==data1$LACTIC1
head(data)
head(data1)
#(a). Draw a scatter plot of the four variables. Interpret the scatter plot.
pairs(~ACETIC+H2S+LACTIC+LACTIC1, data=data1,col='blue',main = "Scatterplot Matrix")
#(b). Find X′X and (X′X)−1 and interpret your results.
X<-as.matrix(data1[,-1])
X_prime<-t(X)
X_prime.X<-X_prime%*%X
X_prime.X_inv<-solve(X_prime.X)
#(c). Find correlation matrix and interpret your results.
c_matrix<-cor(data1,method = "pearson")
dim(c_matrix)
#(d). Perform a bivariate regression of taste on acetic and H2S and interpret your results.
model1<-lm(TASTE~ACETIC+H2S,data=data1)
model1
#(e). Perform a bivariate regression of taste on lactic and H2S, and interpret the results.
model2<-lm(TASTE~LACTIC+H2S,data=data1)
model2
#(f). Perform a multiple regression of taste on acetic, H2S, lactic, and lactic1. Interpret your results.
model3<-lm(TASTE~ACETIC+H2S+LACTIC+LACTIC1,data=data1)
model3
#(g).Check the coefficient diagnosis of multiple regression model estimates found in the solution of (f) and interpret your results.
summary(model3)
#(h). Knowing what you know about multicollinearity, how would you decide among these regressions?
#install.packages("olsrr")
library(olsrr)
#install.packages("tidyverse")
library(tidyverse)
ols_vif_tol(model3)

#Altarnative way to calculate VIF value
#install.packages("GGally")
library(GGally)
#install.packages("car")
library(car)
vif_values<-vif(model3)#variance inflation factor
vif_values
barplot(vif_values,main="VIF VALUES",horiz=FALSE,col = "steelblue")
#(i). How do you take remedial measure if you have multicollinearity? According to you,what is the best regression equation?
model4<-lm(TASTE~LACTIC+N,data=data1)
summary(model4)
ols_vif_tol(model4)





