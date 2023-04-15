#heteroscedasticity
#outline
#graphical analysis of heteroscedasticity
#heteroscedasticity test
#Breuch-pagan test
#white test
#alternative white test
#weighted least square(WLS)
#Fesible Generalized least square(FGLS)
#install packages
packageNames<-c("tidyverse","stargazer","magrittr","lmtest","sandwich")
for(i in packageNames){
  if(!require(i,character.only = T)){
    install.packages(i,dependencies = T)
    require(i,character.only = T)
  }
}

#Graphical analysis of heteroscedasticity
#dataset on house prices
hprice1<-read.csv("D:/306/hprice1.csv")
hprice1%>%
  select(price,lprice,lotsize,sqrft,bdrms)%>%
  str

hprice1%>%
  select(price,lprice,lotsize,sqrft,bdrms)%>%
  stargazer(type = "text")
hprice1%>%
  select(price,lprice,lotsize,sqrft,bdrms)%>%
  head(10)
#regression model for price
model_0<-lm(price~lotsize+sqrft+bdrms,hprice1)
summary(model_0)
hprice1%<>%mutate(uhat=resid(model_0))
#geaph of residual against independent variable
hprice1%<>%mutate(uhat=resid(model_0))
ggplot(data=hprice1,mapping = aes(x=sqrft,y=uhat))+
  theme_bw()+
  geom_point()+
  geom_hline(yintercept = 0,col='red')+
  labs(y='residuals',x='square feet,sqrft')


#geaph of residual against independent fittet value
hprice1%<>%mutate(yhat=fitted(model_0))
ggplot(data=hprice1,mapping = aes(x=yhat,y=uhat))+
  theme_bw()+
  geom_point()+
  geom_hline(yintercept = 0,col='red')+
  labs(y='residuals',x='yhat')
hprice1%<>%mutate(lotsizesq=lotsize^2,
                  sqrftsq=sqrft^2,
                  bdrmssq=bdrms^2,
                  lotsizexbdrms=lotsize*bdrms,
                  lotsizexsqrft=lotsize*sqrft,
                  sqrftxbdrms=sqrft*bdrms
                  )
#heteroscedasticity for price
#residual model is the same as model_0
summary(model_0)
#get residual(uhat) and predicted value(yhat),and square them
hprice1%<>%mutate(uhatsq=uhat^2,
                  yhatsq=yhat^2)
#Breusch-pagan test
#regression for breusch pagan test
model_BP<-lm(uhatsq~lotsize+sqrft+bdrms,hprice1)
summary(model_BP)

#number of independent variable k1
(k1<-model_BP$rank-1)
#F-test and LM-test for heteroscadescity
(r2<-summary(model_BP)$r.squared)#R-square value
(n<-nobs(model_BP))#number of obs
(F_stat<-(r2/k1)/((1-r2)/(n-k1-1)))#F-statistics
(F_pval<-pf(F_stat,k1,n-k1-1,lower.tail=FALSE))#p-value

(LM_stat<-n*r2)#LM statistics
(LM_pval<-pchisq(q=LM_stat,df=k1,lower.tail=FALSE))#p-value

#white test
#regression for white test
model_white<-lm(uhat~lotsize+sqrft+bdrms+lotsizesq+sqrftsq+
                  bdrmssq+lotsizexsqrft+lotsizexbdrms+sqrftxbdrms,hprice1)
summary(model_white)
#number of independent variable
(k2<-model_white$rank-1)
#F-test and LM test for heterosc...
(r2<-summary(model_white)$r.squared)#R-square
(n<-nobs(model_white))#number of observation
(F_stat<-(r2/k2)/((1-r2)/(n-k2-1)))#F-statistics
(F_pval<-pf(F_stat,k2,n-k2-1,lower.tail=FALSE))#p-value
(LM_stat<-n*r2)#LM statistics(Lagrange Multiple)
(LM_pval<-pchisq(q=LM_stat,df=k2,lower.tail=FALSE))#p-value
#(WLS)
#when the heteros.. form is known var(u|x)=(sigma^2)*(sqrft)
#use wls with weight=1/sqrft
model_wls1<-lm(formula=price~lotsize+sqrft+bdrms,data=hprice1,weight=1/sqrft)
summary(model_wls1)

