library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
library(readxl)
library(knitr)
 library(rmarkdown)
library(simmer)
library(simmer.plot)


summary(DataBase)
##Details <- lm(DATABASE$`Annual salary` ~ DATABASE$AGPG + DATABASE$AAPG + DATABASE$PS + DATABASE$Disp + DATABASE$OverAll, data=DATABASE)


hist(DataBase1$`Annual salary`,prob=TRUE, main='Annual salary',xlab = 'Annual salary',col="grey")
lines(density(DataBase1$`Annual salary`),col="blue",lwd=2)

hist(DataBase1$`OverAll`,prob=TRUE, main='OverAll',xlab = 'OverAll',col="grey")
lines(density(DataBase1$`OverAll`),col="red",lwd=2)

hist(DataBase1$`AGPG`,prob=TRUE, main='AGPG',xlab = 'AGPG',col="grey")
lines(density(DataBase1$`AGPG`),col="green",lwd=2)

hist(DataBase1$`PS`,prob=TRUE, main='PS',xlab = 'PS',col="grey")
lines(density(DataBase1$`PS`),col="orange",lwd=2)

plot(ecdf(DataBase1$`Annual salary`))
lines(ecdf(DataBase1$`Annual salary`),col="blue",lwd=2)

plot(ecdf(DataBase1$`OverAll`))
lines(ecdf(DataBase1$`OverAll`),col="red",lwd=2)

plot(ecdf(DataBase1$`AGPG`))
lines(ecdf(DataBase1$`AGPG`),col="green",lwd=2)

plot(ecdf(DataBase1$`PS`))
lines(ecdf(DataBase1$`PS`),col="orange",lwd=2)



bp<-boxplot(DataBase1$App, main='App',outline = FALSE)
bp<-boxplot(DataBase1$Age, main='Age',outline = FALSE)
bp<-boxplot(DataBase1$AGPG, main='AGPG',outline = FALSE)
bp<-boxplot(DataBase1$AAPG, main='AAPG',outline = FALSE)
bp<-boxplot(DataBase1$PS, main='PS',outline = FALSE)
bp<-boxplot(DataBase1$Disp, main='Disp',outline = FALSE)
bp<-boxplot(DataBase1$OverAll, main='OverAll',outline = FALSE)
bp<-boxplot(DataBase1$`Annual salary`, main='Annual salary',outline = FALSE)


plot(DATABASE_NumericOnly)
plot(x=DataBase1$`Annual salary`,y=DataBase1$`Age`,xlab="Annual salary",ylab="Age")

#--------part2---------------

AGPG <- DataBase$AGPG

AAPG <- DataBase$AAPG

PS <- DataBase$PS

Disp <- DataBase$Disp

OverAll <- DataBase$OverAll

Age<- DataBase$Age

App<-DataBase$App

Current_team<-DataBase$`Current team`

National_team<-DataBase$`National team`

Role<-DataBase$Role

Annual_salary <- DataBase$`Annual salary`


cor(Annual_salary, AGPG, method = c("pearson"))
cor(Annual_salary, AAPG, method = c("pearson"))
cor(Annual_salary, PS, method = c("pearson"))
cor(Annual_salary, Age, method = c("pearson"))
cor(Annual_salary, Disp, method = c("pearson"))
cor(Annual_salary, OverAll, method = c("pearson"))
cor(Annual_salary, App, method = c("pearson"))
cor(Annual_salary, National_team, method = c("pearson"))


par(mfrow=c(2,2))
plot(x=DataBase$AGPG,y=DataBase$`Annual salary`,col = "deeppink", main = 'AGPG - Annual salary', xlab='AGPG', ylab='Annual salary')
linearmodel2<-lm(Annual_salary ~ AGPG, data = DataBase)
abline(linearmodel2)

plot(x=DataBase$AAPG,y=DataBase$`Annual salary`,col= "deeppink", main = 'AAPG - Annual salary', xlab='AAPG', ylab='nnual salary')
linearmodel2<-lm(Annual_salary  ~ AAPG, data = DataBase)
abline(linearmodel2)

plot(x=DataBase$PS,y=DataBase$`Annual salary`,col=  "deeppink", main = 'PS - Annual salary',xlab='PS', ylab='nnual salary')
linearmodel2<-lm(Annual_salary  ~ PS, data = DataBase)
abline(linearmodel2)

plot(x=DataBase$Disp,y=DataBase$`Annual salary`,col=  "deeppink", main = 'Disp - Annual salary',xlab='Disp', ylab='nnual salary')
linearmodel2<-lm(Annual_salary  ~ Disp, data = DataBase)
abline(linearmodel2)

plot(x=DataBase$OverAll,y=DataBase$`Annual salary`,col=  "deeppink", main = 'OverAll - Annual salary',xlab='OverAll', ylab='nnual salary')
linearmodel2<-lm(Annual_salary  ~ OverAll, data = DataBase)
abline(linearmodel2)

plot(x=DataBase$Age,y=DataBase$`Annual salary`,col= "deeppink", main = 'Age - Annual salary',xlab='Age', ylab='nnual salary')
linearmodel2<-lm(Annual_salary  ~ Age, data = DataBase)
abline(linearmodel2)

plot(x=DataBase$App,y=DataBase$`Annual salary`,col= "deeppink", main = 'App - Annual salary',xlab='App', ylab='annual salary')
linearmodel2<-lm(Annual_salary  ~ App, data = DataBase)
abline(linearmodel2)

plot(x=DataBase$Role,y=DataBase$`Annual salary`,col= "deeppink", main = 'Role - Annual salary',xlab='Role', ylab='annual salary')
linearmodel2<-lm(Annual_salary  ~ Role, data = DataBase)

plot(x=DataBase$`National team`,y=DataBase$`Annual salary`,col= "deeppink", main = 'National_team - Annual salary',xlab='National_team', ylab='annual salary')
linearmodel2<-lm(Annual_salary  ~ National_team, data = DataBase)

plot(x=DataBase$`Current team`,y=DataBase$`Annual salary`,col= "deeppink", main = 'Current_team - Annual salary',xlab='Current_team', ylab='annual salary')
linearmodel2<-lm(Annual_salary  ~ Current_team, data = DataBase)


df <- data.frame(cbind(AGPG, AAPG, PS, Disp, OverAll, Age))
cor(df, method = "pearson")


#---pearson categorial---
par(mfrow=c(1,2))
plot(App,Annual_salary, col ="deeppink", ylab="Annual salary",xlab="App", main = "Annual_salary ~ App",cex =1,pch=18,frame =FALSE)
plot(National_team,Annual_salary, col ="deeppink", ylab="Annual_salary",xlab="National_team", main = "Annual_salary ~ National_team",cex =1,pch=18,frame =FALSE)
plot(Current_team,Annual_salary, col ="deeppink", ylab="Annual salary",xlab="Current_team", main = "Annual_salary ~ Current_team",cex =1,pch=18,frame =FALSE)
plot(Role,Annual_salary, col ="deeppink", ylab="Annual_salary",xlab="Role", main = "Annual_salary ~ Role",cex =1,pch=18,frame =FALSE)

fit <- lm(Annual_salary ~ AGPG + AAPG + PS+Disp+OverAll+Age+App, data=DataBase)
summary(fit)
AIC(fit)
BIC(fit)

fit1 <- lm(Annual_salary ~ AGPG  + PS+OverAll+Disp+Age, data=DataBase)#no aapg
summary(fit1)
AIC(fit1)
BIC(fit1)

fit2 <- lm(Annual_salary ~ AGPG  + PS+OverAll+Age, data=DataBase)#no aapg+disp
summary(fit2)
AIC(fit2)
BIC(fit2)

fit3 <- lm(Annual_salary ~ AGPG  + PS+OverAll+Age, data=DataBase)#no aapg+disp
summary(fit3)
AIC(fit3)
BIC(fit3)

fit4 <- lm(Annual_salary ~ AGPG + PS+OverAll+Age, data=DataBase)
summary(fit4)
AIC(fit4)
BIC(fit4)
#---Checkinig interactions---

#Goal-CurrentTeam-Annual Salary
  mod<-lm(DataBase$`Annual salary` ~ DataBase$AGPG*DataBase$`Current team`)
  summary(mod)
  plot(DataBase$AGPG[DataBase$`Current team`=='1'] ,DataBase$`Annual salary`[DataBase$`Current team`=='1'],
       col="blue",xlim = c(0.00,1.5),ylim=c(0,35) ,xlab="AGPG",ylab="Annual Salary", main = "Annual Salary vs. AGPG")
  points(DataBase$AGPG[DataBase$`Current team`=='2'] ,DataBase$`Annual salary`[DataBase$`Current team`=='2'], col="green")
 
  legend(980,50,legend = c("1","2","3"),col=c("blue","green", "pink"),pch=c(1,1),bty="n")
  linearmodel1 <- lm(DataBase$`Annual salary`[DataBase$`Current team`=='1'] ~DataBase$AGPG[DataBase$`Current team`=='1'])
  linearmodel2 <- lm(DataBase$`Annual salary`[DataBase$`Current team`=='2'] ~DataBase$AGPG[DataBase$`Current team`=='2'])

  abline(linearmodel1, col="blue")
  abline(linearmodel2, col="green")

#AAPG-$`Current team`-Annual Salary
mod<-lm(DataBase$`Annual salary` ~ DataBase$AAPG*DataBase$`Current team`)
summary(mod)
plot(DataBase$AAPG[DataBase$`Current team`=='1'] ,DataBase$`Annual salary`[DataBase$`Current team`=='1'],
     col="blue",xlim = c(0.00,0.9),ylim=c(0,30) ,xlab="AAPG",ylab="Annual Salary", main = "Annual Salary vs. AAPG")
points(DataBase$AAPG[DataBase$`Current team`=='2'] ,DataBase$`Annual salary`[DataBase$`Current team`=='2'], col="green")

legend(980,50,legend = c("1","2","3"),col=c("blue","green", "pink"),pch=c(1,1),bty="n")
linearmodel1 <- lm(DataBase$`Annual salary`[DataBase$`Current team`=='1'] ~DataBase$AAPG[DataBase$`Current team`=='1'])
linearmodel2 <- lm(DataBase$`Annual salary`[DataBase$`Current team`=='2'] ~DataBase$AAPG[DataBase$`Current team`=='2'])

abline(linearmodel1, col="blue")
abline(linearmodel2, col="green")

#OverAll-$`Current team`-Annual Salary
mod<-lm(DataBase$`Annual salary` ~ DataBase$OverAll*DataBase$`Current team`)
summary(mod)
plot(DataBase$OverAll[DataBase$`Current team`=='1'] ,DataBase$`Annual salary`[DataBase$`Current team`=='1'], col="blue",xlim = c(7.19,8),ylim=c(0,30) ,xlab="OverAll",ylab="Annual Salary", main = "Annual Salary vs. OverAll")
points(DataBase$OverAll[DataBase$`Current team`=='2'] ,DataBase$`Annual salary`[DataBase$`Current team`=='2'],col="green")

legend(980,50,legend = c("1","2","3"),col=c("blue","green", "pink"),pch=c(1,1),bty="n")
linearmodel1 <- lm(DataBase$`Annual salary`[DataBase$`Current team`=='1'] ~DataBase$OverAll[DataBase$`Current team`=='1'])
linearmodel2 <- lm(DataBase$`Annual salary`[DataBase$`Current team`=='2'] ~DataBase$OverAll[DataBase$`Current team`=='2'])

abline(linearmodel1, col="blue")
abline(linearmodel2, col="green")


#Age-$`Current team`-Annual Salary
mod<-lm(DataBase$`Annual salary` ~ DataBase$Age*DataBase$`Current team`)
summary(mod)
plot(DataBase$Age[DataBase$`Current team`=='1'] ,DataBase$`Annual salary`[DataBase$`Current team`=='1'],
       col="blue",xlim = c(17,40),ylim=c(0,30) ,xlab="Age",ylab="Annual Salary", main = "Annual Salary vs. Age")
points(DataBase$Age[DataBase$`Current team`=='2'] ,DataBase$`Annual salary`[DataBase$`Current team`=='2'],
       col="green")

legend(980,50,legend = c("1","2","3"),col=c("blue","green", "pink"),pch=c(1,1),bty="n")
linearmodel1 <- lm(DataBase$`Annual salary`[DataBase$`Current team`=='1'] ~DataBase$Age[DataBase$`Current team`=='1'])
linearmodel2 <- lm(DataBase$`Annual salary`[DataBase$`Current team`=='2'] ~DataBase$Age[DataBase$`Current team`=='2'])

abline(linearmodel1, col="blue")
abline(linearmodel2, col="green")



#---Full model---:
x1 <- DataBase$AGPG     

x2 <- DataBase$AAPG

x3 <-  DataBase$PS

x4 <- DataBase$Disp

x5 <- DataBase$OverAll

x6 <- DataBase$Age

x7 <- DataBase$App

x8<- DataBase$Role

x9<-DataBase$`National team`

y <- DataBase$`Annual salary`

#DataBase$Role.cat<-factor(DataBase$Role,labels = c("1","2","3"))
DataBase$CurrentTeam.cat<-factor(DataBase$`Current team`,labels = c("1","2"))
#DataBase$NationalTeam.cat<-factor(DataBase$`National team`,labels = c("1","2","3","4"))

#--Full model----

model2<-lm(formula = y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x5*DataBase$`CurrentTeam.cat`+x6*DataBase$`CurrentTeam.cat`)
summary(model2)
AIC(model2)
BIC(model2)

model_New<-lm(formula = y~x1+x2+x5+x6+x7+x8+x9
              +x1*DataBase$`CurrentTeam.cat`+x5*DataBase$`CurrentTeam.cat`+x6*DataBase$`CurrentTeam.cat`) #without Ps,Disp
summary(model_New)

#------------------------Forward--------------------------

lm.null<- lm(y~1,data=DataBase)
model.aic.forward<-step(lm.null,direction = "forward", trace = 1,scope = list(upper=model2),data=DataBase)
summary(model.aic.forward)
AIC(model.aic.forward)

lm.null<- lm(y~1,data=DataBase)
model.bic.forward<-step(lm.null,direction = "forward", trace = 1,scope = list(upper=model2),data=DataBase)
summary(model.bic.forward)
BIC(model.bic.forward)


#------------------------Backword--------------------------

lm.null<- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x5*DataBase$`CurrentTeam.cat`+x6*DataBase$`CurrentTeam.cat`) #without Ps,Disp
model.aic.backward<-step(lm.null,direction = "backward", trace = 1,scope = list(upper=model2),data=DataBase)
summary(model.aic.backward)
AIC(model.aic.backward)

lm.null<- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x5*DataBase$`CurrentTeam.cat`+x6*DataBase$`CurrentTeam.cat`)
model.bic.backward<-step(lm.null,direction = "backward", trace = 1,scope = list(upper=model2),data=DataBase)
summary(model.bic.backward)
BIC(model.bic.backward)

#------------------------both--------------------------

lm.null<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x5*DataBase$`CurrentTeam.cat`+x6*DataBase$`CurrentTeam.cat`)
model.aic.both<-step(lm.null,direction = "both", trace = 1,scope = list(upper=model2),data=DataBase)
summary(model.aic.both)
AIC(model.aic.both)

lm.null<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x5*DataBase$`CurrentTeam.cat`+x6*DataBase$`CurrentTeam.cat`)
model.bic.both<-step(lm.null,direction = "both", trace = 1,scope = list(upper=model2),data=DataBase)
summary(model.bic.both)
BIC(model.bic.both)

#--------checking model assumptions-----------#
newModle<-lm(y~x5+x6+DataBase$`CurrentTeam.cat`+x5*DataBase$`CurrentTeam.cat`+x6*DataBase$`CurrentTeam.cat`)
boxcox(newModle,lambda = seq(0, 0.5, 1/10))
improvedModel<-lm(y^(0.4)~x1+x2+x3+x4+x5+x6+x7+x8+x9+x5*DataBase$`CurrentTeam.cat`+x6*DataBase$`CurrentTeam.cat`) #without Ps,Disp
summary(improvedModel)

newModle<-lm(y^(0.25)~x5+x6+DataBase$`CurrentTeam.cat`+x5*DataBase$`CurrentTeam.cat`+x6*DataBase$`CurrentTeam.cat`)
predicted<- predict(lm(newModle))
unstandardizedresiduals<- resid(newModle)
residuals<-(unstandardizedresiduals-mean(unstandardizedresiduals))/sd(unstandardizedresiduals)
plot(predicted,residuals,main="resdual-fitted", xlab="fitted" ,ylab="residuals")
abline(0,0)


newModle<-newModle<-lm(y~x5+x6+DataBase$`CurrentTeam.cat`+x5*DataBase$`CurrentTeam.cat`+x6*DataBase$`CurrentTeam.cat`)
predicted<- predict(lm(newModle))
unstandardizedresiduals<- resid(newModle)
residuals<-(unstandardizedresiduals-mean(unstandardizedresiduals))/sd(unstandardizedresiduals)
plot(predicted,residuals,main="resdual-fitted", xlab="fitted" ,ylab="residuals")
abline(0,0)
summary(newModle)



predicted<- predict(lm(newModle))
unstandardizedresiduals<- resid(newModle)
residuals<-(unstandardizedresiduals-mean(unstandardizedresiduals))/sd(unstandardizedresiduals)
mod<-lm(newModle)
DataBase$fitted<-fitted(mod)
DataBase$residuals<-residuals(mod)
s.e_res <- sqrt(var(DataBase$residuals))
DataBase$stan_residuals<-(residuals(mod)/s.e_res)
qqnorm(DataBase$stan_residuals)
abline(a=0, b=1)

h<-hist(DataBase$stan_residuals, breaks=8, col="pink", xlab ="Normalized error", main="Histogram of normalized error")
xfit<-seq(min(DataBase$stan_residuals),max(DataBase$stan_residuals),length=40)
yfit<-dnorm(xfit,mean=mean(DataBase$stan_residuals),sd=sd(DataBase$stan_residuals))
yfit <- yfit*diff(h$mids[1:2])*length(DataBase$stan_residuals)
lines(xfit, yfit, col="blue", lwd=2)

ks.test(DataBase$stan_residuals, y="pnorm") #KS test

reg <- lm(y~x5+x6+DataBase$`CurrentTeam.cat`+x5*DataBase$`CurrentTeam.cat`+x6*DataBase$`CurrentTeam.cat`)
anova(reg)


#-----Changes in the model-----

cor(Annual_salary, Age, method = c("pearson"))
cor(Annual_salary, log(Age), method = c("pearson"))
cor(Annual_salary, Age^2, method = c("pearson"))
cor(Annual_salary, exp(Age), method = c("pearson"))#########

cor(Annual_salary, OverAll, method = c("pearson"))
cor(Annual_salary, log(OverAll), method = c("pearson"))
cor(Annual_salary, OverAll^2, method = c("pearson"))
cor(Annual_salary, exp(OverAll), method = c("pearson"))#########

cor(Annual_salary, AGPG, method = c("pearson"))
cor(Annual_salary, log(AGPG), method = c("pearson"))
cor(Annual_salary, AGPG^2, method = c("pearson"))#########
cor(Annual_salary, exp(AGPG), method = c("pearson"))

cor(Annual_salary, AAPG, method = c("pearson"))
cor(Annual_salary, log(AAPG), method = c("pearson"))
cor(Annual_salary, sqrt(AAPG), method = c("pearson"))
cor(Annual_salary, exp(AAPG), method = c("pearson"))#########

cor(Annual_salary, App, method = c("pearson"))
cor(Annual_salary, log(App), method = c("pearson"))
cor(Annual_salary, App^2, method = c("pearson"))
cor(Annual_salary, exp(App), method = c("pearson"))#########

cor(Annual_salary, Disp, method = c("pearson"))
cor(Annual_salary, log(Disp), method = c("pearson"))
cor(Annual_salary, sqrt(Disp), method = c("pearson"))#########
cor(Annual_salary, exp(Disp), method = c("pearson"))

cor(Annual_salary, PS, method = c("pearson"))
cor(Annual_salary, log(PS), method = c("pearson"))
cor(Annual_salary, PS^2, method = c("pearson"))
cor(Annual_salary, exp(PS), method = c("pearson"))#########

#-----Check if Changes in the model affects-----

model2<-lm(formula = y~x1^2+x2+x3+x4+exp(x5)+exp(x6)+x7+x8+x9+exp(x5)*DataBase$`CurrentTeam.cat`+exp(x6)*DataBase$`CurrentTeam.cat`)
summary(model2)
AIC(model2)
BIC(model2)

#------------------------Forward--------------------------

lm.null<- lm(y~1,data=DataBase)
model.aic.forward<-step(lm.null,direction = "forward", trace = 1,scope = list(upper=model2),data=DataBase)
summary(model.aic.forward)
AIC(model.aic.forward)

lm.null<- lm(y~1,data=DataBase)
model.bic.forward<-step(lm.null,direction = "forward", trace = 1,scope = list(upper=model2),data=DataBase)
summary(model.bic.forward)
BIC(model.bic.forward)


#------------------------Backword--------------------------

lm.null<- lm(y~(x1^2)+x2+x3+x4+exp(x5)+exp(x6)+x7+x8+x9+exp(x5)*DataBase$`CurrentTeam.cat`+exp(x6)*DataBase$`CurrentTeam.cat`)
model.aic.backward<-step(lm.null,direction = "backward", trace = 1,scope = list(upper=model2),data=DataBase)
summary(model.aic.backward)
AIC(model.aic.backward)

lm.null<- lm(y~(x1^2)+x2+x3+x4+exp(x5)+exp(x6)+x7+x8+x9+exp(x5)*DataBase$`CurrentTeam.cat`+exp(x6)*DataBase$`CurrentTeam.cat`)
model.bic.backward<-step(lm.null,direction = "backward", trace = 1,scope = list(upper=model2),data=DataBase)
summary(model.bic.backward)
BIC(model.bic.backward)

#------------------------both--------------------------

lm.null<-lm(y~(x1^2)+x2+x3+x4+exp(x5)+exp(x6)+x7+x8+x9+exp(x5)*DataBase$`CurrentTeam.cat`+exp(x6)*DataBase$`CurrentTeam.cat`)
model.aic.both<-step(lm.null,direction = "both", trace = 1,scope = list(upper=model2),data=DataBase)
summary(model.aic.both)
AIC(model.aic.both)

lm.null<-lm(y~(x1^2)+x2+x3+x4+exp(x5)+exp(x6)+x7+x8+x9+exp(x5)*DataBase$`CurrentTeam.cat`+exp(x6)*DataBase$`CurrentTeam.cat`)
model.bic.both<-step(lm.null,direction = "both", trace = 1,scope = list(upper=model2),data=DataBase)
summary(model.bic.both)
BIC(model.bic.both)

#------------------------t s.t.--------------------------

X <- DataBaseNew
xt <- t(X)
xtx <- (as.matrix(xt) %*% as.matrix(X))
c <- as.matrix(c(1, 6.92, 34 , 0))
ct <- t(c)
mse <- (7.081)^2
t_st <- (6.24-6.543)/sqrt(mse*(1+(ct%*%solve(xtx)%*%c)))
t_cr<-qt(0.95,96)
paste(t_st)
paste(t_cr)

#------------------------Correlation matrix--------------------------

dataset<-DataNoNames
summary(dataset)
Z <- cor((dataset),method = "pearson")

