library(data.table)
library(rafalib) # makes labels in plots bigger
library(dplyr)
library(tidyr)
library(ggplot2) # allows to create custom plots by groups etc
library(interactions)
library(gridExtra)
library(mgcv)
library(tidymv)
library(xtable)
library(kde1d)
library(plotly)
library(MASS)

#b
dt <- data.table(credit.dat)
dt<-dt[,c("kredit","laufzeit", "moral", "laufkont")]

dt$moral <- as.factor(dt$moral)
dt$laufkont <- as.factor(dt$laufkont)

dt[,no_kredit := 1-kredit]

#1
dt <- as.data.table(aggregate(cbind(kredit,no_kredit) ~ laufzeit + moral + laufkont, FUN=sum, data=dt))
dt[,n_i := no_kredit + kredit]
nrow(dt)

#2
small.model.agg<-glm(cbind(kredit,no_kredit)~laufzeit + moral + laufkont, data=dt, family=binomial(link="logit"))
summary(small.model.agg)

#3
#a
results<-as.data.table(hatvalues(small.model.agg))
results[,c('hatvalues', 'V1') := list(V1, NULL)]

threshold <- 2*sum(results$hatvalues)/length(results$hatvalues)

high_leverage <- results$hatvalues[results$hatvalues>threshold] #x outliers, the regession line moves drastically
results[,high_leverage := results$hatvalues>threshold]

ggplot(results,aes(x=1:nrow(results),y=hatvalues,color=high_leverage,)) +
  geom_point() +
  geom_hline(yintercept = threshold) + 
  xlab("Observation ID") + 
  ylab("Levarage")

which(results$high_leverage == "TRUE")  

#b
#if they are removed
small.model.agg$residuals

results$resid.pearson <-residuals(small.model.agg,type="pearson")
results$resid.dev <-residuals(small.model.agg,type="deviance")
results$resid.adj <-results$resid.pearson/(1-(results$hatvalues)**2)
results$cooks.distance <-cooks.distance(small.model.agg) # outliers in both directions. Threshold for outliers is 1. no outliers in this case

plot(results$resid.dev)
plot(results$resid.pearson)
plot(results$resid.adj)
plot(results$cooks.distance)

results <- melt(results, id.vars = "hatvalues")

ggplot(results)+geom_point(aes(1:nrow(results),value))#+facet_wrap(~variable)


small.model.agg$residuals
dt[,predictions := round(predict(small.model.agg, type="response")*dt$n_i)]

#from solution

my.pres <- resid(small.model.agg, type ="pearson")
range.pres<-max(abs(my.pres))

my.dev <- resid(small.model.agg, type ="deviance")
range.deviance<-max(abs(my.dev))

my.adjusted <- my.pres/(sqrt(1-hatvalues(small.model.agg)))
range.adjusted <-max(abs(my.adjusted))

plot(my.pres, pch = 16,ylim=c(-range.pres,range.pres))

plot(1:length(dt$predictions),dt$predictions-dt$kredit)

#ungroupped data residuals:
small.model <- glm(credit.dat$kredit ~ as.factor(credit.dat$moral) + as.factor(credit.dat$laufkont) + credit.dat$laufzeit,
                   family = binomial(link = "logit"), data = credit.dat)

credit.dat <- as.data.table(credit.dat)
credit.dat[,predictions := (predict(small.model, type = "response"))]
credit.dat[,pearson_resid :=(kredit - predictions)/sqrt(predictions*(1-predictions))]

plot(1:length(credit.dat$pearson_resid),credit.dat$pearson_resid)

my.pear.ung <- resid(small.model, type = "pearson")
my.pear.dev <- resid(small.model, type = "deviance")
my.pear.adj <- my.pear.ung/(sqrt(1-(hatvalues(small.model))^2))

plot(1:length(credit.dat$pearson_resid),my.pear.ung)
plot(1:length(credit.dat$pearson_resid),my.pear.dev)
plot(1:length(credit.dat$pearson_resid),my.pear.adj)

plot(small.model$fitted.values,my.pear.ung)


#Problem 2 
credit=credit.dat
credit=credit[,c(1,3,4,2,11,14)]
attach(credit)

moralF=as.factor(moral)
laufkontF=as.factor(laufkont)

credit2=data.frame(kredit=kredit,laufzeit=laufzeit,moral=moralF,laufkont=laufkontF,alter=alter)

model1=glm(kredit~laufzeit + moral + laufkont+alter,family=binomial(link = "logit"),
           data=credit2)

phat=predict(model1,type="response")
r.raw <- kredit - (phat)
my.var<-phat * (1 - phat)

coef(model1)

beta.laufzeit <- coef(model1)[2]
r.partial.laufzeit <- r.raw/(my.var) + beta.laufzeit * laufzeit
library(DescTools)

ry.laufzeit=range(Trim(r.partial.laufzeit,trim=0.05))

plot(laufzeit,r.partial.laufzeit,pch=15,ylab="partial residuals",ylim=ry.laufzeit)
ols.laufzeit=lm(r.partial.laufzeit~laufzeit)$coefficients # is there a trend in the residuals?
abline(ols.laufzeit,lwd=4,col="blue")
lines(lowess(laufzeit,r.partial.laufzeit),col="red",lwd=4) # Scatter Plot Smoothing

#Problem 3
#a
credit=credit.dat

credit$moral=as.factor(moral)
credit$laufkont=as.factor(laufkont)
credit$buerge = as.factor(credit$buerge)


#b
set.seed(234)

#c
train = sample(1000, 700)

#d
model1 = glm(kredit ~ laufzeit + moral + laufkont + buerge + alter, data = credit,
             family = binomial(link = "logit"), subset = train)

summary(model1)

as.data.table(credit)[train]

predictions_train = ifelse(predict(model1, as.data.table(credit)[train])>0.5,1,0)
cm_train <- table(as.data.table(credit)$kredit[train],predictions_train)
100*(cm_train[1,2]+cm_train[2,1])/sum(cm_train)

predictions_test = ifelse(predict(model1, as.data.table(credit)[-train])>0.5,1,0)
cm_test <- table(as.data.table(credit)$kredit[-train],predictions_test)
100*(cm_test[1,2]+cm_test[2,1])/sum(cm_test)
