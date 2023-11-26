library(interactions) 

#Problem 1
#a)

#a
#Kilometres - polytomous (5 levels) and ordinal 
#Zone - polytomous (7 levels) and categorical
#Bonus - numerical/quantitative
#Make - polytomous (8 levels) and categorical

dt <- insurance.dat
dt$Insured #numerical
dt$Claims #numerical
dt$Payment #numerical

dt$Zone = as.factor(dt$Zone)
dt$Kilometres = as.factor(dt$Kilometres)
dt$Make = as.factor(dt$Make)
dt$Bonus = as.numeric(dt$Bonus)
dt$BonusF = as.factor(dt$Bonus)

dt$BonusF2<- cut(dt$Bonus,
                     breaks = quantile(dt$Bonus, probs =c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = T))


#b)

#2
fit1<-glm(Claims~Zone,data=dt,family=poisson, offset = log(Insured))
p1<-cat_plot(fit1,pred=Zone,data=dt,outcome.scale = "link",geom = "line",y.label = "empirical logit mean")

fit2<-glm(Claims~Kilometres,data=dt,family=poisson, offset = log(Insured))
p2<-cat_plot(fit2,pred=Kilometres,data=dt,outcome.scale = "link",geom = "line",y.label = "empirical logit mean")

fit3<-glm(Claims~Make,data=dt,family=poisson, offset = log(Insured))
p3<-cat_plot(fit3,pred=Make,data=dt,outcome.scale = "link",geom = "line",y.label = "empirical logit mean")

fit4<-glm(Claims~BonusF2,data=dt,family=poisson, offset = log(Insured))
p4<-cat_plot(fit4,pred=BonusF2,data=dt,outcome.scale = "link",geom = "line",y.label = "empirical logit mean")

p1
p2
p3
p4

#if they are ordinal maybe only merge neighbor categories to not lose interpretability 


#levels(dt$Zone) <- list("1"= c("1"),"2"= c("2"), "3_5"=c("3","5"),"4_6"=c("4","6"), "7" = c("7"))
#levels(dt$Kilometres) <- list("1" = c("1"), "2_3_4" = c("2","3","4"), "5" = c("5"))
#levels(dt$Make) <- list("1" = c("1"),
#                        "7_8"=c("7","8"),
#                        "9" = c("9"),
#                        "2_5" = c("2","5"),
#                        "3_6" = c("3","6"), 
#                        "4"=c("4")
#                        )

#levels(dt$Make) <- list("1" = c("1"),
#                        "7_8"=c("7","8"),
#                        "9" = c("9"),
#                        "2" = c("2"),
#                        "3" = c("3"), 
#                        "4"=c("4"),
#                        "5" = c("5"),
#                        "6" = c("6")
#)

levels(dt$Make) <- list("1" = c("1"),
                        "2" = c("2"),
                        "3" = c("3"),
                        "4" = c("4"),
                        "5" = c("5"),
                        "6" = c("6"),
                        "7" = c("7","8"),
                        "8" = c("9")
                        )

fit1<-glm(Claims~Zone,data=dt,family=poisson, offset = log(Insured))
p1<-cat_plot(fit1,pred=Zone,data=dt,outcome.scale = "link",geom = "line",y.label = "empirical logit mean")

fit2<-glm(Claims~Kilometres,data=dt,family=poisson, offset = log(Insured))
p2<-cat_plot(fit2,pred=Kilometres,data=dt,outcome.scale = "link",geom = "line",y.label = "empirical logit mean")

fit3<-glm(Claims~Make,data=dt,family=poisson, offset = log(Insured))
p3<-cat_plot(fit3,pred=Make,data=dt,outcome.scale = "link",geom = "line",y.label = "empirical logit mean")
p3

#c
fit_full<-glm(Claims ~ Zone + Kilometres + Bonus + Make ,
              data=dt,
              family=poisson, 
              offset = log(Insured))

fit_null<-glm(Claims ~ 1,
              data=dt,
              family=poisson,
              offset = log(Insured))

model.main <- step(fit_null, direction = "forward", scope =list(upper=fit_full,lower=fit_null), k = 2)
summary(model.main)

#d
#all main effects are significant at a 5% significance level because for the Wald tests performed (can be seen in summary of the model)
#we get p-values that are lower 0.05 (5%), i.e, at 5% we reject H0 the states that the individual coefficient of the test is 0-valued.
#note: they are significant covariates, but we performed dummy encoding. Using scores (for ordinal covariates or numerical variable) could lead to different results

#e
anova(model.main,test = "Chisq")
#We are sequentially performing partial deviance tests. In total we make 4 tests. For all of them we obtain low p-values, which means
#we reject the null hypothesis (H0) at 5% significance level. Therefore, all coefficients (which are the main effects) are significant 
#and should be included in the model. The test formal definition is... . The covarties were added in the following order: Bonus, Zone, Kilometers, Make.
#Note, each covariate has a different number of levels, therefore we are adding k-1 parameters of the model where k is the number of levels of
#each variable. We add k-1 instead o k to avoid a non full-rank design matrix, which would lead to invertibility problems

#f
#Bonus - 4 
#Zone - 6
#Kilometers - 2
#Make - 0

ti <- 100
x=c(1,    4,   0,0,0,0,0,1,    0,1,0,0,   0,0,0,0,0,0,0)

exp(model.main$coefficients %*% x + log(ti))


#alternative
new_obs <- data.frame(Bonus = 4, Zone = as.factor(7), Kilometres = as.factor(3), Make = as.factor(1), Insured = 100)
expected <- predict(model.main,newdata = new_obs, type = "response")
expected

#Problem 2
#a)

credit.dat <- as.data.table(credit.dat[,c("moral","laufkont","laufzeit","kredit")])
str(credit.dat)

credit.dat[,moral:=as.factor(moral)]
credit.dat[,laufkont:=as.factor(laufkont)]

credit.dat.agg <- copy(credit.dat)
credit.dat.agg[,no_kredit := 1-kredit]
credit.dat.agg <- aggregate(cbind(kredit,no_kredit) ~ moral + laufkont + laufzeit, FUN = sum, data = credit.dat.agg)

small.model.agg <- glm(cbind(kredit,no_kredit) ~ moral + laufkont + laufzeit, family = binomial(),data =credit.dat.agg)
summary(small.model.agg) #Residual Dev > n-p which indicates bad fit and potentially  overdispersion (but it may be for other reasons)

#Residual Deviance Test:
pchisq(small.model.agg$deviance,small.model.agg$df.residual,lower.tail = F)
# P-value < alfa = 0.05 => Reject H0 of model assumptions being satisfied. Therefore we reject this model

#b)
small.model <- glm(cbind(kredit) ~ moral + laufkont + laufzeit, family = binomial(),data =credit.dat)
summary(small.model) #Residual Dev > n-p which indicates bad fit and potentially  overdispersion (but it may be for other reasons)

#Residual Deviance Test:
pchisq(small.model$deviance,small.model$df.residual,lower.tail = F)
# P-value > alfa = 0.05 => Don't Reject H0 of model assumptions being satisfied. Therefore we consider this model valid

