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
dt<-dt[,c("kredit","laufzeit", "moral", "laufkont", "alter", "beruf")]


#kredit - dichotomous, categorical 2 levels

#moral - ordinal
#laufkont - ordinal,  >2 levels
#alter -  numerical/quantitative
#laufzeit - numerical/quantitative
#beruf - ordinal

dt[,no_kredit := 1-kredit]
dt <- as.data.table(aggregate(cbind(kredit,no_kredit) ~ laufzeit + moral + laufkont + alter + beruf, FUN=sum, data=dt))

dt[, ni := as.numeric(no_kredit) + as.numeric(kredit)]

dt$moral <- as.factor(dt$moral)
dt$laufkont <- as.factor(dt$laufkont)
dt$beruf <- as.factor(dt$beruf)

#b
#i)
credit.dat.dt <- data.table(credit.dat)
credit.dat.dt[kredit == 1,.N]
table(credit.dat.dt$kredit)

#ii)
dt$laufzeit
r.gam.eda.laufzeit<-gam(cbind(kredit,no_kredit)~s(laufzeit), data=dt, family=binomial)
r.gam.eda.alter<-gam(cbind(kredit,no_kredit)~s(alter), data=dt, family=binomial(link=logit))

plot(r.gam.eda.laufzeit, se=T)
plot(r.gam.eda.alter, se=T)

fit<-glm(cbind(kredit,no_kredit)~laufkont,data=dt,
         family=binomial)

p1<-cat_plot(fit,pred=laufkont,data=dt,
             outcome.scale = "link",geom = "line",y.label = "logit")
p1

fit2<-glm(cbind(kredit,no_kredit)~moral,data=dt,
         family=binomial)
p2<-cat_plot(fit2,pred=moral,data=dt,
             outcome.scale = "link",geom = "line",y.label = "logit")
p2

fit3<-glm(cbind(kredit,no_kredit)~beruf,data=dt,
          family=binomial)
p3<-cat_plot(fit3,pred=beruf,data=dt,
             outcome.scale = "link",geom = "line",y.label = "logit")
p3
levels(dt$moral) <- list("01" = c("0","1"),"23"=c("2","3"),"4"=c("4"))
levels(dt$beruf) <- list("1"=c("1"),"23"=c("2","3"),"4"=c("4"))

#c
null_model <- glm(cbind(kredit,no_kredit) ~ 1,family=binomial,data=dt)
full_model<-glm(cbind(kredit,no_kredit)~laufzeit + moral + laufkont + poly(alter,2)+beruf,data=dt,
         family=binomial)

forwards = step(null_model,
                scope=list(lower=formula(null_model),upper=formula(full_model)), 
                direction="forward")

formula(forwards)

my.glm.step.main <- glm(formula(forwards), 
                        family=binomial,data=dt)

summary(my.glm.step.main)

#d
sum<-anova(my.glm.step.main, test="Chisq")
sum

pchisq((sum$`Resid. Dev`[4] - sum$`Resid. Dev`[5]), 
       df = 2,
       lower.tail = F)


#e
my.glm.step.main$deviance
sat_model <- glm(cbind(kredit,no_kredit) ~ as.factor(1:nrow(dt)),family=binomial,data=dt)
2*(logLik(sat_model) - logLik(my.glm.step.main)) # Residual deviance


qchisq(0.05,912-9,lower.tail = F)

length(my.glm.step.main$coefficients)

pchisq((sum$`Resid. Dev`[4] - sum$`Resid. Dev`[5]), 
       df = 2,
       lower.tail = F)



#f
#model.inter = step(my.glm.step.main, .^2, direction = "forward")

fit4 <- glm(cbind(kredit, no_kredit) ~ laufzeit*laufkont, data = dt, family = binomial(link="logit"))
interact_plot(fit4,
              pred = laufzeit, 
              modx = laufkont, 
              interval = T,
              outcome.scale = "link",
              data=dt,
              y.label = "logit")

fit5 <- glm(cbind(kredit, no_kredit) ~ laufzeit*moral, data = dt, family = binomial(link="logit"))
interact_plot(fit5,
              pred = laufzeit, 
              modx = moral, 
              interval = T,
              outcome.scale = "link",
              data=dt,
              y.label = "logit")


fit6 <- glm(cbind(kredit, no_kredit) ~ alter*laufkont, data = dt, family = binomial(link="logit"))
interact_plot(fit6,
              pred = alter, 
              modx = laufkont, 
              interval = T,
              outcome.scale = "response",
              data=dt,
              y.label = "logit")

fit7 <- glm(cbind(kredit, no_kredit) ~ alter*moral, data = dt, family = binomial(link="logit"))
interact_plot(fit7,
              pred = alter, 
              modx = moral, 
              interval = T,
              outcome.scale = "response",
              data=dt,
              y.label = "logit")



fit6<- glm(cbind(kredit, no_kredit) ~ laufkont*moral, data = dt, family = binomial(link="logit"))
cat_plot(fit6,
              pred = laufkont, 
              modx = moral, 
              interval = T,
              outcome.scale = "link",
              data=dt,
              y.label = "logit",
              geom="line")

alter_group<- cut(dt$alter,
    breaks = quantile(dt$alter, probs =c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = T))

laufzeit_group<- cut(dt$laufzeit,
                  breaks = quantile(dt$laufzeit, probs =c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = T))

dt$alter_group = alter_group
dt$laufzeit_group = laufzeit_group

fit7 <- glm(cbind(kredit, no_kredit) ~ alter_group*laufzeit_group, data = dt, family = binomial)
cat_plot(fit7,
         pred = alter_group, 
         modx = laufzeit_group, 
         interval = T,
         outcome.scale = "link",
         data=dt,
         y.label = "logit",
         geom="line")

full_interact_model <- glm(cbind(kredit, no_kredit) ~ (laufkont + laufzeit + moral + poly(alter,2)) * (laufkont + laufzeit + moral + poly(alter,2)) , data = dt, family = binomial)

my.glm.step.main.interact <- step(null_model,
                                  scope = list(lower=formula(null_model),upper=formula(full_interact_model)),
                                  direction = "forward",k=2)


#moral - ordinal
#laufkont - ordinal,  >2 levels
#alter -  numerical/quantitative
#laufzeit - numerical/quantitative


# laufzeit Vs. moral and  moral Vs. alter

