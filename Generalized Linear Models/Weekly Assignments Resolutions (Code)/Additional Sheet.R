library(rafalib) # makes labels in plots bigger
library(dplyr)
library(tidyr)
library(ggplot2) # allows to create custom plots by groups etc
library(interactions)
library(gridExtra)
library(mgcv) # fits GAM models
library(tidymv)
library(xtable)
library(kde1d)
library(plotly)
library(MASS)
library(data.table)
library(jtools)

setwd("C:/Users/ricar/OneDrive/Ambiente de Trabalho/Erasmus/GLM/Datasets/")
dt<-fread("atemwege.asc.txt")

#zone - ordinal
#aller - categorical
#kehle - categorical
#schnu - categorical
#huste - categorical
#sex - categorical  
#gewi - continuous

dt<-dt[,c("zone","aller","kehle","schnu","huste","sex","gewi","lubro")]
dt <- dt[gewi>0] # Gewicht = -1 -> remove
dt[,no_lubro := 1-lubro]

dt <- aggregate(cbind(lubro,no_lubro) ~ zone + aller + kehle + schnu + huste + sex + gewi, FUN=sum, data=dt)
dt <- as.data.table(dt)

dt[,zone := as.factor(zone)]
dt[,aller := as.factor(aller)]
dt[,kehle := as.factor(kehle)]
dt[,schnu := as.factor(schnu)]
dt[,huste := as.factor(huste)]
dt[,sex := as.factor(sex)]

####### EDA #######

fit1 <- glm(cbind(lubro, no_lubro) ~ zone, family = binomial(link="logit"), data = dt)
fit2 <- glm(cbind(lubro, no_lubro) ~ aller, family = binomial(link="logit"), data = dt)
fit3 <- glm(cbind(lubro, no_lubro) ~ kehle, family = binomial(link="logit"), data = dt)
fit4 <- glm(cbind(lubro, no_lubro) ~ schnu, family = binomial(link="logit"), data = dt)
fit5 <- glm(cbind(lubro, no_lubro) ~ huste, family = binomial(link="logit"), data = dt)
fit6 <- glm(cbind(lubro, no_lubro) ~ sex, family = binomial(link="logit"), data = dt)
fit7 <- glm(cbind(lubro, no_lubro) ~ gewi, family = binomial(link="logit"), data = dt)


p1 <- cat_plot(fit1, pred = zone, data = dt, outcome.scale = "link", geom = "line",y.label = "logit")
p1

p2 <- cat_plot(fit2, pred = aller, data = dt, outcome.scale = "link", geom = "line",y.label = "logit")
p2

p3 <- cat_plot(fit3, pred = kehle, data = dt, outcome.scale = "link", geom = "line",y.label = "logit")
p3

p4 <- cat_plot(fit4, pred = schnu, data = dt, outcome.scale = "link", geom = "line",y.label = "logit")
p4

p5 <- cat_plot(fit5, pred = huste, data = dt, outcome.scale = "link", geom = "line",y.label = "logit")
p5

p6 <- cat_plot(fit6, pred = sex, data = dt, outcome.scale = "link", geom = "line",y.label = "logit")
p6

p7<-gam(cbind(lubro, no_lubro)~s(gewi), data=dt, family=binomial(link=logit))
plot(p7, se=T)

######### Fitting with step functionm, no interactions ######### 
null_model <- glm(cbind(lubro, no_lubro) ~ 1, family = binomial(link="logit"), data = dt)
full_model <- glm(cbind(lubro, no_lubro) ~ zone + aller + kehle + schnu + huste + sex + gewi, family = binomial(link="logit"), data = dt)

step_no_interacts <- step(null_model, scope = list(lower = null_model, upper = full_model),direction = "forward",k = 2)
#sex variable, by the plots seemed to be unusefull for the outcome prediction, since the logit values for both sexes were equal, this variable cconveys no useful
#information for the prediction we r interested in. zone variable was also removed

anova(step_no_interacts,test = "Chisq") #all significative main effects
summary(step_no_interacts) #and wald tests say the same

fit8 <- glm(cbind(lubro, no_lubro) ~ huste*kehle, family = binomial(link="logit"), data = dt)
fit9 <- glm(cbind(lubro, no_lubro) ~ huste*schnu, family = binomial(link="logit"), data = dt)
fit10 <- glm(cbind(lubro, no_lubro) ~ huste*aller, family = binomial(link="logit"), data = dt)
fit11 <- glm(cbind(lubro, no_lubro) ~ huste*gewi, family = binomial(link="logit"), data = dt)

fit12 <- glm(cbind(lubro, no_lubro) ~ kehle*schnu, family = binomial(link="logit"), data = dt)
fit13 <- glm(cbind(lubro, no_lubro) ~ kehle*aller, family = binomial(link="logit"), data = dt)
fit14 <- glm(cbind(lubro, no_lubro) ~ kehle*gewi, family = binomial(link="logit"), data = dt)

fit15 <- glm(cbind(lubro, no_lubro) ~ schnu*aller, family = binomial(link="logit"), data = dt)
fit16 <- glm(cbind(lubro, no_lubro) ~ schnu*gewi, family = binomial(link="logit"), data = dt)

fit17 <- glm(cbind(lubro, no_lubro) ~ aller*gewi, family = binomial(link="logit"), data = dt) 

p8 <- cat_plot(fit8, pred = huste, modx = kehle,  data = dt, outcome.scale = "link", geom = "line",y.label = "logit")
p9 <- cat_plot(fit9, pred = huste, modx = schnu,  data = dt, outcome.scale = "link", geom = "line",y.label = "logit")
p10 <- cat_plot(fit10, pred = huste, modx = aller,  data = dt, outcome.scale = "link", geom = "line",y.label = "logit")
p11 <- interact_plot(fit11, pred = gewi, modx = huste,  data = dt, outcome.scale = "link", geom = "line",y.label = "logit",interval = T)
p12 <- cat_plot(fit12, pred = kehle, modx = schnu,  data = dt, outcome.scale = "link", geom = "line",y.label = "logit")
p13 <- cat_plot(fit13, pred = kehle, modx = aller,  data = dt, outcome.scale = "link", geom = "line",y.label = "logit")
p14 <- interact_plot(fit14, pred = gewi, modx = kehle,  data = dt, outcome.scale = "link", geom = "line",y.label = "logit",interval = T)
p15 <- cat_plot(fit15, pred = schnu, modx = aller,  data = dt, outcome.scale = "link", geom = "line",y.label = "logit")
p16 <- interact_plot(fit16, pred = gewi, modx = schnu,  data = dt, outcome.scale = "link", geom = "line",y.label = "logit",interval = T)
p17 <- interact_plot(fit17, pred = gewi, modx = aller,  data = dt, outcome.scale = "link", geom = "line",y.label = "logit",interval = T)


p9 # (hsute*schnu) does not seem significant
p10 # (hsute*aller) does not seem significant
p12 # (kehle*schnu) does not seem significant
p13 # (kehle*aller) does not seem significant
p15 # (schnu*aller) does not seem significant
p17 # (gewi*aller) does not seem significant

p8 # (hsute*kehle) seems significant - V
p11 # (hsute*gewi) seems significant - V
p14 # (gewi*kehle) seems significant - V

p16 # (gewi*schnu) seems significant but small effect


######### Fitting with step functionm, with interactions ######### 
starting_model <- step_no_interacts 
full_interactions_model <- glm(cbind(lubro,no_lubro) ~ (aller + kehle + schnu + huste + gewi)*( aller + kehle + schnu + huste + gewi),data=dt, family = binomial(link="logit"))
summary(full_interactions_model)

step_interacts <- step(starting_model, scope = list(lower = starting_model, upper = full_interactions_model),direction = "forward",k = 2)
anova(step_interacts,test="Chisq")
summary(step_interacts)

step_reduced_interacts <- glm(cbind(lubro,no_lubro) ~ (aller + kehle + schnu + huste + gewi) + (huste:gewi + kehle:gewi),data=dt, family = binomial(link="logit"))

#possible models
  #1) step_no_interacts
  #2) step_interacts
  #3) step_reduced_interacts

final_models_table <- data.table(Number_of_Params = c(length(step_no_interacts$coefficients),length(step_interacts$coefficients),length(step_reduced_interacts$coefficients)),
                                 Deviance = c((step_no_interacts$deviance),(step_interacts$deviance),(step_reduced_interacts$deviance)),
                                 AIC = c((step_no_interacts$aic),(step_interacts$aic),(step_reduced_interacts$aic)),
                                 Deviance_PVal = c(pchisq(step_no_interacts$deviance,step_no_interacts$df.residual,lower.tail = F),pchisq(step_interacts$deviance,step_interacts$df.residual,lower.tail = F),pchisq(step_reduced_interacts$deviance,step_reduced_interacts$df.residual,lower.tail = F))
                                 )
final_models_table



#more complex mmodels have smaller AIC and Deviance but coefficients are all similar. We could see if the complex models are also capable of generalizing better
#so we could repeate the whole process on a smaller dataset and test the model on the remaining data

######### Residual Analysis #########
residuals(step_no_interacts, type = "pearson")

table_temp <- data.table(cbind(hatvalues(step_no_interacts),hatvalues(step_interacts),hatvalues(step_reduced_interacts)))

X_temp <- model.matrix(step_no_interacts)
W_temp <- diag(step_no_interacts$weights)
H_temp <- (sqrt(W_temp) %*% X_temp) %*% inv(t(X_temp) %*% W_temp %*% X_temp) %*% t(X_temp) %*% sqrt(W_temp)
diag(H_temp)
table_temp$new <- diag(H_temp)

leverage_points <- hatvalues(step_interacts)
high_leverage_points <- leverage_points[leverage_points>2*sum(leverage_points)/length(leverage_points)]
leverage_dt <- data.table(leverage = leverage_points,
                          high_leverage_points = ifelse(leverage_points>2*sum(leverage_points)/length(leverage_points),"High Leverage","Not High Leverage"))

ggplot(leverage_dt,aes(x=1:nrow(leverage_dt),y=leverage,color = high_leverage_points))+geom_point()

pearson_resid <- residuals(step_interacts,type = "pearson") 
deviance_resid <- residuals(step_interacts,type = "deviance") 
adj_resid <- pearson_resid/sqrt(1-leverage_points)

residuals_dt <- data.table(pearson_resid = pearson_resid,
                           deviance_resid = deviance_resid,
                           adj_resid = adj_resid)

ggplot(residuals_dt, aes(x = 1:nrow(residuals_dt), y = pearson_resid))+geom_point()
ggplot(residuals_dt, aes(x = 1:nrow(residuals_dt), y = deviance_resid))+geom_point()
ggplot(residuals_dt, aes(x = 1:nrow(residuals_dt), y = adj_resid))+geom_point() 
#reidiausl scattered around 0 with no patterns. Slightly skewd which is something that happens in GLMs. Show good fit

cooks.distance(step_interacts)

#no influential observations
ggplot()+geom_point(aes(x=1:nrow(residuals_dt),y=cooks.distance(step_interacts)))+geom_point()

install.packages("AER")
library(AER)
dispersiontest(step_interacts)

table(round(predict(step_interacts,dt,type = "response")))
table(dt$lubro)

overdisp_test_stat <- dt$lubro

table(round(predict(step_interacts,dt,type = "response")))
table(dt$no_lubro)
