library(data.table)
library(interactions)

ins.dat <- insurance.dat
ins.dat <- as.data.table(ins.dat)

#Problem 1

#a
ins.dat <- ins.dat[Claims>0]
ins.dat$average_claim_size <- ins.dat$Payment/ins.dat$Claims

str(ins.dat)
ins.dat$Kilometres <- as.factor(ins.dat$Kilometres)
ins.dat$Zone <- as.factor(ins.dat$Zone)
ins.dat$BonusF <- as.factor(ins.dat$Bonus)
ins.dat$Make <- as.factor(ins.dat$Make)

#c
fit1 <- glm(average_claim_size ~ Kilometres, family = Gamma(link = "log"), data = ins.dat)
fit2 <- glm(average_claim_size ~ Zone, family = Gamma(link = "log"), data = ins.dat)
fit3 <- glm(average_claim_size ~ BonusF, family = Gamma(link = "log"), data = ins.dat)
fit4 <- glm(average_claim_size ~ Make, family = Gamma(link = "log"), data = ins.dat)

cat_plot(fit1, pred = Kilometres, data = ins.dat, interval = T, outcome.scale = "link", y.label = "empirical scaled log mean", geom = "line")
cat_plot(fit2, pred = Zone, data = ins.dat, interval = T, outcome.scale = "link", y.label = "empirical scaled log mean", geom = "line")
cat_plot(fit3, pred = Bonus, data = ins.dat, interval = T, outcome.scale = "link", y.label = "empirical scaled log mean", geom = "line")
cat_plot(fit4, pred = Make, data = ins.dat, interval = T, outcome.scale = "link", y.label = "empirical scaled log mean", geom = "line")

levels(ins.dat$Zone)[7] <- "1"
levels(ins.dat$Make)[4] <- "2"
levels(ins.dat$Make)[8] <- "7"

#d
model.main <- glm(average_claim_size ~Kilometres + Zone + Bonus + Make, data = ins.dat, family = Gamma(link = "log"), weights = Claims)
model.main.summary <- summary(model.main)

model.main$deviance / model.main.summary$dispersion
model.main$df.residual

anova(model.main,test = "Chisq")

#e
test_statistics <- model.main$deviance/model.main.summary$dispersion
test_statistics
pchisq(test_statistics,model.main$df.residual,lower.tail = F)
#p-value almost 1 -> no evidence against null hypothesis, which means we do not reject the null hypothesis at 5% significance level.
#This means the model assumptions are satisfied and there is no lack of fit

#Problem 2

#a
fit5 <- glm(Claims ~ Kilometres*Zone, data = ins.dat, family = poisson, offset = log(Insured))
fit6 <- glm(Claims ~ Kilometres*BonusF, data = ins.dat, family = poisson, offset = log(Insured))
fit7 <- glm(Claims ~ Kilometres*Make, data = ins.dat, family = poisson, offset = log(Insured))
fit8 <- glm(Claims ~ Zone*BonusF, data = ins.dat, family = poisson, offset = log(Insured))
fit9 <- glm(Claims ~ Zone*Make, data = ins.dat, family = poisson, offset = log(Insured))
fit10 <- glm(Claims ~ BonusF*Make, data = ins.dat, family = poisson, offset = log(Insured))

cat_plot(fit5, modx = Kilometres, pred = Zone, data = ins.dat, interval = T, outcome.scale = "link", y.label = "empirical scaled log mean", geom = "line")
cat_plot(fit6, modx = Kilometres, pred = BonusF, data = ins.dat, interval = T, outcome.scale = "link", y.label = "empirical scaled log mean", geom = "line")
cat_plot(fit7, modx = Kilometres, pred = Make, data = ins.dat, interval = T, outcome.scale = "link", y.label = "empirical scaled log mean", geom = "line")
cat_plot(fit8, pred = Zone, modx = BonusF, data = ins.dat, interval = T, outcome.scale = "link", y.label = "empirical scaled log mean", geom = "line")
cat_plot(fit9, pred = Zone, modx = Make, data = ins.dat, interval = T, outcome.scale = "link", y.label = "empirical scaled log mean", geom = "line")
cat_plot(fit10, pred = BonusF, modx = Make, data = ins.dat, interval = T, outcome.scale = "link", y.label = "empirical scaled log mean", geom = "line")

#Kilometres*Zone - maybe, Zone and kilometers (levels 2,3,4) - V
#Kilometres*BonusF - maybe, BonusF and kilometers (levels 2,3,4) - V
#Kilometres*Make- maybe, Zone and kilometers (levels 2,3,4) - V
#Zone*BonusF - Yes - V
#Zone*Make - Yes - V
#BonusF*Make - Yes - V

#b
str(ins.dat)
attach(ins.dat)
ins <- insurance.dat
ins$Kilometres = as.factor(ins$Kilometres)
ins$Zone = as.factor(ins$Zone)
ins$Make = as.factor(ins$Make)
attach(ins)
Kilometres.new=Kilometres
Zone.new=Zone
Make.new=Make
Make.new[Make==8]=7
Make.new[Make==9]=8
Make.new=factor(Make.new)

model.inter = glm(Claims ~ (Bonus + Make.new + Kilometres.new + Zone.new)*(Bonus + Make.new + Kilometres.new + Zone.new),
                  offset=log(Insured), family=poisson)
summary(model.inter)

#c
model.main = glm(Claims ~ Bonus + Zone.new + Kilometres.new + Make.new,
                 offset=log(Insured), family=poisson)

model.inter2 = step(model.main, 
                    scope = list(lower = model.main,upper = model.inter), 
                    direction='forward', 
                    k = 2)

summary(model.inter2) #same model as model.inter, so all interactions reduced the AIC value

anova(model.inter2,test ="Chisq")
#all extremely low p-values so there is evidence against H0, i.e, all coeficients are relevant

#d
Dr <- model.main$deviance
n_p1 <- model.main$df.residual

Df <- model.inter$deviance
n_p <- model.inter$df.residual
sigma_f <- summary(model.inter)$dispersion

pchisq((Dr-Df)/sigma_f,n_p1 - n_p)  #p-value = 1
qchisq(0.05,n_p1 - n_p,lower.tail = F)  #<<<<

#e
plot(resid(model.inter, type = "pearson"))
plot(resid(model.inter, type = "deviance"))

plot(ins$Claims,model.inter$fitted.values)+abline(1,1)

library(data.table)
ins <- as.data.table(ins)

pred2 <- tapply((Claims/Insured)[Make.new==2&Zone.new==2],
                list(Kilometres.new[Make.new==2&Zone.new==2],Bonus[Make.new==1&Zone.new==2]), mean)
pred3 <- tapply((Claims/Insured)[Make.new==2&Zone.new==3],
                list(Kilometres.new[Make.new==2&Zone.new==3],Bonus[Make.new==1&Zone.new==3]), mean)
pred4 <- tapply((Claims/Insured)[Make.new==2&Zone.new==4],
                list(Kilometres.new[Make.new==2&Zone.new==4],Bonus[Make.new==1&Zone.new==4]), mean)

#g
summary(model.inter)
#Myabe, since the total deviance for the data collected is quite higer than the degrees of freedom
#of the chisqured distribution (n-p). The 1st quanitity converges to the latter one, for large n,
#but we do not observe that