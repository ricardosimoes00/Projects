dat <- cbind(expand.grid(Species = c("Anoli", "Disticus"),
                         PD = c("small","large"), PH = c("high", "low")),
             FREQ = c(32, 61, 11, 41, 86, 73, 35, 70))

attach(dat)
dat <- as.data.table(dat)

#b
species_ph <- tapply(FREQ,list(Species,PH),sum)
or_species_ph<-(species_ph[2,2]*species_ph[1,1])/(species_ph[1,2]*species_ph[2,1])

species_pd <- tapply(FREQ,list(Species,PD),sum)
or_species_pd<-(species_pd[2,2]*species_pd[1,1])/(species_pd[1,2]*species_pd[2,1])

ph_pd <- tapply(FREQ,list(PD,PH),sum)
or_ph_pd<-(ph_pd[2,2]*ph_pd[1,1])/(ph_pd[1,2]*ph_pd[2,1])

variance_table <- function(table){
  1/table[1,1]+1/table[1,2]+1/table[2,1]+1/table[2,2]
}

variance_species_ph <- variance_table(species_ph)
variance_species_pd <- variance_table(species_pd)
variance_ph_pd <- variance_table(ph_pd)

ci_table <- function(variance,or,alfa){
  lb_1<-log(or)-qnorm(1-alfa/2)*sqrt(variance)
  ub_1<-log(or)+qnorm(1-alfa/2)*sqrt(variance)
  
  lb_2<-exp(log(or)-qnorm(1-alfa/2)*sqrt(variance))
  ub_2<-exp(log(or)+qnorm(1-alfa/2)*sqrt(variance))
  
  
  return(matrix(cbind(c(lb_1,ub_1),c(lb_2,ub_2)),byrow=T,nrow=2))
}

ci_species_ph <- ci_table(variance_species_ph,or_species_ph,0.1)
ci_species_pd <- ci_table(variance_species_pd,or_species_pd,0.1)
ci_ph_pd <- ci_table(variance_ph_pd,or_ph_pd,0.1)

#c
library(interactions) 

fit1<-glm(FREQ~PD,data=dat,family=poisson)
p1<-cat_plot(fit1,PD,data = dat,geom = "line",outcome.scale = "link",y.label = "empirical log mean")
p1

fit2<-glm(FREQ~PH,data=dat,family=poisson)
p2<-cat_plot(fit2,PH,data = dat,geom = "line",outcome.scale = "link",y.label = "empirical log mean")
p2

fit3<-glm(FREQ~Species,data=dat,family=poisson)
p3<-cat_plot(fit3,Species,data = dat,geom = "line",outcome.scale = "link",y.label = "empirical log mean")
p3#log of yi- ofsset, no offset. empirical log mean

#d
fit4<-glm(FREQ~PH*PD,data=dat,family=poisson)
p4<-cat_plot(model = fit4, pred = PH, modx=PD,
                  interval=TRUE,data=dat,geom = "line")
p4

fit5<-glm(FREQ~PH*Species,data=dat,family=poisson)
p5<-cat_plot(model = fit5, pred = PH, modx=Species,
             interval=TRUE,data=dat,geom = "line")
p5

fit6<-glm(FREQ~PD*Species,data=dat,family=poisson)
p6<-cat_plot(model = fit6, pred = PD, modx=Species,
             interval=TRUE,data=dat,geom = "line")
p6

#e
fit7<-glm(FREQ~PH+PD+Species,family=poisson,data=dat)
fit7$coefficients
fit7$weights
summary(fit7)

#residual dev test at alfa = 0.05:
pchisq(fit7$deviance,fit7$df.residual,lower.tail = F)
#p-value < 0.05 -> Evidence against H0, i.e, we reject H0 (which says the model assumptions are satisfied )
#Therefore, and at a 5% significance level, we reject this mode and say that it fits badly our data

#f
fit8 <- glm(FREQ~PH+PD+Species+PH:PD+PH:Species+PD:Species,family=poisson,data=dat)
fit8 <- glm(FREQ~(PH+PD+Species)*(PH+PD+Species),family=poisson,data=dat)

summary(fit8)

#residual dev test at alfa = 0.05:
pchisq(fit8$deviance,fit8$df.residual,lower.tail = F)
#p-value > 0.05 -> no evidence against H0, i.e, we don't reject H0 (which says the model assumptions are satisfied )
#Therefore, and at a 5% significance level, we don't this mode and say that it fits our data

#g

#We have (XY, XZ, YZ) - fit8

#(XYZ) - fit_m1

#(XY, XZ) - fit_m2
#(XZ, YZ) - fit_m3
#(XY, YZ) - fit_m4

#(X,YZ) - fit_m5
#(Y,XZ) - fit_m6
#(Z,XY) - fit_m7

#(X,Y,Z) - fit_m8

#Z - PH
#Y - PD
#X - Species

fit_m1 <- glm(FREQ~ PH*PD*Species,family=poisson,data=dat)

fit_m2 <- glm(FREQ~ (PH+PD+Species) + Species:PD + Species:PH, family = poisson, data = dat)
fit_m3 <- glm(FREQ~ (PH+PD+Species) + PH:Species + PH:PD, family = poisson, data = dat)
fit_m4 <- glm(FREQ~ (PH+PD+Species) + PD:Species + PD:PH, family = poisson, data = dat)

fit_m5 <- glm(FREQ~ (PH+PD+Species) + PH:PD, family = poisson, data = dat)
fit_m6 <- glm(FREQ~ (PH+PD+Species) + Species:PD, family = poisson, data = dat)
fit_m7 <- glm(FREQ~ (PH+PD+Species) + PH:Species, family = poisson, data = dat)

fit_m8 <- glm(FREQ~ (PH+PD+Species), family = poisson, data = dat)

temp <- data.table(Formula=c(fit_m1$formula,fit_m2$formula,fit_m3$formula,fit_m4$formula,fit_m5$formula,fit_m6$formula,fit_m7$formula,fit_m8$formula),
                   DFs=c(fit_m1$df.residual,fit_m2$df.residual,fit_m3$df.residual,fit_m4$df.residual,fit_m5$df.residual,fit_m6$df.residual,fit_m7$df.residual,fit_m8$df.residual),
           Deviance=c(fit_m1$deviance,fit_m2$deviance,fit_m3$deviance,fit_m4$deviance,fit_m5$deviance,fit_m6$deviance,fit_m7$deviance,fit_m8$deviance))

temp[,P_value := pchisq(Deviance,DFs,lower.tail = F)]
temp[,Good_model_at_5 := ifelse(P_value>0.05,"Yes","No")]

#Ignoring the saturated mode, we only have to compare fit8 with fit_m2. The difference is that fit8 contains PH:PD interaction, so let's do a partial dev tes
(fit_m2$deviance - fit8$deviance)/1

pchisq((fit_m1$deviance - fit_m2$deviance)/1, 2, lower.tail = F)
# >>> 0.05 So at 5% alfa significance level we don't reject H0, i.e, we reject the largest model so our final model is fit_m2. 
# The interaction term PH:PD is 0, i.e, we consider that this two variables are conditionally independent given Species variable

################# From Solutions ################# 
install.packages("oddsratio")
library(oddsratio)
library(DescTools)

#Ignoring Species: like in b)
#Conditioning on Species = Anoli
tab_PD_PH_Anoli<-tapply(FREQ[Species=="Anoli"], list(PD[Species=="Anoli"], PH[Species=="Anoli"]), sum)
odd_PD_PH_Anoli<-tab_PD_PH_Anoli[1,1]*tab_PD_PH_Anoli[2,2]/(tab_PD_PH_Anoli[1,2]*tab_PD_PH_Anoli[2,1])
odd_PD_PH_Anoli

ci_table(variance_table(tab_PD_PH_Anoli),or = odd_PD_PH_Anoli,alfa = 0.1)

#Conditioning on Species = Disticus
attach(dat)
tab_PD_PH_Disticus<-tapply(FREQ[Species=="Disticus"], list(PD[Species=="Disticus"], PH[Species=="Disticus"]), sum)
odd_PD_PH_Disticus<-tab_PD_PH_Disticus[1,1]*tab_PD_PH_Disticus[2,2]/(tab_PD_PH_Disticus[1,2]*tab_PD_PH_Disticus[2,1])
odd_PD_PH_Disticus

ci_table(variance_table(tab_PD_PH_Disticus),or = odd_PD_PH_Disticus,alfa = 0.1)
