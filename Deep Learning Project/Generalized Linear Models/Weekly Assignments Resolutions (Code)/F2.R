library(PerformanceAnalytics)
library(ggplot2)

data <- USCRIME.dat

#2a
fit1 <- lm(R ~ Age+S+Ed+Ex0+Ex1+M+N+U1+U2+W+X+poly(NW,3)+poly(LF,2), data)
fit2 <- lm(R ~ 1, data)

AIC.f <- step(fit2, direction = "forward", scope =list(upper=fit1,lower=fit2), k = 2)   #R ~ Ex0 + X + Ed + Age + poly(NW, 3) + U2 + poly(LF, 2) + N
AIC.b <- step(fit1, direction = "backward", scope =list(upper=fit1,lower=fit2), k = 2)  #R ~ Age + Ed + Ex0 + N + U2 + X + poly(NW, 3) + poly(LF, 2)
AIC.both1 <- step(fit1, direction = "both", scope =list(upper=fit1,lower=fit2), k = 2)  #R ~ Age + Ed + Ex0 + N + U2 + X + poly(NW, 3) + poly(LF, 2)
AIC.both2 <- step(fit2, direction = "both", scope =list(upper=fit1,lower=fit2), k = 2)  #R ~ Age + Ed + Ex0 + N + U2 + X + poly(NW, 3) + poly(LF, 2)

IC.model <- lm(R ~ Age + Ed + Ex0 + N + U2 + X + poly(NW, 3) + poly(LF, 2), data)
full<- lm(R ~ Age+S+Ed+Ex0+Ex1+M+N+U1+U2+W+X+poly(NW,3)+poly(LF,2), data)

summary(full) #r_sq = 0.8401  #Adj r_sq:  0.7549 
AIC(full) # AIC = 425.7869

summary(AIC.f) #r_sq = 0.8285 #Adj r_sq:  0.7746
AIC(AIC.f) # AIC = 419.098

summary(AIC.b) #r_sq = 0.8285 #Adj r_sq:  0.7746
AIC(AIC.b) # AIC = 419.098

summary(AIC.both1) #r_sq = 0.8285 #Adj r_sq:  0.7746
AIC(AIC.both1) # AIC = 419.098

summary(AIC.both2) #r_sq = 0.8285 #Adj r_sq:  0.7746
AIC(AIC.both2) # AIC = 419.098



#2b
summary(IC.model)

#Rej rule: for alfa value higher than p-value we do not reject H0, otherwise we reject H0. Do not reject for alfa = 5%.
#For the covariates the wald test says that we do not rej H0 for covariates N, poly(NW, 3)1  , Etc....

#observed value of the Overall significance test statistics:
fit_null <- lm(R~1, data = data)

SSE_NULL = sum((fit_null$residuals)**2)
SSE = sum((IC.model$residuals)**2)
r = 11
n_p = 47-12

((SSE_NULL-SSE)/r)/(SSE/n_p)

#Problem 4

PolyLF <- poly(data$LF,2)
attributes(PolyLF)$coefs

aLF = attributes(PolyLF)$coefs$alpha
nLF = attributes(PolyLF)$coefs$norm2

f0 = function(x){
  return(1 / sqrt(nLF[2]))
}
f1 = function(x){
  return((x-aLF[1]) / sqrt(nLF[3]))
}
f2 = function(x){
  return(((x-aLF[2]) * sqrt(nLF[3]) * f1(x) - nLF[3] / sqrt(nLF[2]) * f0(x)) / sqrt(nLF[4]))
}

LF <- data$LF
x<-LF[1]
PolyLF[1,]

f1(x)
f2(x)

#b
coef1<-PolyLF[,1]
coef2<-PolyLF[,2]
coef1 %*% coef2

mtx <- matrix(cbind(coef1,coef2),ncol=2)
t(mtx) %*% mtx

#c
fit3 <- lm(data$R ~ poly(data$LF,2))
fit4 <- lm(data$R ~ poly(data$LF,2,raw=TRUE))

summary(fit3)
summary(fit4)
.