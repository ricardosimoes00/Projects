library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(patchwork) 

dt <- as.data.table(loan.dat)

#a
dt[,lamount_norm := (lamount - mean(lamount))/sd(lamount) ]

#b

#l(b|y) = sum(1,n) (y_i) * ln( fi(b*x_i))) + (1-y_i) * ln(1-fi(b*x_i))

#c
my.ll <- function(b,x,y){
  #b is a scalar
  #x and y are vectors
  ll <- 0 
  for (i in 1:length(x)){
    ll <- ll + y[i]*log(pnorm(b*x[i])) + (1-y[i])*log(1-pnorm(b*x[i]))
  }
  return(ll)
}

beta_values <- seq(-3,3,0.0001)

temp_dt1 <- data.table(beta = beta_values,
                      ll = my.ll(beta_values,dt$lamount_norm,dt$fixed))

ggplot(temp_dt1,aes(x=beta, y = ll))+geom_line()
best_beta <- temp_dt1[which.max(temp_dt1$ll),beta]

#d
beta_values[1]+(which.max(temp_dt1$ll)-1)*0.0001

#e           
best_beta <- 0.5757676           
lamount <- loan.dat$lamount
                      
estimated_probs <- pnorm((-best_beta*mean(lamount))/sd(lamount) + (best_beta/sd(lamount))*seq(0,300000,length=1000))          
plot(seq(0,300000,length=1000),estimated_probs)

#f
1-(pnorm(-best_beta*mean(lamount)/sd(lamount) + (best_beta/sd(lamount))*25000))

#g
pnorm(-best_beta*mean(lamount)/sd(lamount) + (best_beta/sd(lamount))*180000)
