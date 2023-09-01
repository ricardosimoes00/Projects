library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(ggrepel)

x_vals <- seq(-4, 4, .01)

log_reg <- function(x) {
  exp(x)/(1+exp(x))
}

prob_reg <- function(x){
  pnorm(x)
}

log_log_reg <- function(x) {
  1-exp(-exp(x))
}

plot(x_vals,log_reg(x_vals))
plot(x_vals,prob_reg(x_vals))
plot(x_vals,log_log_reg(x_vals))

data <- data.table(x = seq(-4, 4, .01), log_reg = log_reg(x_vals), prob_reg = prob_reg(x_vals), log_log_reg = log_log_reg(x_vals))
data <- melt(data,
             id.vars = "x", 
             measure.vars = c("log_reg", "prob_reg", "log_log_reg"), 
             variable.name = "F", 
             value.name = "values",
)


ggplot(data,aes(x, values)) + 
  geom_line() +
  facet_grid(~F)




install.packages("plotly")
library(plotly)

b0<-1
b1<-1
b2<-1

x <- seq(-5, 5, .01)
y <- seq(-5, 5, .01)

mu <- b0+b1*x+b2*y


prob.logit=function(eta){
  return(exp(eta)/(1+exp(eta)))
}
prob.logit=Vectorize(prob.logit)
prob.probit=function(eta){
  return(pnorm(eta))
}
prob.probit=Vectorize(prob.probit)
prob.loglog=function(eta){
  return(1-exp(-exp(eta)))
}
prob.loglog=Vectorize(prob.loglog)

library(plotly)


x_1=seq(-5,5,0.1)
x_2=seq(-5,5,0.1)

beta0=1
beta1=1
beta2=1

len1=length(x_1)
len2=length(x_2)

f=function(x_1,x_2){
  return(beta0+x_1*beta1+x_2*beta2)
}

library(plotly)

u.logit=matrix(NA,len1,len2)
for(i in 1:len1){
  for(j in 1:len2)
    u.logit[i,j]=prob.logit(f(x_1[i],x_2[j]))
}
u.logit=t(u.logit)
u.probit=matrix(NA,len1,len2)
for(i in 1:len1){
  for(j in 1:len2)
    u.probit[i,j]=prob.probit(f(x_1[i],x_2[j]))
}
u.probit=t(u.probit)
u.loglog=matrix(NA,len1,len2)
for(i in 1:len1){
  for(j in 1:len2)
    u.loglog[i,j]=prob.loglog(f(x_1[i],x_2[j]))
}
u.loglog=t(u.loglog)

fig.logit <- plot_ly(
  type = 'surface',
  x = x_1,
  y = x_2,
  z = u.logit)
fig.logit