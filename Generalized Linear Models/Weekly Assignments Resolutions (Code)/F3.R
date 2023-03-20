library(PerformanceAnalytics)
library(ggplot2)

data <- USCRIME.dat

#2a
my.model <- lm(R ~ Ex0 + X + Ed + Age + poly(NW,3)
            + U2 + poly(LF, 2) + N, data)

my.model$residuals

plot(data$X,my.model$residuals)
plot(data$Ed,my.model$residuals)

#b
raw_residuals <- my.model$residuals
summary(my.model)

library(matlib)
X <- model.matrix(my.model)

hat_mat <- X %*% Inverse((t(X) %*% X)) %*% t(X)
diag(hat_mat)
s = sqrt(sum((my.model$residuals)**2)/(47-12))


int_residuals <- raw_residuals/(s*sqrt(1-diag(hat_mat)))

plot(c(1:47),int_residuals)
plot(c(1:47),rstandard(my.model))
abline(h=1.95)
abline(h=-1.95)

100*length(which(int_residuals>1.96 | int_residuals< -1.96))/47
100*length(which(int_residuals>3 | int_residuals< -3))/47


library(MASS)

#calculate studentized residuals
stud_resids <- studres(my.model)
plot(c(1:47),stud_resids)


####################


#c
plot(my.model$fitted.values,rstandard(my.model))
#no dependence shown in the plot, so we don't have to transform Y variable
#The homogeneity assumption 

#d
?qqnorm
qqnorm(rstandard(my.model))
abline(0,1)
#Deviations from a straight line indicate that the normality assumption is violated
qplot(rstandard(my.model))

#e
library(matlib)
X <- model.matrix(my.model)

hat_mat <- X %*% Inverse((t(X) %*% X)) %*% t(X)

for (i in c(1:47)){
  if (hat_mat[i,i] > tr(hat_mat)*2/nrow(data)) {
    print(i)
    print(hat_mat[i,i]> tr(hat_mat)*2/nrow(data))
  }
}

#f
for (i in c(1:47)){
    print(hat_mat[i,i]> tr(hat_mat)*2/nrow(data))
}

aux <- rep(0, 47)
aux[22] <- 1
aux[37] <- 1
aux[45] <- 1
data$bol <- aux

data

ggplot(data,aes(x=Age,y=Ed, color=bol))+geom_point()

plot(x = data$Age, y= data$Ed,pch = 16, col = ifelse(data$bol == TRUE, "red", "blue"))
plot(x = data$Age, y= data$X,pch = 16, col = ifelse(data$bol == TRUE, "red", "blue"))
plot(x = data$Ex0, y= data$X,pch = 16, col = ifelse(data$bol == TRUE, "red", "blue"))

?cooks.distance

any(cooks.distance(my.model)>1)
max(cooks.distance(my.model))#22

