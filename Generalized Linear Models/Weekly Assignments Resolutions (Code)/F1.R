library(PerformanceAnalytics)
library(ggplot2)

data <- USCRIME.dat 
data2 <- data
data2$LF <- NULL
data2$NW <- NULL

#a
?chart.Correlation
chart.Correlation(data2, histogram=TRUE, pch=19)
cor(x = data2$R, y = data2$Ex0)
plot(data2$U1,data2$R)

#X and ED
#R and U1/U2
#R and U1/U2


data2$n

ggplot(data = data2, aes(x = R, y = Ex0)) +   #Strong positive correlation with Police expenditure variable
  geom_point()

ggplot(data = data2, aes(x = R, y = U1)) +   #No correlation (almost 0)
  geom_point()

ggplot(data = data2, aes(x = R, y = U2)) +   #No correlation (almost 0)
  geom_point()

ggplot(data = data2, aes(x = Ex0, y = Ex1)) +   #Almost perfect correlation
  geom_point()

ggplot(data = data2, aes(x = W, y = X)) +   #Strong negative correlation
  geom_point()

#b
fit <- lm(R~., data = data2) #p = k+1 = 11 + 1 = 12 
sum <- summary(fit)
sum

fit_null <- lm(R~1, data = data2) # p = k+1 = 0 + 1 = 1
sum_null <- summary(fit_null)
sum_null


# Significant: Age, Ed, U2, X

(summary$sigma)**2
sum((summary$residuals)**2)/(nrow(data2)-(ncol(data2)))


################################
#observed value of the Overall significance test statistics:
SSE_NULL = sum((fit_null$residuals)**2)
SSE = sum((fit$residuals)**2)
r = 11
n_p = 47-12

((SSE_NULL-SSE)/r)/(SSE/n_p)

################################

#c3
dat1 <- data.frame(x=data$LF, y = data$R)
dat2 <- data.frame(x=data$NW, y = data$R)

dat2

min.f1 <- function(data, par) {
  with(dat1, sum((par[1] + par[2] * x + par[3] * x**2 - y)^2))}

?with

min.f2 <- function(data, par) {
  with(dat2, sum((par[1] + par[2] * x + par[3] * x**2 + par[4] * x**3- y)^2))}



result1 <- optim(par = c(0, 0, 0), fn = min.f1, data = dat1, method = "BFGS")
result2 <- optim(par = c(0, 0, 0, 0), fn = min.f2, data = dat2, method = "BFGS")

result1$par

tr.LF <- result1$par[1] + result1$par[2]*data$LF + result1$par[3]*(data$LF)**2
tr.NW <- result2$par[1] + result2$par[2]*data$NW + result2$par[3]*(data$NW)**2 + result2$par[4]*(data$NW)**3

abline(result1$par[1],result1$par[2],result1$par[3])

result1$par[1]

plot(data$LF, data$R)
x=seq(400,700,0.1)
tr.LF.line <- result1$par[1] + result1$par[2]*x + result1$par[3]*(x)**2
lines(x,tr.LF.line)
plot(tr.LF, data$R)


plot(data$NW, data$R)
x=seq(0,450,0.1)
tr.NW.line <- result2$par[1] + result2$par[2]*x + result2$par[3]*(x)**2 + result2$par[4]*(x)**3
lines(x,tr.NW.line)
plot(tr.NW, data$R)


###or: lm(R~poly(Lf,2,raw = F))
###################################


################################
library(matlib)

dat1 <- data.frame(data$LF, (data$LF)**2, data$R)
colnames(dat1) <- c("x1","x2","y")

dat2 <- data.frame(data$NW, (data$NW)**2, (data$NW)**3, data$R)
colnames(dat2) <- c("x1","x2", "x3", "y")

design_mat1 <- dat1[,c("x1","x2")]
design_mat1 <- cbind(a = 1, design_mat1)
design_mat1 <- data.matrix(design_mat1)

design_mat1

coef1 <- (solve(t(design_mat1) %*% design_mat1)) %*% t(design_mat1) %*% dat1[,c("y")]

design_mat2 <- dat2[,c("x1","x2","x3")]
design_mat2 <- cbind(a = 1, design_mat2)
design_mat2 <- data.matrix(design_mat2)

design_mat2

coef2 <- (solve(t(design_mat2) %*% design_mat2)) %*% t(design_mat2) %*% dat2[,c("y")]

tr.LF2 <- coef1[1] + coef1[2]*dat1$x1 + coef1[3]*dat1$x2
tr.NW2 <- coef2[1] + coef2[2]*dat2$x1 + coef2[3]*dat2$x2 + coef2[4]*dat2$x3

plot(tr.LF2, data$R)
plot(tr.NW2, data$R)

