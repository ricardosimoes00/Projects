library(readxl)
library(stlplus)
library(survMisc)
library(ggplot2)
library(dplyr)
library(forecast)
library(xts)
library(lmtest)
library(Metrics)
library(ggpubr)

plotacffunc <- function(tsdata, title_){
  theme_setting <- theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color="grey90", size=0.5),
    panel.grid.major.x = element_line(color="grey90", size=0.5),
    panel.border = element_rect(fill=NA, color="grey70"),
    axis.text = element_text(family="sans",face = "italic"),
    axis.title = element_text(family="sans",face = "italic"),
    plot.title = element_text(size=20, hjust=0.5, family="sans",face = "bold.italic"))
  
  ts.acf <- acf(tsdata, plot=FALSE, lag.max = 50)
  
  alpha <- 0.95
  conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(ts.acf$n.used)
  
  ts.acf$acf %>% 
    as_tibble() %>% mutate(lags = 0:(n()-1)) %>% 
    ggplot(aes(x=lags, y = V1)) + scale_x_continuous(breaks=seq(0,51,5)) +
    geom_hline(yintercept=conf.lims, lty=2, col='blue') +
    labs(y="Autocorrelations", x="Lag", title= title_) +
    geom_segment(aes(xend=lags, yend=0)) +geom_point() + theme_setting
}
plotpacffunc <- function(tsdata, title_){
  theme_setting <- theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color="grey90", size=0.5),
    panel.grid.major.x = element_line(color="grey90", size=0.5),
    panel.border = element_rect(fill=NA, color="grey70"),
    axis.text = element_text(family="sans",face = "italic"),
    axis.title = element_text(family="sans",face = "italic"),
    plot.title = element_text(size=20, hjust=0.5, family="sans",face = "bold.italic"))
  
  ts.pacf <- pacf(tsdata, plot=FALSE, lag.max = 50)
  
  alpha <- 0.95
  conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(ts.pacf$n.used)
  
  ts.pacf$acf %>% 
    as_tibble() %>% mutate(lags = 0:(n()-1)) %>%
    ggplot(aes(x=lags, y = V1)) + 
    geom_segment(aes(xend=lags, yend=0)) +geom_point() + theme_setting + 
    scale_x_continuous(breaks=seq(0,51,5))+ 
    geom_hline(yintercept=conf.lims, lty=2, col='blue') +
    labs(y="Partial Autocorrelations", x="Lag", title= title_)
}
plotseriesfunc <- function(tsdata, title_){
  theme_setting <- theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color="grey90", size=0.5),
    panel.grid.major.x = element_line(color="grey90", size=0.5),
    panel.border = element_rect(fill=NA, color="grey70"),
    axis.text = element_text(family="sans",face = "italic"),
    axis.title = element_text(family="sans",face = "italic"),
    plot.title = element_text(size=20, hjust=0.5, family="sans",face = "bold.italic"))
  
  ts.s <- autoplot(tsdata, plot=FALSE, lag.max = 50)
  
  ts.s + theme_setting + labs(y="Series Value", x="Time", title= title_)
}
plotdensityfunc <- function(tsdata, title_){
  theme_setting <- theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color="grey90", size=0.5),
    panel.grid.major.x = element_line(color="grey90", size=0.5),
    panel.border = element_rect(fill=NA, color="grey70"),
    axis.text = element_text(family="sans",face = "italic"),
    axis.title = element_text(family="sans",face = "italic"),
    plot.title = element_text(size=20, hjust=0.5, family="sans",face = "bold.italic"))
  
  ts.dens <- gghistogram(tsdata$residuals,bins = 100, xlab = "Values", ylab = "Frequency", title =title_)
  
  
  ts.dens + theme_setting
}

setwd("C:/Users/Fernando/Desktop/ST/V.N. Telha - Maia")
data <- read_excel("C:/Users/Fernando/Desktop/ST/QualidadeARO3.xlsx")
Localidade <- data$`VNTelha-Maia`



#Check for Variability, Trend and Seasonality in the TS
Localidade_TS <- ts(Localidade, start=c(1,1), 
                    end=c(366,24), 
                    frequency=24
)


ggtsdisplay(Localidade_TS)
ggarrange(
  plotseriesfunc(Localidade_TS,"Series Plot"),
  ggarrange(plotacffunc(Localidade_TS,"ACF Plot"),plotpacffunc(Localidade,"PACF Plot"),ncol=2,nrow = 2,heights=c(5,1)),nrow=2)


df<- data.frame(Localidade_TS)

df_hour <- data.frame(id = 1:24)
auxiliar <- c()
for (i in 1:(nrow(df))) {
  auxiliar <- c(auxiliar,df$Localidade_TS[i])
  if (i%%24 == 0)  {
    if (i<100){
      print(auxiliar)
    }
    df_hour <- cbind(df_hour, auxiliar)
    auxiliar <- c()
  }
}
df_hour$id<-NULL
boxplot(t(df_hour),xlab = "Hours of the day ",ylab = "Values", main = "Boxplot of the time series' values, grouped by hour of the day")


for (i in 1:(ncol(df_hour))) {
  names(df_hour)[i] <- i
}

Jan<-c()
Fev<-c()
Mar<-c()
Abr<-c()
Mai<-c()
Jun<-c()
Jul<-c()
Ago<-c()
Set<-c()
Out<-c()
Nov<-c()
Dez<-c()

day_means<-c(colMeans(df_hour))

for (i in 1:366){
  if (i <= 31) {
    Jan<-c(Jan,day_means[i])
  }
  if (31 < i & i <= 60) {
    Fev<-c(Fev,day_means[i])
  }
  if (60 < i & i <= 91) {
    Mar<-c(Mar,day_means[i])
  }
  if (91 < i & i <= 121) {
    Abr<-c(Abr,day_means[i])
  }
  if (121 < i & i <= 152) {
    Mai<-c(Mai,day_means[i])
  }
  if (152 < i & i <= 182) {
    Jun<-c(Jun,day_means[i])
  }
  if (182 < i & i <= 213) {
    Jul<-c(Jul,day_means[i])
  }
  if (213 < i & i <= 244) {
    Ago<-c(Ago,day_means[i])
  }
  if (244 < i & i <= 274) {
    Set<-c(Set,day_means[i])
  }
  if (274 < i & i <= 305) {
    Out<-c(Out,day_means[i])
  }
  if (305 < i & i <= 335) {
    Nov<-c(Nov,day_means[i])
  }
  if (335 < i & i <= 366) {
    Dez<-c(Dez,day_means[i])
  }
}

max_ln<-31
df_month <-data.frame(col1 = c(Jan,rep(NA, max_ln - length(Jan))),
                      col2 = c(Fev,rep(NA, max_ln - length(Fev))),
                      col3 = c(Mar,rep(NA, max_ln - length(Mar))),
                      col4 = c(Abr,rep(NA, max_ln - length(Abr))),
                      col5 = c(Mai,rep(NA, max_ln - length(Mai))),
                      col6 = c(Jun,rep(NA, max_ln - length(Jun))),
                      col7 = c(Jul,rep(NA, max_ln - length(Jul))),
                      col8 = c(Ago,rep(NA, max_ln - length(Ago))),
                      col9 = c(Set,rep(NA, max_ln - length(Set))),
                      col10 = c(Out,rep(NA, max_ln - length(Out))),
                      col11 = c(Nov,rep(NA, max_ln - length(Nov))),
                      col12 = c(Dez,rep(NA, max_ln - length(Dez)))
)

months<-c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
for (i in (1:ncol(df_month))){
  names(df_month)[i] <- months[i]
}

boxplot(df_month)
boxplot(t(df_month))

rm(Jan,Fev,Mar,Abr,Mai,Jun,Jul,Ago,Set,Out,Nov,Dez,auxiliar,day_means,firstHour,i,max_ln)


#Difference operators
sdif_Localidade_TS<-diff(Localidade_TS,lag = 24, differences = 1) 
ggtsdisplay(sdif_Localidade_TS) #Good to remove seasonality
ggarrange(
  plotseriesfunc(sdif_Localidade_TS,"Differenced Series Plot"),
  ggarrange(plotacffunc(sdif_Localidade_TS,"ACF Plot"),plotpacffunc(sdif_Localidade_TS,"PACF Plot"),ncol=2,nrow = 2,heights=c(5,1)),nrow=2)


tdif_Localidade_TS<-diff(Localidade_TS)
ggtsdisplay(tdif_Localidade_TS) #There is no big differ

#Only apply seasonal difference operator

#Selected Models - using step-by-step approach explained in the report, no auto.arima :)
test1<-Arima(Localidade_TS, order=c(1,0,1), seasonal=c(0,1,1), lambda="auto")
test2<-Arima(Localidade_TS, order=c(1,0,1), seasonal=c(1,1,1), lambda="auto")
test3<-Arima(Localidade_TS, order=c(1,0,1), seasonal=c(0,1,2), lambda="auto")
test4<-Arima(Localidade_TS, order=c(1,0,1), seasonal=c(1,1,2), lambda="auto")
test5<-Arima(Localidade_TS, order=c(2,0,0), seasonal=c(0,1,1), lambda="auto")
test6<-Arima(Localidade_TS, order=c(2,0,0), seasonal=c(1,1,1), lambda="auto")
test7<-Arima(Localidade_TS, order=c(2,0,0), seasonal=c(0,1,2), lambda="auto")

plot_test1<-ggtsdisplay(test1$residuals)
plot_test2<-ggtsdisplay(test2$residuals)
plot_test3<-ggtsdisplay(test3$residuals)
plot_test4<-ggtsdisplay(test4$residuals)
plot_test5<-ggtsdisplay(test5$residuals)
plot_test6<-ggtsdisplay(test6$residuals)
plot_test7<-ggtsdisplay(test7$residuals)

res_test1<-checkresiduals(test1)
res_test2<-checkresiduals(test2)
res_test3<-checkresiduals(test3)
res_test4<-checkresiduals(test4)
res_test5<-checkresiduals(test5)
res_test6<-checkresiduals(test6)
res_test7<-checkresiduals(test7)

round(cbind(test1$aic,test1$bic,mae(test1$fitted,Localidade_TS),rmse(test1$fitted,Localidade_TS)),2)
round(cbind(test2$aic,test2$bic,mae(test2$fitted,Localidade_TS),rmse(test2$fitted,Localidade_TS)),2)
round(cbind(test3$aic,test3$bic,mae(test3$fitted,Localidade_TS),rmse(test3$fitted,Localidade_TS)),2)
round(cbind(test4$aic,test4$bic,mae(test4$fitted,Localidade_TS),rmse(test4$fitted,Localidade_TS)),2)
round(cbind(test5$aic,test5$bic,mae(test5$fitted,Localidade_TS),rmse(test5$fitted,Localidade_TS)),2)
round(cbind(test6$aic,test6$bic,mae(test6$fitted,Localidade_TS),rmse(test6$fitted,Localidade_TS)),2)
round(cbind(test7$aic,test6$bic,mae(test7$fitted,Localidade_TS),rmse(test7$fitted,Localidade_TS)),2)

#AIC/BIC differences
min_aic <- min(test1$aic, test2$aic, test3$aic, test4$aic, test5$aic, test6$aic, test7$aic)
min_bic <- min(test1$bic, test2$bic, test3$bic, test4$bic, test5$bic, test6$bic, test7$bic)

round(cbind(test1$aic-min_aic,test1$bic-min_bic))
round(cbind(test2$aic-min_aic,test2$bic-min_bic))
round(cbind(test3$aic-min_aic,test3$bic-min_bic))
round(cbind(test4$aic-min_aic,test4$bic-min_bic))
round(cbind(test5$aic-min_aic,test5$bic-min_bic))
round(cbind(test6$aic-min_aic,test6$bic-min_bic))
round(cbind(test7$aic-min_aic,test7$bic-min_bic))


#Residuals check
checkresiduals(test2)
ggarrange(
  plotseriesfunc(test2$residuals,"Residuals Series Plot"),
  ggarrange(plotacffunc(test2$residuals,"ACF Plot"),plotdensityfunc(test2,"Density Plot"),ncol=2,nrow = 2,heights=c(5,1)),nrow=2)
Box.test(test2$residuals,lag=6, type="Ljung")

checkresiduals(test3)
ggarrange(
  plotseriesfunc(test3$residuals,"Residuals Series Plot"),
  ggarrange(plotacffunc(test3$residuals,"ACF Plot"),plotdensityfunc(test3,"Density Plot"),ncol=2,nrow = 2,heights=c(5,1)),nrow=2)
Box.test(test3$residuals,lag=6, type="Ljung")

checkresiduals(test4)
ggarrange(
  plotseriesfunc(test4$residuals,"Residuals Series Plot"),
  ggarrange(plotacffunc(test4$residuals,"ACF Plot"),plotdensityfunc(test4,"Density Plot"),ncol=2,nrow = 2,heights=c(5,1)),nrow=2)
Box.test(test4$residuals,lag=6, type="Ljung")

checkresiduals(test6)
ggarrange(
  plotseriesfunc(test6$residuals,"Residuals Series Plot"),
  ggarrange(plotacffunc(test6$residuals,"ACF Plot"),plotdensityfunc(test6,"Density Plot"),ncol=2,nrow = 2,heights=c(5,1)),nrow=2)
Box.test(test6$residuals,lag=2, type="Ljung")

checkresiduals(test7)
ggarrange(
  plotseriesfunc(test7$residuals,"Residuals Series Plot"),
  ggarrange(plotacffunc(test7$residuals,"ACF Plot"),plotdensityfunc(test7,"Density Plot"),ncol=2,nrow = 2,heights=c(5,1)),nrow=2)
Box.test(test7$residuals,lag=2, type="Ljung")

#Forecast for best model
test3_forecast <- forecast(test3, level=c(95), h=5)

test3$coef

x <- window(Localidade_TS, 365, c(366,24))
y <- test3_forecast$mean
z <- test3_forecast$upper
w <- test3_forecast$lower

ts.plot(x, y, z, w,  gpars = list(col = c("black", "brown1", "brown", "brown")), main = "Time series forecasting plot", ylab = "Values")

round(cbind(as.matrix(test3_forecast$mean),as.matrix(test3_forecast$lower),as.matrix(test3_forecast$upper)),2)