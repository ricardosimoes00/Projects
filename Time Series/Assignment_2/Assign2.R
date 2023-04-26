library(readxl)
library(stlplus)
library(survMisc)
library(ggplot2)
library(dplyr)
library(forecast)
library(xts)
library(lmtest)
library(fGarch)
library(rugarch)
library(imputeTS)
library(moments)
library(TSstudio)
library(gridExtra)  
library(ggpubr)
library(randtests)

#Extract the data
EDP_data <- readxl::read_excel("EDPRENOVAVEISprice.xls", skip = 3)
GALP_data <- readxl::read_excel("GALPENERGIANOMprice.xls", skip = 3)
MOTAENGIL_data <- readxl::read_excel("MOTAENGILprice.xls", skip = 3)
NOS_data <- readxl::read_excel("NOSSGPSprice.xls", skip = 3)
NOVABASE_data <- readxl::read_excel("NOVABASESGPSprice.xls", skip = 3)


NOVABASE_data[nrow(NOVABASE_data) + 1,] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA)
NOVABASE_data$Date[509] <- "2022/01/18"
NOVABASE_data <- NOVABASE_data[order(NOVABASE_data$Date, decreasing = TRUE),]

NOVABASE_data <- na_interpolation(NOVABASE_data)

#0 missing values
sum(is.na(EDP_data))
sum(is.na(GALP_data))
sum(is.na(MOTAENGIL_data))
sum(is.na(NOS_data))
sum(is.na(NOVABASE_data))


#Extract closing values
closed_values_EDP <- EDP_data$Close
closed_values_GALP <- GALP_data$Close
closed_values_MOTAENGIL <- MOTAENGIL_data$Close
closed_values_NOS <- NOS_data$Close
closed_values_NOVABASE <- NOVABASE_data$Close

#Create time series index data
time_index_EDP <- as.Date(EDP_data$Date, format="%Y-%m-%d")
time_index_GALP <- as.Date(GALP_data$Date, format="%Y-%m-%d")
time_index_MOTAENGIL <- as.Date(MOTAENGIL_data$Date, format="%Y-%m-%d")
time_index_NOS <- as.Date(NOS_data$Date, format="%Y-%m-%d")
time_index_NOVABASE <- as.Date(NOVABASE_data$Date, format="%Y-%m-%d")


#Plot the Time Series
ts_plot(xts(x = closed_values_EDP, order.by = time_index_EDP),
        title = " ",
        Xtitle = "Time",
        Ytitle = "Price (euros)")
ts_plot(xts(x = closed_values_GALP, order.by = time_index_GALP),
        title = " ",
        Xtitle = "Time",
        Ytitle = "Price (euros)")
ts_plot(xts(x = closed_values_MOTAENGIL, order.by = time_index_MOTAENGIL),
        title = " ",
        Xtitle = "Time",
        Ytitle = "Price (euros)")
ts_plot(xts(x = closed_values_NOS, order.by = time_index_NOS),
        title = " ",
        Xtitle = "Time",
        Ytitle = "Price (euros)")
ts_plot(xts(x = closed_values_NOVABASE, order.by = time_index_NOVABASE),
        title = " ",
        Xtitle = "Time",
        Ytitle = "Price (euros)")


#title = "EDP Closing Values by Day"
#title = "GALP Closing Values by Day"
#title = "MOTAENGIL Closing Values by Day"
#title = "NOS Closing Values by Day"
#title = "NOVABASE Closing Values by Day",

#Function to compute log-returns
convert_log_returns <- function(data){
  returns <- c()
  for(i in 1:(length(data)-1)){
    returns <- append(returns, log(data[i+1]/data[i]))
  }
  return(returns)
}

#Apply function to our data
log_returns_EDP <- convert_log_returns(closed_values_EDP)
log_returns_GALP <- convert_log_returns(closed_values_GALP)
log_returns_MOTAENGIL <- convert_log_returns(closed_values_MOTAENGIL)
log_returns_NOS <- convert_log_returns(closed_values_NOS)
log_returns_NOVABASE <- convert_log_returns(closed_values_NOVABASE)



#Create time series index data
returns_time_index_EDP <- as.Date(EDP_data$Date[-1], format="%Y-%m-%d")
returns_time_index_GALP <- as.Date(GALP_data$Date[-1], format="%Y-%m-%d")
returns_time_index_MOTAENGIL <- as.Date(MOTAENGIL_data$Date[-1], format="%Y-%m-%d")
returns_time_index_NOS <- as.Date(NOS_data$Date[-1], format="%Y-%m-%d")
returns_time_index_NOVABASE <- as.Date(NOVABASE_data$Date[-1], format="%Y-%m-%d")


#Creating the Time series
ts_returns_EDP <- xts(x = log_returns_EDP, order.by = returns_time_index_EDP)
ts_returns_GALP <- xts(x = log_returns_GALP, order.by = returns_time_index_GALP)
ts_returns_MOTAENGIL <- xts(x = log_returns_MOTAENGIL, order.by = returns_time_index_MOTAENGIL)
ts_returns_NOS <- xts(x = log_returns_NOS, order.by = returns_time_index_NOS)
ts_returns_NOVABASE <- xts(x = log_returns_NOVABASE, order.by = returns_time_index_NOVABASE)


#Plot the log Returns Time Series
ts_plot(ts_returns_EDP, 
        title = " ",
        Xtitle = "Time",
        Ytitle = "Log-Return")
ts_plot(ts_returns_GALP, 
        title = " ",
        Xtitle = "Time",
        Ytitle = "Log-Return")
ts_plot(ts_returns_MOTAENGIL, 
        title = " ",
        Xtitle = "Time",
        Ytitle = "Log-Return")
ts_plot(ts_returns_NOS, 
        title = " ",
        Xtitle = "Time",
        Ytitle = "Log-Return")
ts_plot(ts_returns_NOVABASE, 
        title = " ",
        Xtitle = "Time",
        Ytitle = "Log-Return")

qqplot_draw <-function(log_data,title_){
  theme_setting <- theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color="grey90", size=0.5),
    panel.grid.major.x = element_line(color="grey90", size=0.5),
    panel.border = element_rect(fill=NA, color="grey70"),
    axis.text = element_text(family="sans",face = "italic"),
    axis.title = element_text(family="sans",face = "italic"),
    plot.title = element_text(size=10, hjust=0.5, family="sans",face = "bold.italic"))
  
  
  ggplot(as.data.frame(log_data), aes(sample = log_data)) + stat_qq() +
    stat_qq_line() +
    labs(y="Sample", x="Theoretical", title= title_) + theme_setting
}

qqplot_draw(log_returns_EDP, "Normal Q-Q Plot for EDP")
qqplot_draw(log_returns_GALP, "Normal Q-Q Plot for GALP")
qqplot_draw(log_returns_MOTAENGIL, "Normal Q-Q Plot for MOTAENGIL")
qqplot_draw(log_returns_NOS, "Normal Q-Q Plot for NOS")
qqplot_draw(log_returns_NOVABASE, "Normal Q-Q Plot for NOVABASE")


plotacffunc <- function(tsdata, title_){
  theme_setting <- theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color="grey90", size=0.5),
    panel.grid.major.x = element_line(color="grey90", size=0.5),
    panel.border = element_rect(fill=NA, color="grey70"),
    axis.text = element_text(family="sans",face = "italic"),
    axis.title = element_text(family="sans",face = "italic"),
    plot.title = element_text(size=10, hjust=0.5, family="sans",face = "bold.italic"))
  
  ts.acf <- acf(tsdata, plot=FALSE, lag.max = 50)
  
  alpha <- 0.95
  conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(ts.acf$n.used)
  
  ts.acf$acf %>% 
    as_tibble() %>% mutate(lags = 0:(n()-1)) %>% 
    ggplot(aes(x=lags, y = V1)) + scale_x_continuous(breaks=seq(0,51,5)) +
    geom_hline(yintercept=conf.lims, lty=2, col='blue') +
    labs(y="Autocorrelation", x="Lag", title= title_) +
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
    plot.title = element_text(size=10, hjust=0.5, family="sans",face = "bold.italic"))
  
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

plotacffunc(ts_returns_EDP,"Autocorrelation Function Plot for EDP")
plotacffunc(ts_returns_GALP,"Autocorrelation Function Plot for GALP")
plotacffunc(ts_returns_MOTAENGIL,"Autocorrelation Function Plot for MOTAENGIL")
plotacffunc(ts_returns_NOS,"Autocorrelation Function Plot for NOS")
plotacffunc(ts_returns_NOVABASE,"Autocorrelation Function Plot for NOVABASE")

plotacffunc(abs(ts_returns_EDP),"ACF of Absolute Values for EDP")
plotacffunc(abs(ts_returns_GALP),"ACF of Absolute Values for GALP")
plotacffunc(abs(ts_returns_MOTAENGIL),"ACF of Absolute Values for MOTAENGIL")
plotacffunc(abs(ts_returns_NOS),"ACF of Absolute Values for NOS")
plotacffunc(abs(ts_returns_NOVABASE),"ACF of Absolute Values for NOVABASE")

plotacffunc((ts_returns_EDP)^2, "ACF of Squared Values for EDP")
plotacffunc((ts_returns_GALP)^2, "ACF of Squared Values for GALP")
plotacffunc((ts_returns_MOTAENGIL)^2, "ACF of Squared Values for MOTAENGIL")
plotacffunc((ts_returns_NOS)^2, "ACF of Squared Values for NOS")
plotacffunc((ts_returns_NOVABASE)^2, "ACF of Squared Values for NOVABASE")

#mean of log-returns
mean(log_returns_EDP)
mean(log_returns_GALP)
mean(log_returns_MOTAENGIL)
mean(log_returns_NOS)
mean(log_returns_NOVABASE)

var(log_returns_EDP)
var(log_returns_GALP)
var(log_returns_MOTAENGIL)
var(log_returns_NOS)
var(log_returns_NOVABASE)

kurtosis(log_returns_EDP)
kurtosis(log_returns_GALP)
kurtosis(log_returns_MOTAENGIL)
kurtosis(log_returns_NOS)
kurtosis(log_returns_NOVABASE)


find_best_model <- function(series, model, submodel, dist, max_p, max_q, min_p, min_q){
  results <- data.frame()
  count <- 1
  for(i in min_p:max_p){
    for(j in min_q:max_q){
      if(i==0 & j==0){next}
      results[count, 1]<- paste0(paste0(paste0("model_", toString(i)), "_"), toString(j))
      
      spec <- ugarchspec(variance.model = list(model = model, garchOrder = c(i,j), submodel = submodel), 
                         mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                         distribution.model = dist)
      
      criteria <- infocriteria(ugarchfit(spec, series, solver = "hybrid"))
      
      results[count, 2] <- criteria[1]
      results[count ,3] <- criteria[2]
      
      count = count + 1
    }
  }
  return(results)
}


find_best_GARCH_M <- function(series, submodel, max_p, max_q){
  results <- matrix(nrow = max_q*max_p, ncol = 3)
  colnames(results) <- c("GARCH Model","AIC","BIC")
  count <- 1
  for(i in 1:max_p){
    for(j in 1:max_q){
      results[count, 1]<- paste0(paste0(paste0("model_", toString(i)), "_"), toString(j))
      
      spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(i,j)), mean.model = list(armaOrder = c(0,0), include.mean = TRUE, archm = TRUE, archpow = 2), distribution.model = submodel)
      criteria <- infocriteria( ugarchfit(spec, series, solver = "hybrid"))
      
      results[count, 2] <- as.numeric(criteria[1])
      results[count ,3] <- as.numeric(criteria[2])
      
      count = count + 1
    }
    
  }
  return(as.data.frame(results))
}




#GARCH

#Cond Dist = Norm
garch_EDP_norm <- find_best_model(ts_returns_EDP, "fGARCH", "GARCH", "norm", 5, 5, 0, 0)
garch_GALP_norm <- find_best_model(ts_returns_GALP, "fGARCH", "GARCH", "norm", 5, 5, 0, 0)
garch_MOTAENGIL_norm <- find_best_model(ts_returns_MOTAENGIL, "fGARCH", "GARCH", "norm", 5, 5, 0, 0)
garch_NOS_norm <- find_best_model(ts_returns_NOS, "fGARCH", "GARCH", "norm", 5, 5, 0, 0)
garch_NOVABASE_norm <- find_best_model(ts_returns_NOVABASE, "fGARCH", "GARCH", "norm", 5, 5, 0, 0)


#APARCH

#Cond Dist = Norm
aparch_EDP_norm <- find_best_model(ts_returns_EDP, "apARCH", NULL, "norm", 5, 5, 1, 1)
aparch_GALP_norm <- find_best_model(ts_returns_GALP, "apARCH", NULL, "norm", 5, 5, 1, 1)
aparch_MOTAENGIL_norm <- find_best_model(ts_returns_MOTAENGIL, "apARCH", NULL, "norm", 5, 5, 1, 1)
aparch_NOS_norm <- find_best_model(ts_returns_NOS, "apARCH", NULL, "norm", 5, 5, 1, 1)
aparch_NOVABASE_norm <- find_best_model(ts_returns_NOVABASE, "apARCH", NULL, "norm", 5, 5, 1, 1)


#FIGARCH

#Cond Dist = norm
figarch_EDP_norm <- find_best_model(ts_returns_EDP, "fiGARCH", NULL, "norm", 5, 5, 1, 1)
figarch_GALP_norm <- find_best_model(ts_returns_GALP, "fiGARCH", NULL, "norm", 5, 5, 1, 1)
figarch_MOTAENGIL_norm <- find_best_model(ts_returns_MOTAENGIL, "fiGARCH", NULL, "norm", 5, 5, 1, 1)
figarch_NOS_norm <- find_best_model(ts_returns_NOS, "fiGARCH", NULL, "norm", 5, 5, 1, 1)
figarch_NOVABASE_norm <- find_best_model(ts_returns_NOVABASE, "fiGARCH", NULL, "norm", 5, 5, 1, 1)


#IGARCH

#Cond Dist = norm
igarch_EDP_norm <- find_best_model(ts_returns_EDP, "iGARCH", NULL, "norm", 5, 5, 1, 1)
igarch_GALP_norm <- find_best_model(ts_returns_GALP, "iGARCH", NULL, "norm", 5, 5, 1, 1)
igarch_MOTAENGIL_norm <- find_best_model(ts_returns_MOTAENGIL, "iGARCH", NULL, "norm", 5, 5, 1, 1)
igarch_NOS_norm <- find_best_model(ts_returns_NOS, "iGARCH", NULL, "norm", 5, 5, 1, 1)
igarch_NOVABASE_norm <- find_best_model(ts_returns_NOVABASE, "iGARCH", NULL, "norm", 5, 5, 1, 1)

#GARCH-M
garchm_EDP_norm <- find_best_GARCH_M(ts_returns_EDP, "norm", 5, 5)
garchm_GALP_norm <- find_best_GARCH_M(ts_returns_GALP, "norm", 5, 5)
garchm_MOTAENGIL_norm <- find_best_GARCH_M(ts_returns_MOTAENGIL, "norm", 5, 5)
garchm_NOS_norm <- find_best_GARCH_M(ts_returns_NOS, "norm", 5, 5)
garchm_NOVABASE_norm <- find_best_GARCH_M(ts_returns_NOVABASE, "norm", 5, 5)




#IMPROVEMENTS

#GARCH
#Cond Dist = snorm
garch_EDP_snorm <- find_best_model(ts_returns_EDP, "fGARCH", "GARCH", "snorm", 5, 5, 0, 0)
garch_GALP_snorm <- find_best_model(ts_returns_GALP, "fGARCH", "GARCH", "snorm", 5, 5, 0, 0)
garch_MOTAENGIL_snorm <- find_best_model(ts_returns_MOTAENGIL, "fGARCH", "GARCH", "snorm", 5, 5, 0, 0)
garch_NOS_snorm <- find_best_model(ts_returns_NOS, "fGARCH", "GARCH", "snorm", 5, 5, 0, 0)
garch_NOVABASE_snorm <- find_best_model(ts_returns_NOVABASE, "fGARCH", "GARCH", "snorm", 5, 5, 0, 0)
#Cond Dist = ged
garch_EDP_ged <- find_best_model(ts_returns_EDP, "fGARCH", "GARCH", "ged", 5, 5, 0, 0)
garch_GALP_ged <- find_best_model(ts_returns_GALP, "fGARCH", "GARCH", "ged", 5, 5, 0, 0)
garch_MOTAENGIL_ged <- find_best_model(ts_returns_MOTAENGIL, "fGARCH", "GARCH", "ged", 5, 5, 0, 0)
garch_NOS_ged <- find_best_model(ts_returns_NOS, "fGARCH", "GARCH", "ged", 5, 5, 0, 0)
garch_NOVABASE_ged <- find_best_model(ts_returns_NOVABASE, "fGARCH", "GARCH", "ged", 5, 5, 0, 0)
#Cond Dist = std
garch_EDP_std <- find_best_model(ts_returns_EDP, "fGARCH", "GARCH", "std", 5, 5, 0, 0)
garch_GALP_std <- find_best_model(ts_returns_GALP, "fGARCH", "GARCH", "std", 5, 5, 0, 0)
garch_MOTAENGIL_std <- find_best_model(ts_returns_MOTAENGIL, "fGARCH", "GARCH", "std", 5, 5, 0, 0)
garch_NOS_std <- find_best_model(ts_returns_NOS, "fGARCH", "GARCH", "std", 5, 5, 0, 0)
garch_NOVABASE_std <- find_best_model(ts_returns_NOVABASE, "fGARCH", "GARCH", "std", 5, 5, 0, 0)


#APARCH
#Cond Dist = snorm
aparch_EDP_snorm <- find_best_model(ts_returns_EDP, "apARCH", NULL, "snorm", 5, 5, 1, 1)
aparch_GALP_snorm <- find_best_model(ts_returns_GALP, "apARCH", NULL, "snorm", 5, 5, 1, 1)
aparch_MOTAENGIL_snorm <- find_best_model(ts_returns_MOTAENGIL, "apARCH", NULL, "snorm", 5, 5, 1, 1)
aparch_NOS_snorm <- find_best_model(ts_returns_NOS, "apARCH", NULL, "snorm", 5, 5, 1, 1)
aparch_NOVABASE_snorm <- find_best_model(ts_returns_NOVABASE, "apARCH", NULL, "snorm", 5, 5, 1, 1)
#Cond Dist = ged
aparch_EDP_ged <- find_best_model(ts_returns_EDP, "apARCH", NULL, "ged", 5, 5, 1, 1)
aparch_GALP_ged <- find_best_model(ts_returns_GALP, "apARCH", NULL, "ged", 5, 5, 1, 1)
aparch_MOTAENGIL_ged <- find_best_model(ts_returns_MOTAENGIL, "apARCH", NULL, "ged", 5, 5, 1, 1)
aparch_NOS_ged <- find_best_model(ts_returns_NOS, "apARCH", NULL, "ged", 5, 5, 1, 1)
aparch_NOVABASE_ged <- find_best_model(ts_returns_NOVABASE, "apARCH", NULL, "ged", 5, 5, 1, 1)
#Cond Dist = std
aparch_EDP_std <- find_best_model(ts_returns_EDP, "apARCH", NULL, "std", 5, 5, 1, 1)
aparch_GALP_std <- find_best_model(ts_returns_GALP, "apARCH", NULL, "std", 5, 5, 1, 1)
aparch_MOTAENGIL_std <- find_best_model(ts_returns_MOTAENGIL, "apARCH", NULL, "std", 5, 5, 1, 1)
aparch_NOS_std <- find_best_model(ts_returns_NOS, "apARCH", NULL, "std", 5, 5, 1, 1)
aparch_NOVABASE_std <- find_best_model(ts_returns_NOVABASE, "apARCH", NULL, "std", 5, 5, 1, 1)


#FIGARCH
#Cond Dist = snorm
figarch_EDP_snorm <- find_best_model(ts_returns_EDP, "fiGARCH", NULL, "snorm", 5, 5, 1, 1)
figarch_GALP_snorm <- find_best_model(ts_returns_GALP, "fiGARCH", NULL, "snorm", 5, 5, 1, 1)
figarch_MOTAENGIL_snorm <- find_best_model(ts_returns_MOTAENGIL, "fiGARCH", NULL, "snorm", 5, 5, 1, 1)
figarch_NOS_snorm <- find_best_model(ts_returns_NOS, "fiGARCH", NULL, "snorm", 5, 5, 1, 1)
figarch_NOVABASE_snorm <- find_best_model(ts_returns_NOVABASE, "fiGARCH", NULL, "snorm", 5, 5, 1, 1)
#Cond Dist = ged
figarch_EDP_ged <- find_best_model(ts_returns_EDP, "fiGARCH", NULL, "ged", 5, 5, 1, 1)
figarch_GALP_ged <- find_best_model(ts_returns_GALP, "fiGARCH", NULL, "ged", 5, 5, 1, 1)
figarch_MOTAENGIL_ged <- find_best_model(ts_returns_MOTAENGIL, "fiGARCH", NULL, "ged", 5, 5, 1, 1)
figarch_NOS_ged <- find_best_model(ts_returns_NOS, "fiGARCH", NULL, "ged", 5, 5, 1, 1)
figarch_NOVABASE_ged <- find_best_model(ts_returns_NOVABASE, "fiGARCH", NULL, "ged", 5, 5, 1, 1)
#Cond Dist = std
figarch_EDP_std <- find_best_model(ts_returns_EDP, "fiGARCH", NULL, "std", 5, 5, 1, 1)
figarch_GALP_std <- find_best_model(ts_returns_GALP, "fiGARCH", NULL, "std", 5, 5, 1, 1)
figarch_MOTAENGIL_std <- find_best_model(ts_returns_MOTAENGIL, "fiGARCH", NULL, "std", 5, 5, 1, 1)
figarch_NOS_std <- find_best_model(ts_returns_NOS, "fiGARCH", NULL, "std", 5, 5, 1, 1)
figarch_NOVABASE_std <- find_best_model(ts_returns_NOVABASE, "fiGARCH", NULL, "std", 5, 5, 1, 1)


#Cond Dist = snorm
igarch_EDP_snorm <- find_best_model(ts_returns_EDP, "iGARCH", NULL, "snorm", 5, 5, 1, 1)
igarch_GALP_snorm <- find_best_model(ts_returns_GALP, "iGARCH", NULL, "snorm", 5, 5, 1, 1)
igarch_MOTAENGIL_snorm <- find_best_model(ts_returns_MOTAENGIL, "iGARCH", NULL, "snorm", 5, 5, 1, 1)
igarch_NOS_snorm <- find_best_model(ts_returns_NOS, "iGARCH", NULL, "snorm", 5, 5, 1, 1)
igarch_NOVABASE_snorm <- find_best_model(ts_returns_NOVABASE, "iGARCH", NULL, "snorm", 5, 5, 1, 1)
#Cond Dist = ged
igarch_EDP_ged <- find_best_model(ts_returns_EDP, "iGARCH", NULL, "ged", 5, 5, 1, 1)
igarch_GALP_ged <- find_best_model(ts_returns_GALP, "iGARCH", NULL, "ged", 5, 5, 1, 1)
igarch_MOTAENGIL_ged <- find_best_model(ts_returns_MOTAENGIL, "iGARCH", NULL, "ged", 5, 5, 1, 1)
igarch_NOS_ged <- find_best_model(ts_returns_NOS, "iGARCH", NULL, "ged", 5, 5, 1, 1)
igarch_NOVABASE_ged <- find_best_model(ts_returns_NOVABASE, "iGARCH", NULL, "ged", 5, 5, 1, 1)
#Cond Dist = std
igarch_EDP_std <- find_best_model(ts_returns_EDP, "iGARCH", NULL, "std", 5, 5, 1, 1)
igarch_GALP_std <- find_best_model(ts_returns_GALP, "iGARCH", NULL, "std", 5, 5, 1, 1)
igarch_MOTAENGIL_std <- find_best_model(ts_returns_MOTAENGIL, "iGARCH", NULL, "std", 5, 5, 1, 1)
igarch_NOS_std <- find_best_model(ts_returns_NOS, "iGARCH", NULL, "std", 5, 5, 1, 1)
igarch_NOVABASE_std <- find_best_model(ts_returns_NOVABASE, "iGARCH", NULL, "std", 5, 5, 1, 1)



#garchm


garchm_EDP_snorm <- find_best_GARCH_M(ts_returns_EDP, "snorm", 5, 5)
garchm_GALP_snorm <- find_best_GARCH_M(ts_returns_GALP, "snorm", 5, 5)
garchm_MOTAENGIL_snorm <- find_best_GARCH_M(ts_returns_MOTAENGIL, "snorm", 5, 5)
garchm_NOS_snorm <- find_best_GARCH_M(ts_returns_NOS, "snorm", 5, 5)
garchm_NOVABASE_snorm <- find_best_GARCH_M(ts_returns_NOVABASE, "snorm", 5, 5)

garchm_EDP_ged <- find_best_GARCH_M(ts_returns_EDP, "ged", 5, 5)
garchm_GALP_ged <- find_best_GARCH_M(ts_returns_GALP, "ged", 5, 5)
garchm_MOTAENGIL_ged <- find_best_GARCH_M(ts_returns_MOTAENGIL, "ged", 5, 5)
garchm_NOS_ged <- find_best_GARCH_M(ts_returns_NOS, "ged", 5, 5)
garchm_NOVABASE_ged <- find_best_GARCH_M(ts_returns_NOVABASE, "ged", 5, 5)

garchm_EDP_std <- find_best_GARCH_M(ts_returns_EDP, "std", 5, 5)
garchm_GALP_std <- find_best_GARCH_M(ts_returns_GALP, "std", 5, 5)
garchm_MOTAENGIL_std <- find_best_GARCH_M(ts_returns_MOTAENGIL, "std", 5, 5)
garchm_NOS_std <- find_best_GARCH_M(ts_returns_NOS, "std", 5, 5)
garchm_NOVABASE_std <- find_best_GARCH_M(ts_returns_NOVABASE, "std", 5, 5)




##########BEST RESULTS EDP###############
garch_EDP_norm[which.min(garch_EDP_norm$V2),]
garch_EDP_snorm[which.min(garch_EDP_snorm$V2),]
garch_EDP_ged[which.min(garch_EDP_ged$V2),]
garch_EDP_std[which.min(garch_EDP_std$V2),]

igarch_EDP_norm[which.min(igarch_EDP_norm$V2),]
igarch_EDP_snorm[which.min(igarch_EDP_snorm$V2),]
igarch_EDP_ged[which.min(igarch_EDP_ged$V2),]
igarch_EDP_std[which.min(igarch_EDP_std$V2),]

garchm_EDP_norm[which.min(garchm_EDP_norm$AIC),]
garchm_EDP_snorm[which.min(garchm_EDP_snorm$AIC),]
garchm_EDP_ged[which.min(garchm_EDP_ged$AIC),]
garchm_EDP_std[which.min(garchm_EDP_std$AIC),]

aparch_EDP_norm[which.min(aparch_EDP_norm$V2),]
aparch_EDP_snorm[which.min(aparch_EDP_snorm$V2),]
aparch_EDP_ged[which.min(aparch_EDP_ged$V2),]
aparch_EDP_std[which.min(aparch_EDP_std$V2),]

figarch_EDP_norm[which.min(figarch_EDP_norm$V2),]
figarch_EDP_snorm[which.min(figarch_EDP_snorm$V2),]
figarch_EDP_ged[which.min(figarch_EDP_ged$V2),]
figarch_EDP_std[which.min(figarch_EDP_std$V2),]

spec_EDP <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1,1), submodel = "GARCH"), 
                               mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                               distribution.model = "std")
EDP_best<-ugarchfit(spec_EDP, ts_returns_EDP, solver = "hybrid")
coef(EDP_best)
residuals_EDP_best<-residuals(EDP_best,standardize = TRUE)
plotacffunc(residuals_EDP_best, "ACF Plot for EDP Residuals")
plotacffunc((residuals_EDP_best)^2, "ACF Plot for EDP Squared Residuals")
ggarrange(plotacffunc(residuals_EDP_best, "ACF Plot for EDP Residuals"),
          plotacffunc((residuals_EDP_best)^2, "ACF Plot for EDP Squared Residuals"))
qqplot_draw(as.numeric(residuals_EDP_best),"Normal Q-Q Plot for EDP Residuals")

runs.test(as.numeric(residuals_EDP_best))
jarque.test(as.numeric(residuals_EDP_best))
shapiro.test(as.numeric(residuals_EDP_best))

##########BEST RESULTS GALP###############
garch_GALP_norm[which.min(garch_GALP_norm$V2),]
garch_GALP_snorm[which.min(garch_GALP_snorm$V2),]
garch_GALP_ged[which.min(garch_GALP_ged$V2),]
garch_GALP_std[which.min(garch_GALP_std$V2),]

igarch_GALP_norm[which.min(igarch_GALP_norm$V2),]
igarch_GALP_snorm[which.min(igarch_GALP_snorm$V2),]
igarch_GALP_ged[which.min(igarch_GALP_ged$V2),]
igarch_GALP_std[which.min(igarch_GALP_std$V2),]

garchm_GALP_norm[which.min(garchm_GALP_norm$AIC),]
garchm_GALP_snorm[which.min(garchm_GALP_snorm$AIC),]
garchm_GALP_ged[which.min(garchm_GALP_ged$AIC),]
garchm_GALP_std[which.min(garchm_GALP_std$AIC),]

aparch_GALP_norm[which.min(aparch_GALP_norm$V2),]
aparch_GALP_snorm[which.min(aparch_GALP_snorm$V2),]
aparch_GALP_ged[which.min(aparch_GALP_ged$V2),]
aparch_GALP_std[which.min(aparch_GALP_std$V2),]

figarch_GALP_norm[which.min(figarch_GALP_norm$V2),]
figarch_GALP_snorm[which.min(figarch_GALP_snorm$V2),]
figarch_GALP_ged[which.min(figarch_GALP_ged$V2),]
figarch_GALP_std[which.min(figarch_GALP_std$V2),]

spec_GALP <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,5), submodel = NULL), 
                       mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                       distribution.model = "ged")
GALP_best<-ugarchfit(spec_GALP, ts_returns_GALP, solver = "hybrid")
GALP_best
residuals_GALP_best<-residuals(GALP_best,standardize = TRUE)
plotacffunc(residuals_GALP_best, "ACF Plot for GALP Residuals")
plotacffunc((residuals_GALP_best)^2, "ACF Plot for GALP Squared Residuals")
ggarrange(plotacffunc(residuals_GALP_best, "ACF Plot for GALP Residuals"),
          plotacffunc((residuals_GALP_best)^2, "ACF Plot for GALP Squared Residuals"))
qqplot_draw(as.numeric(residuals_GALP_best),"Normal Q-Q Plot for GALP Residuals")

runs.test(as.numeric(residuals_GALP_best))
jarque.test(as.numeric(residuals_GALP_best))
shapiro.test(as.numeric(residuals_GALP_best))



##########BEST RESULTS MOTAENGIL###############

garch_MOTAENGIL_norm[which.min(garch_MOTAENGIL_norm$V2),]
garch_MOTAENGIL_snorm[which.min(garch_MOTAENGIL_snorm$V2),]
garch_MOTAENGIL_ged[which.min(garch_MOTAENGIL_ged$V2),]
garch_MOTAENGIL_std[which.min(garch_MOTAENGIL_std$V2),]

igarch_MOTAENGIL_norm[which.min(igarch_MOTAENGIL_norm$V2),]
igarch_MOTAENGIL_snorm[which.min(igarch_MOTAENGIL_snorm$V2),]
igarch_MOTAENGIL_ged[which.min(igarch_MOTAENGIL_ged$V2),]
igarch_MOTAENGIL_std[which.min(igarch_MOTAENGIL_std$V2),]

garchm_MOTAENGIL_norm[which.min(garchm_MOTAENGIL_norm$AIC),]
garchm_MOTAENGIL_snorm[which.min(garchm_MOTAENGIL_snorm$AIC),]
garchm_MOTAENGIL_ged[which.min(garchm_MOTAENGIL_ged$AIC),]
garchm_MOTAENGIL_std[which.min(garchm_MOTAENGIL_std$AIC),]

aparch_MOTAENGIL_norm[which.min(aparch_MOTAENGIL_norm$V2),]
aparch_MOTAENGIL_snorm[which.min(aparch_MOTAENGIL_snorm$V2),]
aparch_MOTAENGIL_ged[which.min(aparch_MOTAENGIL_ged$V2),]
aparch_MOTAENGIL_std[which.min(aparch_MOTAENGIL_std$V2),]

figarch_MOTAENGIL_norm[which.min(figarch_MOTAENGIL_norm$V2),]
figarch_MOTAENGIL_snorm[which.min(figarch_MOTAENGIL_snorm$V2),]
figarch_MOTAENGIL_ged[which.min(figarch_MOTAENGIL_ged$V2),]
figarch_MOTAENGIL_std[which.min(figarch_MOTAENGIL_std$V2),]

spec_MOTAENGIL <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(3,1), submodel = "GARCH"), 
                        mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                        distribution.model = "std")
MOTAENGIL_best<-ugarchfit(spec_MOTAENGIL, ts_returns_MOTAENGIL, solver = "hybrid")
MOTAENGIL_best
residuals_MOTAENGIL_best<-residuals(MOTAENGIL_best,standardize = TRUE)
plotacffunc(residuals_MOTAENGIL_best, "ACF Plot for MOTAENGIL Residuals")
plotacffunc((residuals_MOTAENGIL_best)^2, "ACF Plot for MOTAENGIL Squared Residuals")
ggarrange(plotacffunc(residuals_MOTAENGIL_best, "ACF Plot for MOTAENGIL Residuals"),
          plotacffunc((residuals_MOTAENGIL_best)^2, "ACF Plot for MOTAENGIL Squared Residuals"))
qqplot_draw(as.numeric(residuals_MOTAENGIL_best),"Normal Q-Q Plot for MOTAENGIL Residuals")

runs.test(as.numeric(residuals_MOTAENGIL_best))
jarque.test(as.numeric(residuals_MOTAENGIL_best))
shapiro.test(as.numeric(residuals_MOTAENGIL_best))


##########BEST RESULTS NOS###############
garch_NOS_norm[which.min(garch_NOS_norm$V2),]
garch_NOS_snorm[which.min(garch_NOS_snorm$V2),]
garch_NOS_ged[which.min(garch_NOS_ged$V2),]
garch_NOS_std[which.min(garch_NOS_std$V2),]

igarch_NOS_norm[which.min(igarch_NOS_norm$V2),]
igarch_NOS_snorm[which.min(igarch_NOS_snorm$V2),]
igarch_NOS_ged[which.min(igarch_NOS_ged$V2),]
igarch_NOS_std[which.min(igarch_NOS_std$V2),]

garchm_NOS_norm[which.min(garchm_NOS_norm$AIC),]
garchm_NOS_snorm[which.min(garchm_NOS_snorm$AIC),]
garchm_NOS_ged[which.min(garchm_NOS_ged$AIC),]
garchm_NOS_std[which.min(garchm_NOS_std$AIC),]

aparch_NOS_norm[which.min(aparch_NOS_norm$V2),]
aparch_NOS_snorm[which.min(aparch_NOS_snorm$V2),]
aparch_NOS_ged[which.min(aparch_NOS_ged$V2),]
aparch_NOS_std[which.min(aparch_NOS_std$V2),]

figarch_NOS_norm[which.min(figarch_NOS_norm$V2),]
figarch_NOS_snorm[which.min(figarch_NOS_snorm$V2),]
figarch_NOS_ged[which.min(figarch_NOS_ged$V2),]
figarch_NOS_std[which.min(figarch_NOS_std$V2),]

spec_NOS <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,2), submodel = NULL), 
                       mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                       distribution.model = "std")
NOS_best<-ugarchfit(spec_NOS, ts_returns_NOS, solver = "hybrid")
NOS_best
residuals_NOS_best<-residuals(NOS_best,standardize = TRUE)
plotacffunc(residuals_NOS_best, "ACF Plot for NOS Residuals")
plotacffunc((residuals_NOS_best)^2, "ACF Plot for NOS Squared Residuals")
ggarrange(plotacffunc(residuals_NOS_best, "ACF Plot for NOS Residuals"),
          plotacffunc((residuals_NOS_best)^2, "ACF Plot for NOS Squared Residuals"))
qqplot_draw(as.numeric(residuals_NOS_best),"Normal Q-Q Plot for NOS Residuals")

runs.test(as.numeric(residuals_NOS_best))
jarque.test(as.numeric(residuals_NOS_best))
shapiro.test(as.numeric(residuals_NOS_best))


##########BEST RESULTS NOVABASE###############

garch_NOVABASE_norm[which.min(garch_NOVABASE_norm$V2),]
garch_NOVABASE_snorm[which.min(garch_NOVABASE_snorm$V2),]
garch_NOVABASE_ged[which.min(garch_NOVABASE_ged$V2),]
garch_NOVABASE_std[which.min(garch_NOVABASE_std$V2),]

igarch_NOVABASE_norm[which.min(igarch_NOVABASE_norm$V2),]
igarch_NOVABASE_snorm[which.min(igarch_NOVABASE_snorm$V2),]
igarch_NOVABASE_ged[which.min(igarch_NOVABASE_ged$V2),]
igarch_NOVABASE_std[which.min(igarch_NOVABASE_std$V2),]

garchm_NOVABASE_norm[which.min(garchm_NOVABASE_norm$AIC),]
garchm_NOVABASE_snorm[which.min(garchm_NOVABASE_snorm$AIC),]
garchm_NOVABASE_ged[which.min(garchm_NOVABASE_ged$AIC),]
garchm_NOVABASE_std[which.min(garchm_NOVABASE_std$AIC),]

aparch_NOVABASE_norm[which.min(aparch_NOVABASE_norm$V2),]
aparch_NOVABASE_snorm[which.min(aparch_NOVABASE_snorm$V2),]
aparch_NOVABASE_ged[which.min(aparch_NOVABASE_ged$V2),]
aparch_NOVABASE_std[which.min(aparch_NOVABASE_std$V2),]

figarch_NOVABASE_norm[which.min(figarch_NOVABASE_norm$V2),]
figarch_NOVABASE_snorm[which.min(figarch_NOVABASE_snorm$V2),]
figarch_NOVABASE_ged[which.min(figarch_NOVABASE_ged$V2),]
figarch_NOVABASE_std[which.min(figarch_NOVABASE_std$V2),]

spec_NOVABASE <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1), submodel = NULL), 
                       mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                       distribution.model = "std")
NOVABASE_best<-ugarchfit(spec_NOVABASE, ts_returns_NOVABASE, solver = "hybrid")
NOVABASE_best
residuals_NOVABASE_best<-residuals(NOVABASE_best,standardize = TRUE)
plotacffunc(residuals_NOVABASE_best, "ACF Plot for NOVABASE Residuals")
plotacffunc((residuals_NOVABASE_best)^2, "ACF Plot for NOVABASE Squared Residuals")
ggarrange(plotacffunc(residuals_NOVABASE_best, "ACF Plot for NOVABASE Residuals"),
           plotacffunc((residuals_NOVABASE_best)^2, "ACF Plot for NOVABASE Squared Residuals"))
qqplot_draw(as.numeric(residuals_NOVABASE_best),"Normal Q-Q Plot for NOVABASE Residuals")

runs.test(as.numeric(residuals_NOVABASE_best))
jarque.test(as.numeric(residuals_NOVABASE_best))
shapiro.test(as.numeric(residuals_NOVABASE_best))




