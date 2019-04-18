library(tseries)
library(tidyverse)
library(vars)
library(tsDyn)
library(readxl)
library(LSTS)
library(stats)
library(forecast)

file.choose()
data<- read_excel("/Users/maria/Desktop/учеба /эконометрика II/PS.xls")%>% mutate(log_Euro = log(Euro), log_SP500 = log(SP500))

#a) (30 points) Consider the daily logarithmic exchange rates (i.e. ln(st)).
#i) Calculate the first 50 autocorrelations for the daily logarithmic exchange rates and their first 
#differences (i.e. rt = ln(st)−ln(st−1). Is the daily logarithmic exchange rate a stationary process?
#Is the series of the exchange rate returns stationary? Use ADF and Phillips- Perron tests and be specific 
#about your choices of the test specifications.

plot(data$log_Euro)
first_diff_log_Euro <- diff(data$log_Euro, differences = 1)
acf(data$log_Euro, lag.max = 50, type = c("correlation"), plot = TRUE)
acf(first_diff_log_Euro, lag.max = 50, type = c("correlation"), plot = TRUE)

adf.test(data$log_Euro)
adf.test(first_diff_log_Euro)

PP.test(data$log_Euro, lshort = FALSE)
PP.test(first_diff_log_Euro, lshort = FALSE)

## ii) Perform the Ljung-Box test for 20, 40, and 60 lags of rt. What’s your conclusion?

Box.test(first_diff_log_Euro, lag = 20, type = c("Ljung-Box"))
Box.test(first_diff_log_Euro, lag = 40, type = c("Ljung-Box"))
Box.test(first_diff_log_Euro, lag = 60, type = c("Ljung-Box"))

## iii) Following the Box-Jenkins procedure, determine the most appropriate model for rt. 
#(Do not forget to perform the Ljung-Box test for the residuals of the chosen model). 
#Report the values of AIC and BIC for the candidate models.

par(mfrow=c(2,1))
acf(first_diff_log_Euro)
pacf(first_diff_log_Euro)

MA1 <- arima(first_diff_log_Euro, order = c(0, 0, 1))
AIC_BIC <- data.frame(AIC(MA1), BIC(MA1))
AIC_BIC
Box.test(first_diff_log_Euro, type = c("Ljung-Box"))



# b) Repeat the exercise for the daily logarithmic S&P500 prices.

plot(data$log_SP500)
first_diff_log_SP500 <- diff(data$log_SP500, differences = 1)
plot(first_diff_log_SP500, type = "l")
acf(data$log_SP500, lag.max = 50, type = c("correlation"), plot = TRUE)
acf(first_diff_log_SP500, lag.max = 50, type = c("correlation"), plot = TRUE)

adf.test(data$log_SP500)
adf.test(first_diff_log_SP500)

PP.test(data$log_SP500, lshort = FALSE)
PP.test(first_diff_log_SP500, lshort = FALSE)

Box.test(first_diff_log_SP500, lag = 20, type = c("Ljung-Box"))
Box.test(first_diff_log_SP500, lag = 40, type = c("Ljung-Box"))
Box.test(first_diff_log_SP500, lag = 60, type = c("Ljung-Box"))

par(mfrow=c(1,1))
acf(first_diff_log_SP500, lag.max = 10)
pacf(first_diff_log_SP500, lag.max = 10)

MA2 <- arima(first_diff_log_SP500, order = c(0, 0, 2))
MA2
ARMA1_1 <- auto.arima(first_diff_log_SP500)
ARMA1_1
AIC_BIC2 <- data.frame(AIC(MA2), BIC(MA2), AIC(ARMA1_1), BIC(ARMA1_1))
AIC_BIC2
Box.test(first_diff_log_SP500, type = c("Ljung-Box"))

# c) i) Do you expect the processes for the logarithmic exchange rate and the logarithmic S&P500 prices to be cointegrated?

# ii) Determine how many lags to use in a VAR/VECM model for the logarithmic exchange rates and 
#logarithmic S&P500 prices by us- ing the information criteria. Based on that, perform the maximum 
#eigenvalue test. What is your conclusion?

new_data <- data.frame(log_Euro = data$log_Euro, log_SP500 = data$log_SP500)
Optimal_lags <- VARselect(new_data)$selection
Optimal_lags

VECM_model <- VECM(new_data, lag = 3, r = 1, estim = "ML")
rank.test(VECM_model, type = c("eigen"), r_null = 1, cval = 0.05)

## iii) Next, estimate the cointegrating coefficient by the dynamic OLS. 
#Set p = 30 and the number of lags to use for HAC as 0.75T1/3. Is it significant?
p <- 30
lags_HAC <- 0.75*3518^(1/3)

### d) Is there any evidence that the logarithmic exchange rates Granger-cause the logarithmic S&P500 prices? and vice versa?
var <- VAR(new_data, p = 3, type = "const")
log_Euro_GrangerCause <- causality(var, cause = "log_Euro")$Granger
log_SP500_GrangerCause <- causality(var, cause = "log_SP500")$Granger
log_Euro_GrangerCause
log_SP500_GrangerCause

