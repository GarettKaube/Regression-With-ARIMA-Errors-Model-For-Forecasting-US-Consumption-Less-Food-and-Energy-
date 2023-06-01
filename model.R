library(tidyverse)
library(fredr)
library(fpp2)
library(dplyr)
library(mFilter)
library(pls)
library(urca)
library(forecast)
library(ggthemes)
library(tseries)
library(lubridate)

# set key
fredr_set_key("........")

# include the covid shock or not
include_covid = TRUE

if (include_covid){
  test_end = c(2023,4)
} else {
  test_end =  c(2020,1)
}


# Consumption less Food and Energy
consumption_ = fredr(series_id = "DPCCRC1M027SBEA", observation_start = as.Date("1976-01-01"), frequency = 'm') %>%
  select(-c(series_id, realtime_start, realtime_end)) %>%
  rename(consumption = value)

consumption_no_date=consumption_ %>% select(-c(date))

consumption_ts = ts(consumption_no_date, start=c(1976,7), end = c(2023,3), frequency = 12)

# plot original series
autoplot(consumption_ts, color = 'deepskyblue') +
  ylab('Personal Consumption')+
  ggtitle('Personal Consumption over Time') +
  theme_minimal()+
  theme(plot.title = element_text(size=20,hjust = 0.5))
ggsave('yo.jpeg',dpi=300)

# log differenced series
autoplot(log(consumption_ts) %>% diff(), color = 'deepskyblue') + ylab('Consumption Growth') +
  ggtitle('Change in Log Consumption Monthly') +
  theme_minimal()+
  theme(plot.title = element_text(size=20,hjust = 0.5))
ggsave('diff.jpeg',dpi=300)
# train test split

ggAcf(log(consumption_ts) %>% diff(), color='red')
ggPacf(log(consumption_ts) %>% diff(), color='red')
ggsave('Pacf.jpeg',dpi=300)

log(consumption_ts) %>% diff() %>% adf.test()

diff_cons = log(consumption_ts) %>% diff()
  
test = window(diff_cons,start = c(2013,3), test_end)
#---------------------Regressors---------------------------------

source('data.R')

#########################################################################
#---------------------------MODELLING-----------------------------#
#########################################################################

#---------------------Train-Test-Data----------------------------

start = c(1982,2)


# variables to be included in model
subset = c("inflation", 
           "unemployment_ts", 
           "real_interest_ts", 
           "savings_amount", 
           "savings", 
           "disp_inc")


data_full = merge(consumption_, regressors, by='date', all.y = T) %>%
  select(-c(date))
data_full_ts = ts(data_full, start=start,frequency = 12)


train_reg = window(data_full_ts, start = start, end = c(2013,2))
test_reg = window(data_full_ts, start = c(2013,3),end=test_end)

# log difference target
consumption_train = log(train_reg[,1]) %>% diff()

xreg_train = as.matrix(as.data.frame(train_reg[,subset]))
xreg_train = xreg_train[2:nrow(xreg_train), ]

#---------------------Dynamic Regression----------------------------

#---Auto Arima------
dym_auto = auto.arima(consumption_train, xreg = xreg_train, ic = c("aic"))
summary(dym_auto)
checkresiduals(dym_auto, theme =theme_economist()) 
ggsave('resid.png', dpi=300)

autoplot(fitted(dym_auto), color = 'deepskyblue', label='Fitted') + autolayer(consumption_train) +
  theme_minimal() +
  ylab('Fitted Values') +
  ggtitle('Fitted Values')

#########################################################################
#---------------------------MODEL SELECTION-----------------------------#
#########################################################################

order_ = c(2,0,2)
seasonal_ = c(1,0,0)

# Cross validation
dym_fore = function(x, h, xreg, newxreg) {
  forecast(Arima(x, order=order_, seasonal = seasonal_, xreg=xreg), xreg=newxreg)
}

cv = tsCV(consumption_train, dym_fore, h=5, xreg = xreg_train)

# CV score
(score = mean(cv**2, na.rm = T))



#########################################################################
#----------------------------TESTING------------------------------------#
#########################################################################

# forecasts
xreg_test = as.matrix(as.data.frame(test_reg[,subset]))

#test_ = Arima(consumption_train, order=c(3,0,1))

dynamic_forecast = forecast(dym_auto, h=nrow(xreg_test), xreg = xreg_test)
cons = log(data_full_ts[,1]) %>% diff()
autoplot(dynamic_forecast, include = 10) + autolayer(window(cons,start = c(2000,1)), color = 'red' ) + 
  theme_minimal() +
  ylab('Log Consumption Growth')
ggsave('cov_fc.jpeg', dpi=300)
autoplot(cons, color='red')


#Train set MSE
res = residuals(dym_auto)

train_mse = sum(res**2) / length(res)
train_mse

(test_mse = sum((dynamic_forecast$mean - test)**2) / length(dynamic_forecast$mean))


# h=2 forecast
dynamic_forecast_future = forecast(dym_auto, h=2, xreg = xreg_test)
autoplot(dynamic_forecast_future, include = 1) + autolayer(window(cons,start = c(2020,1)), color = 'red' ) + 
  theme_minimal() +
  ylab('Log Consumption Growth')
ggsave('future_fc.jpeg', dpi=300)

# undo transform
untransformed_fc = exp(dynamic_forecast_future$mean)
autoplot(untransformed_fc, include = 1) + autolayer(window(exp(cons),start = c(2020,1)), color = 'red' ) + 
  theme_minimal() +
  ylab('Log Consumption Growth') +
  ggtitle('Untransformed Forcasts') +
  theme(plot.title = element_text(size=20,hjust = 0.5)) +
ggsave('fc.jpeg', dpi=300)


