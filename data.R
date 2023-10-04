library(tidyverse)
library(fredr)
library(fpp2)
library(dplyr)
library(mFilter)
library(pls)
library(urca)
library(forecast)
library(readxl)
library(tseries)
library(lubridate)
library(zoo)

# initialization
# lagged variables
lag_ = TRUE

fredr_set_key("........")

#------------------------2 year treasury----------------------


TWO_YEAR_TREASURY = fredr(series_id = "GS2", observation_start = as.Date("1976-06-01"), frequency = 'm') %>%
  select(-c(series_id, realtime_start, realtime_end)) %>%
  rename(yield_two_year = value) %>% 
  select(-c(date))
#TWO_YEAR_TREASURY = drop_na(TWO_YEAR_TREASURY)


TWO_YEAR_TREASURY_daily = fredr(series_id = "DGS2", observation_start = as.Date("1976-06-01"), frequency = 'd') %>%
  select(-c(series_id, realtime_start, realtime_end)) %>%
  rename(yield_two_year = value) %>% 
  select(-c(date))

latest_daily = TWO_YEAR_TREASURY_daily[nrow(TWO_YEAR_TREASURY_daily),]
TWO_YEAR_TREASURY = rbind(TWO_YEAR_TREASURY, latest_daily)

TWO_YEAR_TREASURY_ts = ts(TWO_YEAR_TREASURY, start=c(1976,6), frequency = 12)


autoplot(TWO_YEAR_TREASURY_ts, color='deepskyblue') + theme_minimal() 

TWO_YEAR_TREASURY_ts_diff = TWO_YEAR_TREASURY_ts %>% diff()
TWO_YEAR_TREASURY_ts_diff %>% adf.test()
autoplot(TWO_YEAR_TREASURY_ts_diff) +theme_light()

ggAcf(TWO_YEAR_TREASURY_ts_diff)
ggPacf(TWO_YEAR_TREASURY_ts_diff)

#latest_daily = TWO_YEAR_TREASURY_daily[nrow(TWO_YEAR_TREASURY_daily),]
#TWO_YEAR_TREASURY_ts[length(TWO_YEAR_TREASURY_ts)] = latest_daily

#------------------------Real-Interest---------------------
real_interest = fredr(series_id = "REAINTRATREARAT1YE", observation_start = as.Date("1982-01-01"), frequency = 'm') %>%
  select(-c(series_id, realtime_start, realtime_end)) %>%
  rename(real_interest = value) %>% 
  select(-c(date))


real_interest_ts = ts(real_interest, start=c(1982,1), frequency = 12)
real_interest_ts  %>% adf.test() 

ri_decomp = mstl(real_interest_ts)
real_interest_ts = real_interest_ts - seasonal(ri_decomp)


#------------------------Population----------------------

POP = fredr(series_id = "POPTHM", observation_start = as.Date("1976-06-01"), frequency = 'm') %>%
  select(-c(series_id, realtime_start, realtime_end)) %>%
  rename(population = value) %>% 
  select(-c(date))

pop_ts = ts(POP[[1]], start=c(1976,6), frequency = 12)
autoplot(pop_ts,color='red') +theme_light()

decomp_pop = mstl(pop_ts)
pop_ts = pop_ts - seasonal(decomp_pop)

pop_ts_diff = pop_ts %>% log() %>% diff()%>% diff()
autoplot(pop_ts_diff,color='red') +theme_light()

pop_ts_diff%>% adf.test()
ggAcf(pop_ts_diff)
ggPacf(pop_ts_diff)


#-------------------Real_GDP-----------------------------------
real_gdp = fredr(series_id = "GDPC1", observation_start = as.Date("1960-06-01"), frequency = 'q') %>%
  select(-c(series_id, realtime_start, realtime_end)) %>%
  rename(real_gdp = value) %>% 
  select(-c(date))

real_gdp_ts = ts(real_gdp, start = c(1960,4), frequency = 4)
autoplot(real_gdp_ts)

real_gdp_ts_diff = real_gdp_ts %>% log() %>% diff() %>% stats::lag(k=-1)
real_gdp_ts_diff %>% adf.test()
autoplot(real_gdp_ts_diff)
ggAcf(real_gdp_ts_diff)
ggPacf(real_gdp_ts_diff)
#-------------------------CPI---------------------------------
inflation = fredr(series_id = "CPILFESL", observation_start = as.Date("1960-06-01"), frequency = 'm') %>%
  select(-c(series_id, realtime_start, realtime_end)) %>%
  rename(inflation = value) %>% 
  select(-c(date))

inflation_ts = ts(inflation, start = c(1960,6), frequency = 12)

inflation_ts_diff = inflation_ts %>% log() %>% diff()%>% diff() %>% stats::lag(k=-1)
inflation_ts_diff %>% adf.test()
autoplot(inflation_ts_diff)

#--------------------savings-rate-------------------------------
savings = fredr(series_id = "PSAVERT", observation_start = as.Date("1960-06-01"), frequency = 'm') %>%
  select(-c(series_id, realtime_start, realtime_end)) %>%
  rename(savings = value) %>% 
  select(-c(date))

savings_ts = ts(savings, start = c(1960,6), frequency = 12)
autoplot(savings_ts)
autoplot(log(savings_ts))

savings_ts_diff = savings_ts

autoplot(savings_ts_diff)

savings_ts_diff %>%  adf.test()

#-----------------------------------Savings-------------------------------------
savings_amount = fredr(series_id = "PMSAVE", observation_start = as.Date("1960-06-01"), frequency = 'm') %>%
  select(-c(series_id, realtime_start, realtime_end)) %>%
  rename(savings_amount = value) %>% 
  select(-c(date))

savings_amount_ts = ts(savings_amount, start = c(1960,6), frequency = 12)
autoplot(log(savings_amount_ts))
savings_amount_ts_diff = log(savings_amount_ts) %>% diff()
savings_amount_ts_diff %>% adf.test()

#-----------------------------------Treasury spread-----------------------------
t_spread = fredr(series_id = "T10Y2Y", observation_start = as.Date("1976-06-01"), frequency = 'd') %>%
  select(-c(series_id, realtime_start, realtime_end)) %>%
  rename(t_spread = value)  

t_spread[, 'month'] = lapply(t_spread[, 'date'], FUN = as.Date)
t_spread[, 'month'] = lapply(t_spread[, 'month'], FUN = month)

t_spread[, 'year'] = lapply(t_spread[, 'date'], FUN = as.Date)
t_spread[, 'year'] = lapply(t_spread[, 'year'], FUN = year)

t_spread = drop_na(t_spread)

last_obs = group_by(t_spread, year, month) %>% 
  summarise(t_spread = last(t_spread))

t_spread_ts = last_obs %>% 
  ungroup() %>% 
  select(-c(year, month)) %>% 
  ts(start = c(1976,6), frequency = 12)

autoplot(t_spread_ts)

t_spread_ts_diff = t_spread_ts %>% diff() 
autoplot(t_spread_ts_diff) + theme_light()

t_spread_ts_diff %>% adf.test()

#---------------------------Unemployment-----------------------------------------
unemployment = fredr(series_id = "UNRATENSA", observation_start = as.Date("1976-06-01"), frequency = 'm') %>%
  select(-c(series_id, realtime_start, realtime_end)) %>%
  rename(unemployment = value) %>% 
  select(-c(date))


unemployment_ts = ts(unemployment, start = c(1976,6), frequency = 12)
autoplot(unemployment_ts)

decomp_un = mstl(unemployment_ts)

unemployment_ts = unemployment_ts - seasonal(decomp_un)

unemployment_ts_diff = diff(unemployment_ts)
unemployment_ts_diff %>% adf.test()

autoplot(unemployment_ts_diff, color='red')

#------------------------------Government Expenditure--------------------------
gov = fredr(series_id = "W068RCQ027SBEA", observation_start = as.Date("1976-01-01"), frequency = 'q') %>%
  select(-c(series_id, realtime_start, realtime_end)) %>%
  rename(gov_expenditure = value) %>% 
  select(-c(date))

gov_ts = ts(gov, start=c(1976,1), frequency = 4)
autoplot(gov_ts)
gov_ts = gov_ts %>% log() %>% diff() 
gov_ts %>% adf.test()
autoplot(gov_ts)
#--------------------------Disposable Income-------------------------------------
Disposable_inc = fredr(series_id = "DSPIC96", observation_start = as.Date("1976-06-01"), frequency = 'm') %>%
  select(-c(series_id, realtime_start, realtime_end)) %>%
  rename(disp_inc = value) %>% 
  select(-c(date))


Disposable_inc_ts = ts(Disposable_inc, start = c(1976,6),frequency = 12)

Disposable_inc_ts = Disposable_inc_ts %>% log() %>% diff()
Disposable_inc_ts%>% adf.test()
autoplot(Disposable_inc_ts)

#------------------------------------------------------------------------------

# lag variables
if (lag_==TRUE){
  real_interest_ts =  real_interest_ts %>% stats::lag(k=-1)
  unemployment_ts_diff = unemployment_ts_diff %>% stats::lag(k=-1)
  pop_ts_diff = pop_ts_diff %>% stats::lag(k=-2)
  real_gdp_ts_diff = real_gdp_ts_diff %>% stats::lag(k=-1)
  inflation_ts_diff = inflation_ts_diff %>% stats::lag(k=-1)
  savings_amount_ts_diff = stats::lag(savings_amount_ts_diff,k=-2)
  gov_ts = gov_ts %>% stats::lag(k=-1)
  savings_ts_diff_lag = savings_ts_diff %>% stats::lag(k=-3)
  Disposable_inc_ts =  Disposable_inc_ts %>% stats::lag(k=-2)
}




# data frames (Very bad and ugly code)
pop_ts_diff_df = data.frame(population=as.matrix(pop_ts_diff), date=as.Date(as.yearmon(time(pop_ts_diff))))
TWO_YEAR_TREASURY_ts_diff_df = data.frame(interest_rate=as.matrix(TWO_YEAR_TREASURY_ts_diff), date=as.Date(as.yearmon(time(TWO_YEAR_TREASURY_ts_diff))))
real_gdp_ts_diff_df = data.frame(real_gdp=as.matrix(real_gdp_ts_diff), date=as.Date(as.yearmon(time(real_gdp_ts_diff))))
inflation_df = data.frame(inflation=as.matrix(inflation_ts_diff), date=as.Date(as.yearmon(time(inflation_ts_diff))))
savings_df = data.frame(savings=as.matrix(savings_ts_diff_lag), date=as.Date(as.yearmon(time(savings_ts_diff_lag))))
yield_spread_df = data.frame(t_spread=as.matrix(t_spread_ts_diff), date=as.Date(as.yearmon(time(t_spread_ts_diff))))
unemployment_df = data.frame(unemployment=as.matrix(unemployment_ts_diff), date=as.Date(as.yearmon(time(unemployment_ts_diff))))
real_interest_df = data.frame(real_interest=as.matrix(real_interest_ts), date=as.Date(as.yearmon(time(real_interest_ts))))
savings_amount_df = data.frame(savings_amount=as.matrix(savings_amount_ts_diff), date=as.Date(as.yearmon(time(savings_amount_ts_diff))))
gov_df = data.frame(gov_exp=as.matrix(gov_ts), date=as.Date(as.yearmon(time(gov_ts))))
dis_income_df = data.frame(disp_inc=as.matrix(Disposable_inc_ts), date=as.Date(as.yearmon(time(Disposable_inc_ts))))

df_list = list(pop_ts_diff_df,TWO_YEAR_TREASURY_ts_diff_df, inflation_df, savings_df, 
               yield_spread_df, unemployment_df, real_interest_df,savings_amount_df, dis_income_df)

df_qlist = list(real_gdp_ts_diff_df, gov_df)

#merge data
regressors = Reduce(function(x, y) merge(x, y, by='date'), df_list) %>%
  merge(real_gdp_ts_diff_df, by='date', all.x=T, all.y=T) %>% 
  tidyr::fill(real_gdp,.direction = "down") %>% 
  merge(gov_df, by='date', all.x=T, all.y=T) %>% 
  tidyr::fill(gov_expenditure,.direction = "down") %>% 
  drop_na()


# some plots

autoplot(Disposable_inc_ts, color='deepskyblue') + 
  theme_minimal() +
  ggtitle("Disposable Income") +
  theme(plot.title = element_text(size=20,hjust = 0.5))
ggsave('dis.jpeg',dpi=300)

regressors_ts = regressors %>% select(-c(date)) %>% ts(start = c(1982,2), frequency = 12)


autoplot(regressors_ts[,c(9,10,11)], facets = TRUE, color = 'deepskyblue')  + 
  theme_minimal() +
  ylab('') +
  ggtitle("Transformed Data") +
  theme(plot.title = element_text(size=20,hjust = 0.5))
ggsave('facet2.jpeg',dpi=600)
