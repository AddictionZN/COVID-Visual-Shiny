rm(list=ls())
# Libraries ----
library(tidyverse)
library(tidyquant)
library(scales)
library(forecast)
library(ggplot2)


num_forecasts = 6

dat <- readRDS(file = "./data/subregion_agg.rds")

dat <- dat %>%
  filter(subregion1_name %in% c("Free State")) %>%
  group_by(subregion1_name, date) %>%
  select(date, subregion1_name, "new_confirmed") %>%
  set_names(c("date", "province", "metric")) %>%
  arrange(date)

create_forecast <- function(dat, num_forecasts){
  name_country <- unique(dat$province)
  auto_forecast <-  forecast(auto.arima(dat$metric),num_forecasts)$mean
  max_date <- max(dat$date)
  new_dates <- max_date + c(1:num_forecasts)
  new_forecast <- tibble( province = name_country, date = new_dates , metric = as.vector(auto_forecast), forecast = 1 )
  return(new_forecast)
}

create_forecast(dat, num_forecasts)

predictions_by_country <- function(){
  dat %>%
    group_by(province) %>%
    group_map(~ create_forecast(.x, num_forecasts=6), .keep=T )
}

predictions_by_country()


forecast_data <- function(){
  unforecasted_data <- dat
  unforecasted_data$forecast <- 0
  forecasted_data <- predictions_by_country()
  forecasted_data <- do.call(rbind,forecasted_data)
  rbind(unforecasted_data,forecasted_data) 
}

foo <- forecast_data()
foo


ggplot( data = forecast_data() %>% filter(forecast==0), aes(y = metric, x = date, color=province) ) +
  geom_line(size = 1.5) +
  geom_line(data = forecast_data() %>% filter(forecast==1),size = 2.5,linetype=7,alpha=0.25) +
  scale_y_continuous( label=comma) +
  theme_bw() +
  geom_vline(xintercept= forecast_data() %>% filter(forecast==0) %>% pull(date) %>% max, linetype="dotdash",size=0.5)

