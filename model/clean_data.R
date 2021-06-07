rm(list=ls())

library(tidyverse)
library(data.table)

dat <- fread(file = './data/sa_covid_stats.csv')

dat_country <- dat[ , lapply(.SD,sum,na.rm=T), by=c("country_name","date"), .SDcols=c(9:25)]
dat_subregion <- dat[ , lapply(.SD,sum,na.rm=T), by=c("country_name","subregion1_name","date"), .SDcols=c(9:25)]

dat_country_t <- tibble(dat_country)
dat_subregion_t <- tibble(dat_subregion)

saveRDS(dat_country_t, file="./data/country_agg.rds")
saveRDS(dat_subregion_t, file="./data/subregion_agg.rds")
