# install.packages("lavaan")
# install.packages("ggplot2")
# install.packages("semPlot")
# install.packages("lavaanPlot")
# install.packages("dplyr")
# install.packages("psych")
# install.packages("wesanderson")
# install.packages("piecewiseSEM")
# install.packages("nlme")
# install.packages("lme4")
# install.packages("mgcv")
# install.packages("spdep")
# install.packages("lubridate")
# install.packages("car")
# install.packages("lmtest")
# install.packages("astsa")
# install.packages("plm")

library(lavaan)
library(ggplot2)
library(semPlot)
library(lavaanPlot)
library(dplyr)
library(psych)
library(wesanderson)
library(piecewiseSEM)
library(nlme)
library(lme4)
library(mgcv)
library(spdep)
library(lubridate)
library(zoo)
library(car)
library(lmtest)
library(astsa)
library(plm)

version <- "v0.8"

# Read DATA
admin1_d_dt <- read.csv("Data/model_dataset_admin1_D_date.csv")

admin1_d_dt <- admin1_d_dt[with(admin1_d_dt, order(admin1Pcod_2, date_2)), ]
admin1_d_dt$date_2 <- as.Date(admin1_d_dt$date_2)

# Weekday, Weekend of not
admin1_d_dt <- admin1_d_dt %>%
  mutate(weekday = wday(date_2))
admin1_d_dt <- admin1_d_dt %>%
  mutate(weekend = as.integer(weekday %in% c(1, 7)))

admin1_d_dt$new_cases <- ifelse(admin1_d_dt$new_cases < 0, 0, admin1_d_dt$new_cases)

# new cases and trips per 1 million of destination population
admin1_d_dt <- admin1_d_dt %>%
  mutate(new_cases.per1M = new_cases * 1000000 / Population)
admin1_d_dt <- admin1_d_dt %>%
  mutate(admin1_inflow_trip_count.per1M = admin1_inflow_trip_count * 1000000 / Population)
admin1_d_dt <- admin1_d_dt %>%
  mutate(admin1_inbound_admin2_crossbound_trip_count.per1M = admin1_inbound_admin2_crossbound_trip_count * 1000000 / Population)
admin1_d_dt <- admin1_d_dt %>%
  mutate(admin2_inbound_trip_count.per1M = admin2_inbound_trip_count * 1000000 / Population)

# Calculate cumulative sum of cases
# Sort the data frame by 'date_col' and 'index_col' in ascending order
admin1_d_dt <- admin1_d_dt %>%
  arrange(date_2, admin1Pcod_2)
admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(cum_cases = cumsum(new_cases))

# calculate log of cases
min_value <- min(admin1_d_dt$new_cases)
admin1_d_dt <- admin1_d_dt %>%
  mutate(log.new_cases = log(new_cases - min_value + 1))

min_value <- min(admin1_d_dt$new_cases.per1M)
admin1_d_dt <- admin1_d_dt %>%
  mutate(log.new_cases.per1M = log(new_cases.per1M - min_value + 1))

# calculate log for trips
admin1_d_dt <- admin1_d_dt %>%
  mutate(log.admin1_inflow_trip_count = log(admin1_inflow_trip_count + 1))
admin1_d_dt <- admin1_d_dt %>%
  mutate(log.admin1_inbound_admin2_crossbound_trip_count = log(admin1_inbound_admin2_crossbound_trip_count + 1))
admin1_d_dt <- admin1_d_dt %>%
  mutate(log.admin2_inbound_trip_count = log(admin2_inbound_trip_count + 1))
admin1_d_dt <- admin1_d_dt %>%
  mutate(log.admin1_inflow_trip_count.per1M = log((admin1_inflow_trip_count + 1) * 1000000 / Population))
admin1_d_dt <- admin1_d_dt %>%
  mutate(log.admin1_inbound_admin2_crossbound_trip_count.per1M = log((admin1_inbound_admin2_crossbound_trip_count + 1) * 1000000 / Population))
admin1_d_dt <- admin1_d_dt %>%
  mutate(log.admin2_inbound_trip_count.per1M = log((admin2_inbound_trip_count + 1) * 1000000 / Population))

# calculate moving average of daily cases
admin1_d_dt$MA7.new_cases <- rollmean(admin1_d_dt$new_cases, k = 7, fill = NA)
admin1_d_dt$MA7.new_cases.per1M <- rollmean(admin1_d_dt$new_cases.per1M, k = 7, fill = NA)
admin1_d_dt$MA7.log.new_cases.per1M <- rollmean(admin1_d_dt$log.new_cases.per1M, k = 7, fill = NA)

print(colnames(admin1_d_dt))

# lag by 1, 7, 8 steps
admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.trips.person = dplyr::lag(trips.person, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.trips.person = dplyr::lag(trips.person, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.trip_miles.person = dplyr::lag(trip_miles.person, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.trip_miles.person = dplyr::lag(trip_miles.person, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.new_cases = dplyr::lag(new_cases, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.new_cases = dplyr::lag(new_cases, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.new_cases = dplyr::lag(new_cases, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.new_cases.per1M = dplyr::lag(new_cases.per1M, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.new_cases.per1M = dplyr::lag(new_cases.per1M, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.new_cases.per1M = dplyr::lag(new_cases.per1M, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.log.new_cases.per1M = dplyr::lag(log.new_cases.per1M, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.log.new_cases.per1M = dplyr::lag(log.new_cases.per1M, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.log.new_cases.per1M = dplyr::lag(log.new_cases.per1M, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l3.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 3, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l4.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 4, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l28.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 28, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l27.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 27, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l18.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 18, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l15.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 15, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l16.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 16, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l17.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 17, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l24.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 24, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l21.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 21, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l22.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 22, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l23.MA7.log.new_cases.per1M = dplyr::lag(MA7.log.new_cases.per1M, n = 23, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.cum_cases = dplyr::lag(cum_cases, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.admin1_inflow_trip_count = dplyr::lag(admin1_inflow_trip_count, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.admin1_inflow_trip_count = dplyr::lag(admin1_inflow_trip_count, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.admin1_inflow_trip_count = dplyr::lag(admin1_inflow_trip_count, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.log.admin1_inflow_trip_count = dplyr::lag(log.admin1_inflow_trip_count, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.log.admin1_inflow_trip_count = dplyr::lag(log.admin1_inflow_trip_count, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.log.admin1_inflow_trip_count = dplyr::lag(log.admin1_inflow_trip_count, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.admin1_inbound_admin2_crossbound_trip_count = dplyr::lag(admin1_inbound_admin2_crossbound_trip_count, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.admin1_inbound_admin2_crossbound_trip_count = dplyr::lag(admin1_inbound_admin2_crossbound_trip_count, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.admin1_inbound_admin2_crossbound_trip_count = dplyr::lag(admin1_inbound_admin2_crossbound_trip_count, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.log.admin1_inbound_admin2_crossbound_trip_count = dplyr::lag(log.admin1_inbound_admin2_crossbound_trip_count, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.log.admin1_inbound_admin2_crossbound_trip_count = dplyr::lag(log.admin1_inbound_admin2_crossbound_trip_count, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.log.admin1_inbound_admin2_crossbound_trip_count = dplyr::lag(log.admin1_inbound_admin2_crossbound_trip_count, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.admin2_inbound_trip_count = dplyr::lag(admin2_inbound_trip_count, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.admin2_inbound_trip_count = dplyr::lag(admin2_inbound_trip_count, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.admin2_inbound_trip_count = dplyr::lag(admin2_inbound_trip_count, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.log.admin2_inbound_trip_count = dplyr::lag(log.admin2_inbound_trip_count, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.log.admin2_inbound_trip_count = dplyr::lag(log.admin2_inbound_trip_count, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.log.admin2_inbound_trip_count = dplyr::lag(log.admin2_inbound_trip_count, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.admin1_inflow_trip_count.per1M = dplyr::lag(admin1_inflow_trip_count.per1M, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l3.admin1_inflow_trip_count.per1M = dplyr::lag(admin1_inflow_trip_count.per1M, n = 3, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.admin1_inflow_trip_count.per1M = dplyr::lag(admin1_inflow_trip_count.per1M, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.admin1_inflow_trip_count.per1M = dplyr::lag(admin1_inflow_trip_count.per1M, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.admin1_inbound_admin2_crossbound_trip_count.per1M = dplyr::lag(admin1_inbound_admin2_crossbound_trip_count.per1M, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l3.admin1_inbound_admin2_crossbound_trip_count.per1M = dplyr::lag(admin1_inbound_admin2_crossbound_trip_count.per1M, n = 3, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.admin1_inbound_admin2_crossbound_trip_count.per1M = dplyr::lag(admin1_inbound_admin2_crossbound_trip_count.per1M, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.admin1_inbound_admin2_crossbound_trip_count.per1M = dplyr::lag(admin1_inbound_admin2_crossbound_trip_count.per1M, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l16.admin1_inbound_admin2_crossbound_trip_count.per1M = dplyr::lag(admin1_inbound_admin2_crossbound_trip_count.per1M, n = 16, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.admin2_inbound_trip_count.per1M = dplyr::lag(admin2_inbound_trip_count.per1M, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.admin2_inbound_trip_count.per1M = dplyr::lag(admin2_inbound_trip_count.per1M, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.admin2_inbound_trip_count.per1M = dplyr::lag(admin2_inbound_trip_count.per1M, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l18.admin2_inbound_trip_count.per1M = dplyr::lag(admin2_inbound_trip_count.per1M, n = 18, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l22.admin2_inbound_trip_count.per1M = dplyr::lag(admin2_inbound_trip_count.per1M, n = 22, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.log.admin1_inflow_trip_count.per1M = dplyr::lag(log.admin1_inflow_trip_count.per1M, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l3.log.admin1_inflow_trip_count.per1M = dplyr::lag(log.admin1_inflow_trip_count.per1M, n = 3, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l4.log.admin1_inflow_trip_count.per1M = dplyr::lag(log.admin1_inflow_trip_count.per1M, n = 4, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.log.admin1_inflow_trip_count.per1M = dplyr::lag(log.admin1_inflow_trip_count.per1M, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.log.admin1_inflow_trip_count.per1M = dplyr::lag(log.admin1_inflow_trip_count.per1M, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.log.admin1_inbound_admin2_crossbound_trip_count.per1M = dplyr::lag(log.admin1_inbound_admin2_crossbound_trip_count.per1M, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l3.log.admin1_inbound_admin2_crossbound_trip_count.per1M = dplyr::lag(log.admin1_inbound_admin2_crossbound_trip_count.per1M, n = 3, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.log.admin1_inbound_admin2_crossbound_trip_count.per1M = dplyr::lag(log.admin1_inbound_admin2_crossbound_trip_count.per1M, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.log.admin1_inbound_admin2_crossbound_trip_count.per1M = dplyr::lag(log.admin1_inbound_admin2_crossbound_trip_count.per1M, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l12.log.admin1_inbound_admin2_crossbound_trip_count.per1M = dplyr::lag(log.admin1_inbound_admin2_crossbound_trip_count.per1M, n = 12, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l13.log.admin1_inbound_admin2_crossbound_trip_count.per1M = dplyr::lag(log.admin1_inbound_admin2_crossbound_trip_count.per1M, n = 13, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l16.log.admin1_inbound_admin2_crossbound_trip_count.per1M = dplyr::lag(log.admin1_inbound_admin2_crossbound_trip_count.per1M, n = 16, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l17.log.admin1_inbound_admin2_crossbound_trip_count.per1M = dplyr::lag(log.admin1_inbound_admin2_crossbound_trip_count.per1M, n = 17, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l1.log.admin2_inbound_trip_count.per1M = dplyr::lag(log.admin2_inbound_trip_count.per1M, n = 1, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.log.admin2_inbound_trip_count.per1M = dplyr::lag(log.admin2_inbound_trip_count.per1M, n = 7, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l8.log.admin2_inbound_trip_count.per1M = dplyr::lag(log.admin2_inbound_trip_count.per1M, n = 8, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l9.log.admin2_inbound_trip_count.per1M = dplyr::lag(log.admin2_inbound_trip_count.per1M, n = 9, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l10.log.admin2_inbound_trip_count.per1M = dplyr::lag(log.admin2_inbound_trip_count.per1M, n = 10, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l18.log.admin2_inbound_trip_count.per1M = dplyr::lag(log.admin2_inbound_trip_count.per1M, n = 18, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l22.log.admin2_inbound_trip_count.per1M = dplyr::lag(log.admin2_inbound_trip_count.per1M, n = 22, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l23.log.admin2_inbound_trip_count.per1M = dplyr::lag(log.admin2_inbound_trip_count.per1M, n = 23, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.lockdown = dplyr::lag(lockdown, n = 7, default = NA))
admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l12.lockdown = dplyr::lag(lockdown, n = 12, default = NA))
admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l9.lockdown = dplyr::lag(lockdown, n = 9, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l3.lockdown = dplyr::lag(lockdown, n = 3, default = NA))
admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l16.lockdown = dplyr::lag(lockdown, n = 16, default = NA))
admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l22.lockdown = dplyr::lag(lockdown, n = 22, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l7.weekend = dplyr::lag(weekend, n = 7, default = NA))
admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l12.weekend = dplyr::lag(weekend, n = 12, default = NA))
admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l9.weekend = dplyr::lag(weekend, n = 9, default = NA))

admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l3.weekend = dplyr::lag(weekend, n = 3, default = NA))
admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l16.weekend = dplyr::lag(weekend, n = 16, default = NA))
admin1_d_dt <- admin1_d_dt %>%
  group_by(admin1Pcod_2) %>%
  mutate(l22.weekend = dplyr::lag(weekend, n = 22, default = NA))

start_date <- as.Date("2020-03-05")
max_day <- as.numeric(difftime(max(admin1_d_dt$date_2), start_date, units = "days"))

colSums(is.na(admin1_d_dt))

# define columns
x_cols <- c(
"rwi", 
"X0_14_pct", 
"X65p_pct", 
"M_pct", 
# "lockdown_1", "lockdown_2", "lockdown_3", "lockdown_4", "lockdown_5", 
"first_case", 
"ban_travel", 
"lockdown", 
"weekend", 
# "admin1_inflow_trip_count", 
"admin1_inflow_trip_count.per1M", 
# "admin1_inbound_admin2_crossbound_trip_count", 
"admin1_inbound_admin2_crossbound_trip_count.per1M", 
# "admin2_inbound_trip_count", 
"admin2_inbound_trip_count.per1M", 
# "log.admin1_inflow_trip_count", 
"log.admin1_inflow_trip_count.per1M", 
# "log.admin1_inbound_admin2_crossbound_trip_count", 
"log.admin1_inbound_admin2_crossbound_trip_count.per1M", 
# "log.admin2_inbound_trip_count", 
"log.admin2_inbound_trip_count.per1M"
)

index_cols <- c("admin1Name_2", "admin1Pcod_2")

y_cols <- c("new_cases", "new_cases.per1M", "log.new_cases.per1M", "log.new_cases")

lagged_cols <- c(
    # "l1.trip_miles.person",
    # "l7.trip_miles.person",
    # "l1.trips.person",
    # "l7.trips.person",
    # "l1.new_cases",
    # "l7.new_cases",
    # "l8.new_cases",
    "MA7.new_cases", 
    "MA7.new_cases.per1M", 
    "MA7.log.new_cases.per1M", 
    "l1.new_cases.per1M",
    "l7.new_cases.per1M",
    "l8.new_cases.per1M",
    "l1.log.new_cases.per1M",
    "l7.log.new_cases.per1M",
    "l8.log.new_cases.per1M",
    "l1.MA7.log.new_cases.per1M",
    "l3.MA7.log.new_cases.per1M",
    "l7.MA7.log.new_cases.per1M",
    "l8.MA7.log.new_cases.per1M",
    "l28.MA7.log.new_cases.per1M",
    "l27.MA7.log.new_cases.per1M",
    "l18.MA7.log.new_cases.per1M",
    "l15.MA7.log.new_cases.per1M",
    "l16.MA7.log.new_cases.per1M",
    "l21.MA7.log.new_cases.per1M",
    "l24.MA7.log.new_cases.per1M",
    "l22.MA7.log.new_cases.per1M",
    "l23.MA7.log.new_cases.per1M",
    "l1.admin1_inflow_trip_count", 
    "l7.admin1_inflow_trip_count",
    "l8.admin1_inflow_trip_count", 
    "l1.admin1_inbound_admin2_crossbound_trip_count",
    "l7.admin1_inbound_admin2_crossbound_trip_count",
    "l8.admin1_inbound_admin2_crossbound_trip_count",
    "l1.admin2_inbound_trip_count",
    "l7.admin2_inbound_trip_count",
    "l8.admin2_inbound_trip_count",
    "l1.log.admin1_inflow_trip_count", 
    "l7.log.admin1_inflow_trip_count",
    "l8.log.admin1_inflow_trip_count", 
    "l1.log.admin1_inbound_admin2_crossbound_trip_count",
    "l7.log.admin1_inbound_admin2_crossbound_trip_count",
    "l8.log.admin1_inbound_admin2_crossbound_trip_count",
    "l1.log.admin2_inbound_trip_count",
    "l7.log.admin2_inbound_trip_count",
    "l8.log.admin2_inbound_trip_count",
    "l1.admin1_inflow_trip_count.per1M",
    "l3.admin1_inflow_trip_count.per1M",
    "l7.admin1_inflow_trip_count.per1M",
    "l8.admin1_inflow_trip_count.per1M",
    "l1.admin1_inbound_admin2_crossbound_trip_count.per1M",
    "l7.admin1_inbound_admin2_crossbound_trip_count.per1M",
    "l8.admin1_inbound_admin2_crossbound_trip_count.per1M",
    "l16.admin1_inbound_admin2_crossbound_trip_count.per1M",
    "l1.admin2_inbound_trip_count.per1M",
    "l7.admin2_inbound_trip_count.per1M",
    "l8.admin2_inbound_trip_count.per1M",
    "l22.admin2_inbound_trip_count.per1M",
    "l1.log.admin1_inflow_trip_count.per1M",
    "l3.log.admin1_inflow_trip_count.per1M",
    "l4.log.admin1_inflow_trip_count.per1M",
    "l7.log.admin1_inflow_trip_count.per1M",
    "l8.log.admin1_inflow_trip_count.per1M",
    "l1.log.admin1_inbound_admin2_crossbound_trip_count.per1M",
    "l3.log.admin1_inbound_admin2_crossbound_trip_count.per1M",
    "l7.log.admin1_inbound_admin2_crossbound_trip_count.per1M",
    "l8.log.admin1_inbound_admin2_crossbound_trip_count.per1M",
    "l12.log.admin1_inbound_admin2_crossbound_trip_count.per1M",
    "l13.log.admin1_inbound_admin2_crossbound_trip_count.per1M",
    "l16.log.admin1_inbound_admin2_crossbound_trip_count.per1M",
    "l17.log.admin1_inbound_admin2_crossbound_trip_count.per1M",
    "l1.log.admin2_inbound_trip_count.per1M",
    "l7.log.admin2_inbound_trip_count.per1M",
    "l8.log.admin2_inbound_trip_count.per1M",
    "l9.log.admin2_inbound_trip_count.per1M",
    "l10.log.admin2_inbound_trip_count.per1M",
    "l18.log.admin2_inbound_trip_count.per1M",
    "l22.log.admin2_inbound_trip_count.per1M",
    "l23.log.admin2_inbound_trip_count.per1M",
    "l7.lockdown",
    "l12.lockdown",
    "l9.lockdown",
    "l7.weekend",
    "l12.weekend",
    "l9.weekend", 
    "l3.lockdown",
    "l16.lockdown",
    "l22.lockdown",
    "l3.weekend",
    "l16.weekend",
    "l22.weekend"
)

time_window <- 120
AllAdmin1 <- unique(admin1_d_dt$admin1Pcod_2)
Prov <- c()
ccount <- 1

for (ii in AllAdmin1) {
  start_date <- as.Date("2020-03-05")
  for (jj in (1:(max_day - time_window))) {
    temp_trips <- admin1_d_dt[
      (admin1_d_dt$date_2 < start_date + time_window) &
        (admin1_d_dt$date_2 >= start_date) & 
        (admin1_d_dt$admin1Pcod_2 == ii), 
    ]
    model <- lm(
      log.new_cases.per1M ~ 
        l3.log.admin1_inflow_trip_count.per1M +
        l16.log.admin1_inbound_admin2_crossbound_trip_count.per1M +
        l22.log.admin2_inbound_trip_count.per1M +
        l1.log.new_cases.per1M +
        weekend +
        rwi +
        # X0_14_pct +
        X65p_pct +
        M_pct,
      data = temp_trips
    )
    
    residuals <- residuals(model)
    dw_v <- car::durbinWatsonTest(residuals)
    
    if (!(is.na(dw_v))) { Prov[[ccount]] <- c(ii, jj, start_date, dw_v) }
    else { Prov[[ccount]] <- c(ii, jj, start_date, 0) }

    ccount <- ccount + 1
    start_date <- start_date + 1
  }
}


# Extract the 4th element from each sublist
fourth_elements <- sapply(Prov, function(x) x[4])

# Convert to numeric
fourth_elements <- as.numeric(fourth_elements)

# Calculate statistics
mean_value <- mean(fourth_elements, na.rm = TRUE)
median_value <- median(fourth_elements, na.rm = TRUE)
sd_value <- sd(fourth_elements, na.rm = TRUE)
var_value <- var(fourth_elements, na.rm = TRUE)
summary_stats <- summary(fourth_elements)
# Calculate the 10th and 90th percentiles
p10 <- quantile(fourth_elements, 0.10, na.rm = TRUE)
p90 <- quantile(fourth_elements, 0.90, na.rm = TRUE)

# Print the results
print(paste("Mean:", mean_value))
print(paste("Median:", median_value))
print(paste("Standard Deviation:", sd_value))
print(paste("Variance:", var_value))
print(paste("10th Percentile:", p10))
print(paste("90th Percentile:", p90))
print("Summary Statistics:")
print(summary_stats)

ecdf_function <- ecdf(fourth_elements)
quantile_value <- ecdf_function(1.5)
print(quantile_value)

ecdf_function <- ecdf(fourth_elements)
quantile_value <- ecdf_function(2.5)
print(quantile_value)

ecdf_function <- ecdf(fourth_elements)
quantile_value <- ecdf_function(1)
print(quantile_value)

ecdf_function <- ecdf(fourth_elements)
quantile_value <- ecdf_function(3)
print(quantile_value)

# Data description
options(scipen = 999)
result <- describe(admin1_d_dt)
result[, c("mean", "sd", "min", "max")]

# Filter for writing of "Results" section
summary(admin1_d_dt$admin2_inbound_trip_count.per1M)
summary(admin1_d_dt$admin1_inbound_admin2_crossbound_trip_count.per1M)
summary(admin1_d_dt$admin1_inflow_trip_count.per1M)
summary(admin1_d_dt$new_cases.per1M)

start_date <- as.Date("2020-05-28")
end_date <- as.Date("2020-07-13")
# Use filter() to slice the dataframe by date
filtered_df <- admin1_d_dt %>%
  filter(date_2 >= start_date, date_2 <= end_date)
summary(filtered_df$admin2_inbound_trip_count.per1M)
summary(filtered_df$new_cases.per1M)

