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

version <- "v0.11"

# Read DATA
root_path <- "."
admin1_d_dt <- read.csv(paste0(root_path, "/Data/model_dataset_admin1_D_date.csv"))

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
admin1_d_dt$MA7.new_cases <- rollmean(admin1_d_dt$new_cases, align = "right", k = 7, fill = NA)
admin1_d_dt$MA7.new_cases.per1M <- rollmean(admin1_d_dt$new_cases.per1M, align = "right", k = 7, fill = NA)
admin1_d_dt$MA7.log.new_cases.per1M <- rollmean(admin1_d_dt$log.new_cases.per1M, align = "right", k = 7, fill = NA)

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
    "l4.MA7.log.new_cases.per1M",
    "l7.MA7.log.new_cases.per1M",
    "l8.MA7.log.new_cases.per1M",
    "l28.MA7.log.new_cases.per1M",
    "l27.MA7.log.new_cases.per1M",
    "l18.MA7.log.new_cases.per1M",
    "l15.MA7.log.new_cases.per1M",
    "l16.MA7.log.new_cases.per1M",
    "l17.MA7.log.new_cases.per1M",
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

# describe the selected columns
data_descp <- data.frame(describe(subset(admin1_d_dt, select = c(y_cols, x_cols))))
write.csv(data_descp, file=sprintf(paste0(root_path, "/Results/data_description_%s.csv"), version))

# SEM PANEL MODEL
# A function for all provinces
all_admin1_SEM_Panel <- function(max_day, admin1_d_dt, time_window) {
  # RUN LOOP
  all_corr <- c()
  all_perform <- c()
  rd_effc_model_2 <- c()
  rd_effc_model_3 <- c()
  rd_effc_model_4 <- c()
  rd_se_model_2 <- c()  # New list for random effect standard deviations
  rd_se_model_3 <- c()
  rd_se_model_4 <- c()
  start_date <- as.Date("2020-03-05")
  for (jj in (1:(max_day - time_window))) {
    print(jj)

    skip_to_next <- FALSE
    temp_trips <- admin1_d_dt[
      (admin1_d_dt$date_2 < start_date + time_window) &
        (admin1_d_dt$date_2 >= start_date),
    ]
    temp_trips <- subset(temp_trips, select = c(index_cols, y_cols, x_cols, lagged_cols))

    temp_trips$admin1Pcod_2 <- as.factor(temp_trips$admin1Pcod_2)
    temp_trips <- na.omit(temp_trips)
    rownames(temp_trips) <- NULL

    tryCatch({
      model_list <- list(
        lme(
          log.new_cases.per1M ~ 
            l3.log.admin1_inflow_trip_count.per1M +
            l16.log.admin1_inbound_admin2_crossbound_trip_count.per1M +
            l22.log.admin2_inbound_trip_count.per1M +
            l1.log.new_cases.per1M +
            weekend +
            rwi +
            X65p_pct +
            M_pct,
          random = ~1 | admin1Pcod_2, 
          na.action = na.omit, 
          data = temp_trips),
        lme(
          l3.log.admin1_inflow_trip_count.per1M ~ 
            l4.MA7.log.new_cases.per1M +
            l4.log.admin1_inflow_trip_count.per1M +
            rwi +
            X65p_pct +
            M_pct +
            l3.weekend,
          random = ~1 | l3.lockdown/admin1Pcod_2, 
          na.action = na.omit,
          data = temp_trips), 
        lme(
          l16.log.admin1_inbound_admin2_crossbound_trip_count.per1M ~ 
            l17.MA7.log.new_cases.per1M +
            l17.log.admin1_inbound_admin2_crossbound_trip_count.per1M +
            rwi +
            X65p_pct +
            M_pct +
            l16.weekend,
          random = ~1 | l16.lockdown/admin1Pcod_2, 
          na.action = na.omit,
          data = temp_trips), 
        lme(
          l22.log.admin2_inbound_trip_count.per1M ~ 
            l23.MA7.log.new_cases.per1M +
            l23.log.admin2_inbound_trip_count.per1M +
            rwi +
            X65p_pct +
            M_pct +
            l22.weekend,
          random = ~1 | l22.lockdown/admin1Pcod_2, 
          na.action = na.omit,
          data = temp_trips)
      )
      fit <- as.psem(model_list)
      new_summary <- summary(fit, .progressBar = F, rsq = T)
      para <- coefs(fit, standardize = "scale", intercepts = TRUE)
      para$date_2 <- start_date
      perf <- new_summary$R2
      perf$date_2 <- start_date
      all_corr[[jj]] <- para
      all_perform[[jj]] <- perf

      # Get random effects of sub-models
      # ranef_1 <- ranef(model_list[[1]])
      ranef_2 <- as.data.frame(ranef(model_list[[2]])[1])
      ranef_3 <- as.data.frame(ranef(model_list[[3]])[1])
      ranef_4 <- as.data.frame(ranef(model_list[[4]])[1])

      ranef_2$date_2 <- start_date
      ranef_3$date_2 <- start_date
      ranef_4$date_2 <- start_date
      ranef_2$sub_model <- "model_2"
      ranef_3$sub_model <- "model_3"
      ranef_4$sub_model <- "model_4"
      ranef_2$lockdown_level <- row.names(ranef_2)
      ranef_3$lockdown_level <- row.names(ranef_3)
      ranef_4$lockdown_level <- row.names(ranef_4)

      # Get random effects and approximate their standard errors with nlme
      # Model 2
      ranef_2_lvl2 <- ranef(model_list[[2]], level = 2)  # Predicted random effects for innermost level
      vc_2 <- VarCorr(model_list[[2]])  # Variance components
      intercept_rows_2 <- which(rownames(vc_2) == "(Intercept)")
      sigma_2 <- as.numeric(vc_2[intercept_rows_2[2], "StdDev"])  # StdDev for admin1Pcod_2      n_groups_2 <- nrow(ranef_2)  # Number of groups
      n_groups_2 <- nrow(ranef_2_lvl2)
      # Approximate SE: sigma / sqrt(n), where n is number of observations per group (simplified)
      # For simplicity, assume equal SE across groups based on variance
      se_2 <- sigma_2 / sqrt(nrow(temp_trips) / n_groups_2)

      # Model 3
      ranef_3_lvl2 <- ranef(model_list[[3]], level = 2)
      vc_3 <- VarCorr(model_list[[3]])
      intercept_rows_3 <- which(rownames(vc_3) == "(Intercept)")
      sigma_3 <- as.numeric(vc_3[intercept_rows_3[2], "StdDev"])
      n_groups_3 <- nrow(ranef_3_lvl2)
      se_3 <- sigma_3 / sqrt(nrow(temp_trips) / n_groups_3)

      # Model 4
      ranef_4_lvl2 <- ranef(model_list[[4]], level = 2)
      vc_4 <- VarCorr(model_list[[4]])
      intercept_rows_4 <- which(rownames(vc_4) == "(Intercept)")
      sigma_4 <- as.numeric(vc_4[intercept_rows_4[2], "StdDev"])
      n_groups_4 <- nrow(ranef_4_lvl2)
      se_4 <- sigma_4 / sqrt(nrow(temp_trips) / n_groups_4)

      ranef_2$se <- se_2
      ranef_3$se <- se_3
      ranef_4$se <- se_4

      rd_effc_model_2[[jj]] <- ranef_2
      rd_effc_model_3[[jj]] <- ranef_3
      rd_effc_model_4[[jj]] <- ranef_4

    },
    error = function(e) { skip_to_next <<- TRUE })
    start_date <- start_date + 1
    if (skip_to_next) { next }
  }
  all_corr <- do.call(rbind.data.frame, all_corr)
  all_perform <- do.call(rbind.data.frame, all_perform)
  rd_effc_model_2 <- do.call(rbind.data.frame, rd_effc_model_2)
  rd_effc_model_3 <- do.call(rbind.data.frame, rd_effc_model_3)
  rd_effc_model_4 <- do.call(rbind.data.frame, rd_effc_model_4)

  return(list(all_corr, all_perform, rd_effc_model_2, rd_effc_model_3, rd_effc_model_4))
}

time_window_set <- 120
results <- all_admin1_SEM_Panel(max_day, admin1_d_dt, time_window_set)

write.csv(results[[1]], sprintf(paste0(root_path, '/Results/SEM_coefs_%s_%s.csv'), time_window_set, version))
write.csv(results[[2]], sprintf(paste0(root_path, '/Results/SEM_perf_%s_%s.csv'), time_window_set, version))
write.csv(results[[3]], sprintf(paste0(root_path, '/Results/sub_model2_rd_effc_%s_%s.csv'), time_window_set, version))
write.csv(results[[4]], sprintf(paste0(root_path, '/Results/sub_model3_rd_effc_%s_%s.csv'), time_window_set, version))
write.csv(results[[5]], sprintf(paste0(root_path, '/Results/sub_model4_rd_effc_%s_%s.csv'), time_window_set, version))
