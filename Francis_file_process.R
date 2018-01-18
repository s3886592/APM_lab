library(tidyverse)
library(stringr)
library(modelr)
library(cowplot)
library(lubridate)

inflow <- read_tsv("Francis_inflow.txt", comment = "#")
outflow <- read_tsv("Francis_outflow.txt", comment = "#")

inflow <- inflow %>%
  select(datetime, `110815_00060`)

outflow <- outflow %>%
  select(datetime,`110816_00060`)

sta_datetime <- parse_datetime("2014-03-01 00:00:00")
end_datetime <- parse_datetime("2014-06-30 23:59:00")
int <- interval(sta_datetime, end_datetime)

inflow <- inflow %>%
  filter(datetime %within% int)
outflow <- outflow %>%
  filter(datetime %within% int)

names(inflow) <- c("time", "inflow")
names(outflow) <- c("time", "outflow")

inflow_check <- inflow %>%
  mutate(minutes = minute(time))

inflow$time[length(inflow$time)] - inflow$time[1]

ts <- seq(sta_datetime, end_datetime, by = "2 min")
ts <- data.frame(time = ts,
                 inflow2 = rep(0, length(ts)))
ts <- ts %>%
  full_join(inflow, by = "time") %>%
  mutate(inflow = replace(inflow, is.na(inflow), 0)) %>%
  arrange(time) %>%
  as_tibble()

# CFS to 101.94064776 m3/h
# 101.94064776 / Surface area * 1000 = mm/h
ts <- ts %>%
  mutate(rain = inflow + inflow2,
         rain = rain*339.8022) %>%
  select(-inflow, -inflow2)

write_data <- function(data, file_name, time_format = "%m/%d/%Y %H:%M") {
  data <- data %>%
    mutate(time = format(time, time_format))
  write.table(
    data,
    file = file_name,
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE ,
    sep = "  "
  )
}

write_data(ts, "Francis_rain.csv")

outflow <- outflow %>%
  mutate(outflow = outflow*339.8022 ) # convert to mm/h
write_data(outflow, "Francis_outflow.csv")

28.316846592 # CFS to LPS
