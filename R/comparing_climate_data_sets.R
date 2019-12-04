library(tidyverse)
library(sf)
library(lubridate)

load(here::here("data/north_coast_climate.rdata"))

#All analyses derived from Jones and Goodrich 2008
test <- readr::read_csv(here::here("data/1961766.csv"))

#Comparing the RIEM data to the NOAA historical climate database
test2 <- test %>% 
  dplyr::select(DATE, ncei_tmax = TMAX, ncei_tmin = TMIN) %>% 
  as.data.frame() %>% 
  dplyr::filter(DATE >= "1971-01-01") %>% 
  mutate(year = year(DATE), month = month(DATE), day = day(DATE))

riem_dat <- north_coast_weather %>% 
  mutate(tmpf = weathermetrics::fahrenheit.to.celsius(tmpf)) %>% 
  group_by(year = `year(valid)`, month = `month(valid)`, day = `day(valid)`) %>% 
  summarise(riem_tmin = min(tmpf, na.rm = T),
               riem_tmax = max(tmpf, na.rm = T)) %>% 
  as.data.frame() %>% 
  dplyr::filter(year > 1971)


all_dats <- inner_join(test2, riem_dat, by = c("year","month","day"))
plot(ncei_tmin ~ riem_tmin, data = all_dats)
plot(ncei_tmax ~ riem_tmax, data = all_dats)
plot(ncei_tmax ~ DATE, data = all_dats, type = "l")
points(riem_tmax ~ DATE, data = all_dats, type = "l", col = "red")
