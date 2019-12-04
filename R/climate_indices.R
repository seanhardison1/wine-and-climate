library(tidyverse)
library(sf)
library(lubridate)
library(magrittr)

load(here::here("data/north_coast_climate.rdata"))

#All analyses derived from Jones and Goodrich 2008

#summarise the source data set
riem_summ <- north_coast_weather %>% 
  mutate(tmpf = weathermetrics::fahrenheit.to.celsius(tmpf)) %>% 
  group_by(year = `year(valid)`, month = `month(valid)`, day = `day(valid)`) %>% 
  summarise(tmin = min(tmpf, na.rm = T),
            tmax = max(tmpf, na.rm = T),
            precip = mean(p01i, na.rm = T)) %>% 
  as.data.frame() %>% 
  dplyr::filter(year > 1971)

#develop climate indices-----------------------------------

#growing season:
#tavg = avg growing season temp
#tmax_avg = avg growing season max temp
#tmin_avg = avg growing season min temp
gs_clim_indices <- riem_summ %>% 
  filter(month %in% c(4:10)) %>% 
  group_by(year) %>% 
  dplyr::summarise(tavg = mean(c(tmin, tmax), na.rm = T),
                   tmax_avg = mean(tmax, na.rm = T),
                   tmin_avg = mean(tmin, na.rm = T))

#ripening period:
# tavg = avg ripening period temp
rp_clim_indices <- 
  riem_summ %>% 
  filter(month %in% 4:10) %>%
  mutate(flag = ifelse(month == 4 & day >= 15, "aprgs",
                       ifelse(month == 10 & day <= 15, "octgs", 
                        ifelse(month %in% 5:9, "gs", NA)))) %>% 
  filter(!is.na(flag)) %>% 
  dplyr::select(-flag) %>% 
  group_by(year) %>% 
  dplyr::summarise(tavg = mean(c(tmin, tmax), na.rm = T))

#growing degree days with base = 10 degC:
# gdd = sum(avg_daily_temp - base); gdd > 0
base <- 10

gdd <- riem_summ %>% 
  filter(month %in% c(4:10)) %>% 
  group_by(year, month, day) %>% 
  dplyr::summarise(gdd = mean(c(tmin, tmax), na.rm = T) - base) %>% 
  mutate(gdd = ifelse(gdd < 0, 0, gdd)) %>% 
  group_by(year) %>% 
  dplyr::summarise(gdd = sum(gdd))
