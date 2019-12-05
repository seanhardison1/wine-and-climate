library(tidyverse)
library(sf)
library(lubridate)
library(magrittr)

load(here::here("data/north_coast_climate.rdata"))

loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}



#All analyses derived from Jones and Goodrich 2008
one <- loadRData(here::here("data/north_coast_stations/O69_ASOS.rdata"))
two <- loadRData(here::here("data/north_coast_stations/STS_ASOS.rdata"))
three <- loadRData(here::here("data/north_coast_stations/DVO_ASOS.rdata"))
four <- loadRData(here::here("data/north_coast_stations/O69_ASOS.rdata"))
five <- loadRData(here::here("data/north_coast_stations/O69_ASOS.rdata"))
north_coast_weather <- rbind(one, two, three, four, five)

#summarise the source data set
riem_summ <- north_coast_weather %>% 
  mutate_at(vars(tmpf_min:tmpf_avg),weathermetrics::fahrenheit.to.celsius) %>% 
  group_by(year = `year(valid)`, month = `month(valid)`, day = `day(valid)`) %>% 
  summarise(tmin = mean(tmpf_min, na.rm = T),
            tmax = mean(tmpf_max, na.rm = T),
            tavg = mean(tmpf_avg, na.rm = T)) %>% 
  as.data.frame() %>% 
  dplyr::filter(year >= 1970)

#develop climate indices-----------------------------------

#growing season:
#tavg = avg growing season temp
#tmax_avg = avg growing season max temp
#tmin_avg = avg growing season min temp
gs_clim_indices <- riem_summ %>% 
  filter(month %in% c(4:10)) %>% 
  group_by(year) %>% 
  dplyr::summarise(tavg_gs = mean(tavg),
                   tmax_avg_gs = mean(tmax, na.rm = T),
                   tmin_avg_gs = mean(tmin, na.rm = T))

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
  dplyr::summarise(tavg_rp = mean(tavg, na.rm = T))

#growing degree days with base = 10 degC:
# gdd = sum(avg_daily_temp - base); gdd > 0
base <- 10

gdd <- riem_summ %>% 
  filter(month %in% c(4:10)) %>% 
  group_by(year, month, day) %>% 
  mutate(gdd = tavg - base) %>% 
  mutate(gdd = ifelse(gdd < 0, 0, gdd)) %>% 
  group_by(year) %>% 
  dplyr::summarise(gdd = sum(gdd))

#annual frost days between june-july
frost_days <- 
  riem_summ %>% 
  filter(tmin < 0) %>% 
  group_by(year) %>% 
  dplyr::summarise(frost_days = n())

clim_indices <- gs_clim_indices %>% 
  left_join(., rp_clim_indices, by = "year") %>% 
  left_join(., gdd, by = "year") %>% 
  left_join(.,frost_days, by = "year") %>% 
  mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
  mutate_if(is.numeric, list(~na_if(., -Inf)))
save(clim_indices, file = here::here("data/north_coast_climate_indices.rdata"))
