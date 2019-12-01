library(tidyverse)
library(sf)
library(raster)
library(magrittr)
#load in data
load(here::here("data/central_coast_stations.rdata"))

files <- list.files(here::here("data/central_coast_stations"))
#riem weather data
cc_weather_raw <- NULL
for (i in 1:length(files)){
  assign("cc_weather_raw",rbind(read_csv(here::here("data/central_coast_stations",
                                                  files[i])),
                                    cc_weather_raw))
}

cc_weather_raw %<>% dplyr::rename(year = `year(valid)`,
                                  month = `month(valid)`,
                                  day = `day(valid)`) 



annual_weather <- cc_weather_raw %>% 
  group_by(year) %>% 
  dplyr::summarise_at(vars(tmpf:mslp), mean, na.rm = T)

ggplot(data = annual_weather) +
  geom_line(aes(x = year, y = tmpf)) 

seasonal_weather <- cc_weather_raw %>% 
  mutate(season = case_when(month %in% c(1, 2, 3) ~ "winter",
                            month %in% c(4,5,6) ~ "spring",
                            month %in% c(7,8,9) ~ "summer",
                            month %in% c(10,11,12) ~ "fall")) %>% 
  group_by(year, season) %>% 
  dplyr::summarise_at(vars(tmpf:mslp), mean, na.rm = T)

ggplot(data = seasonal_weather) +
  geom_line(aes(x = year, y = tmpf, color = season)) 
