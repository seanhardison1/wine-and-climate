library(tidyverse)
library(magrittr)
library(ggeffects)
library(GGally)

calwines <- 
  readr::read_csv(here::here("data/winemag-data_first150k.csv")) %>% 
  filter(province == "California")


unique(calwines$region_2)

sonoma <- readr::read_csv(here::here("data","sonoma_weather_1952_pres.csv"))
plot(sonoma$Date, sonoma$`Air max` - sonoma$min, type = "l")
