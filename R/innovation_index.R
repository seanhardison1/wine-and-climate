library(tidyverse)
library(magrittr)
d <- read.csv(here::here("data","bearing_CA_grapes.csv"))

# https://www.wineinstitute.org/resources/statistics/article88

innov_ind = d %>%
  mutate_all(as.character) %>% 
  mutate(Reported.Bearing = as.numeric(str_remove(Reported.Bearing, ",")),
         Reported.Non.bearing = as.numeric(str_remove(Reported.Non.bearing, ",")),
         Efficiency = Reported.Bearing/Reported.Non.bearing,
         Year = as.numeric(Year))


ggplot(data = innov_ind) +
  geom_line(aes(x = Year, y = Efficiency))
