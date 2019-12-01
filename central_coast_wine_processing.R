library(tidyverse)
library(sf)
library(magrittr)

load(here::here("data/central_coast_wines.rdata"))

varieties <- c("(syrah\\-grenache)","(grenache\\-syrah)","(shiraz\\-viognier)",
               "(syrah\\-petite sirah)","merlot",
               "cabernet sauvignon","ner veltliner",
               "zinfandel","petite sirah", "petite verdot",
               "cabernet franc","sauvignon blanc","petit verdot",
               "rosé","malbec","rose","sparkling","sangiovese",
               "chardonnay","grenache", "syrah","tempranillo","riesling",
               "pinot grigio","gewürztraminer",
               "pinot noir", "pinot gris","meritage",
               " red ", " white ", "sylvaner","pinot blanc",
               "fumé blanc")

t <- central_coast_wines %>% 
  filter(str_detect(variety, "https")) %>% 
  mutate(titles = tolower(titles)) %>% 
  mutate(variety = str_extract(titles, 
                               str_flatten(varieties, 
                                           collapse = "|"))) 
central_coast_wines %<>% 
  filter(!str_detect(variety, "https")) %>% 
  rbind(.,t) %>% 
  filter(years > 1900)
 
# save(central_coast_wines, file = here::here('data/processed_cc_wines.rdata'))
write_csv(central_coast_wines,  here::here('data/cc_wine_lacking_data.csv'))
# write_csv(t, here::here('data/broken_links.csv'))
# 
# t <- data.frame(group = c("rhis is chardonnay","this si a petite sirah",
#                           "this is a petite verdot","this is syrah-grenache",
#                           "this is just syrah", "this is grenache-syrah",
#                           " syrah grenache-syrah"))
# 
# 
# unlist(t$variety)
# 

