library(gam)
library(mgcv)
library(tidyverse)
library(magrittr)
library(itsadug)
library(zoo)
library(forecast)

 #load data sets
ratings <- readxl::read_excel(here::here("data/north_coast_wine_ratings.xlsx")) %>% 
  mutate_all(.,as.numeric)
load(here::here("data/north_coast_climate_indices.rdata"))
# https://climatedataguide.ucar.edu/climate-data/nino-sst-indices-nino-12-3-34-4-oni-and-tni
enso <- readxl::read_excel(here::here('data/oni.xlsx'))  %>%
  tidyr::gather(Month, Value, -year) %>% 
  group_by(year) %>% 
  dplyr::summarise(enso = mean(Value)) %>% 
  filter(year != 2019)

clim_ratings <- clim_indices %>% 
  left_join(.,ratings, by = "year") %>% 
  left_join(.,enso) %>% 
  mutate(lag1_enso = lag(clim_ratings$enso))

#colinearity check
GGally::ggpairs(clim_ratings %>% dplyr::select(-year,-chardonnay:-`pinot noir`))

#First we assess the time series for ARIMA structure
clim_vars <- names(clim_ratings)[c(2:6)]
wine_vars <- names(clim_ratings)[11:14]
mod_out <- NULL
for (i in clim_vars){
  for (j in wine_vars){
    
    mod <- gamm(get(paste(j)) ~ s(enso) + s(year),
           data = clim_ratings,
           method = "REML")
    
   out <- broom::tidy(mod$gam) %>% 
      mutate(wine = j,
             clim = i,
             edf = round(edf, 5))
   
   assign('mod_out', rbind(mod_out, out))
   
  }
}



clim_vars <- names(clim_ratings)[c(2:6)]
mod_out <- NULL
for (i in clim_vars){

    mod <- gam(get(paste("cabernet sauvignon")) ~ s(get(paste(i))) + s(enso) + s(year),
                data = clim_ratings,
                method = "REML")
    
    out <- broom::tidy(mod$gam) %>% 
      mutate(wine = "cabernet sauvignon",
             clim = i)
    assign('mod_out', rbind(mod_out, out))
  }

mod_out %>% filter(str_detect(term, "enso")) %>% arrange(desc(p.value))
    
clim_vars <- names(clim_ratings)[c(2:6)]
mod_out <- NULL
for (i in clim_vars){
  
  mod <- gam(get(paste("cabernet sauvignon")) ~ 
                s(get(paste(i))) +
                s(enso) + 
                s(year),
              data = clim_ratings,
              method = "REML")
  
  out <- broom::tidy(mod) %>% 
    mutate(wine = "cabernet sauvignon",
           clim = i)

  assign('mod_out', rbind(mod_out, out))
}

mod_out %>% filter(edf > 1.0001) %>% View

g1 <- gam(get(paste("cabernet sauvignon")) ~ 
            s(tmin_avg_gs, bs = "tp") +
            s(lag1_enso, bs = "tp") + 
            s(enso, bs = "tp")+
            s(year, bs = "tp"),
          method = "ML",
          data = clim_ratings)

g2 <- gam(get(paste("cabernet sauvignon")) ~ 
            s(tmax_avg_gs, bs = "tp") +
            s(lag1_enso, bs = "tp") + 
            s(enso, bs = "tp")+ 
            s(year, bs = "tp"),
          method = "ML",
          data = clim_ratings)

g3 <- gam(get(paste("cabernet sauvignon")) ~ 
            s(gdd, bs = "tp") +
            s(enso, bs = "tp") + 
            s(lag1_enso, bs = "tp") + 
            s(year, bs = "tp"),
          method = "ML",
          data = clim_ratings)

AIC(g1, g2, g3)
gratia::draw(g1)
gratia::draw(g2)
gratia::draw(g3)
