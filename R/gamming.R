library(gam)
library(mgcv)
library(tidyverse)
library(magrittr)
library(itsadug)

ratings <- readxl::read_excel(here::here("data/north_coast_wine_ratings.xlsx"))
ratings %<>% tidyr::gather(Var, Value, -year) %>% 
  mutate(Value = as.numeric(Value))

load(here::here('data/north_coast_climate.rdata'))

clim_stations <- north_coast_weather %>% 
  group_by(station, `year(valid)`) %>% 
  dplyr::summarise_at(vars(tmpf:mslp), mean, na.rm = T) %>% 
  dplyr::rename(year = `year(valid)`) %>% 
  tidyr::gather(.,Var, Value, -year, -station)


ggplot() +
  geom_line(data = clim_stations, aes(x = year, y = Value,
                             color = station))+
  facet_wrap(.~Var, scales = "free_y")

clim <- north_coast_weather %>% 
  group_by(`year(valid)`) %>% 
  dplyr::summarise_at(vars(tmpf:mslp), mean, na.rm = T) %>% 
  dplyr::rename(year = `year(valid)`) %>% 
  tidyr::gather(.,Var, Value, -year)

clim_ratings <- clim %>% 
  left_join(.,ratings, by = "year") %>% 
  dplyr::select(year, 
                clim_var = Var.x, 
                varietal = Var.y,
                rating = Value.y,
                clim_val = Value.x)

ggplot(data = clim_ratings) +
  geom_point(aes(x = clim_val, y = rating, color = varietal)) +
  stat_smooth(method = "lm", aes(x = clim_val, y = rating,
                                 group = varietal)) +
  facet_wrap(.~clim_var, scales = "free")

(t <- clim_ratings %>% filter(clim_var == "tmpf") %>% 
  split(.,.$varietal) %>% 
  map(~ lm(rating ~ clim_val, data = .x)) %>% 
  map(summary) 
)


gam_clim <- clim_ratings %>% filter(clim_var == "tmpf") %>% 
  mutate(varietal = factor(varietal)) 

temp_mod <- 
  gam(rating ~ s(clim_val) +
        s(clim_val, varietal, m = 2, bs = "fs") ,
    data = gam_clim, method = "REML")
plot(temp_mod)

par(mfrow = c(1,2),
    mar = c(4,4,4,1))
plot_smooth(temp_mod, view = "clim_val", plot_all = "varietal",
            col = "#b591c94D", 
            legend_plot_all = NA, 
            hide.label = T, main = "Summed and Partial Effects")
plot_smooth(temp_mod, view = "clim_val", plot_all = "varietal",
            col = "#f5d1424D",
            rm.ranef = T, legend_plot_all = NA, 
            hide.label = T,add = T)
plot_smooth(temp_mod, view = "clim_val", plot_all = "varietal",
            hide.label = T, main = "Partial Effects")
