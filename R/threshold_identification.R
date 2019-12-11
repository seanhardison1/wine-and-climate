library(gam)
library(mgcv)
library(tidyverse)
library(magrittr)
library(itsadug)
library(zoo)
library(forecast)
library(patchwork)

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
  left_join(.,enso) 
clim_ratings$lag1_enso <- lag(clim_ratings$enso)
#most parsimonious model identified via selection process

# mod <- gam(get(paste("cabernet sauvignon")) ~ 
#             s(gdd, bs = "tp") +
#             s(enso, bs = "tp") + 
#             s(lag1_enso, bs = "tp") + 
#             s(year, bs = "tp"),
#           method = "ML",
#           data = clim_ratings)

mod <- gam(get(paste("cabernet sauvignon")) ~ 
              s(enso) + 
              s(lag1_enso) + 
              s(year),
            method = "ML",
           data = clim_ratings)

# AIC(mod, mod2)

#predict from model
newdata <- data.frame(enso = seq(min(clim_ratings$enso, na.rm = T),
                                 max(clim_ratings$enso, na.rm = T),
                                 length.out = 200),
                      year = seq(min(clim_ratings$year, na.rm = T),
                                 max(clim_ratings$year, na.rm = T),
                                 length.out = 200),
                      lag1_enso = seq(min(clim_ratings$lag1_enso, na.rm = T),
                                        max(clim_ratings$lag1_enso, na.rm = T),
                                        length.out = 200))
pred <- predict(mod, newdata = newdata, type = "terms", se.fit = TRUE)

out <- newdata %>% 
  mutate(enso_pred = pred$fit[,1],
         lag1_enso_pred = pred$fit[,2],
         tmax_se = pred$se.fit[,1],
         enso_se = pred$se.fit[,2])

df.res <- df.residual(mod)
crit.t <- qt(0.025, df.res, lower.tail = FALSE)
out2 <- out %>% mutate(upper_temp = lag1_enso_pred + (crit.t * tmax_se),
                       lower_temp = lag1_enso_pred - (crit.t * tmax_se),
                       upper_enso = enso_pred + (crit.t * enso_se),
                       lower_enso = enso_pred - (crit.t * enso_se))
#download derivatives gist (thanks Gavin Simpson!)
tmpf <- tempfile()
download.file("https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30/raw/82118ee30c9ef1254795d2ec6d356a664cc138ab/Deriv.R",
              tmpf, method = "wget")
source(tmpf)

#finding confidence intervals for model derivatives

Term <- "enso"
mod.d <- Deriv(mod)
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(out2$enso_pred, d = mod.d[[Term]]$deriv,
                      mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)
enso_fig <- out2 %>% 
    mutate(incr = unlist(mod.dsig$incr),
           decr = unlist(mod.dsig$decr),
           enso_deriv_lower = confint.Deriv(mod.d, term = "enso")$enso$lower,
           enso_deriv_upper = confint.Deriv(mod.d, term = "enso")$enso$upper,
           enso_deriv = mod.d$enso$deriv)

Term <- "lag1_enso"
mod.d <- Deriv(mod)
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(out2$lag1_enso_pred, d = mod.d[[Term]]$deriv,
                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)
lag1_enso_fig <- out2 %>% 
  mutate(incr = unlist(mod.dsig$incr),
         decr = unlist(mod.dsig$decr),
         lag1_enso_deriv_lower = confint.Deriv(mod.d, term = "lag1_enso")$lag1_enso$lower,
         lag1_enso_deriv_upper = confint.Deriv(mod.d, term = "lag1_enso")$lag1_enso$upper,
         lag1_enso_deriv = mod.d$lag1_enso$deriv) %>% 
  mutate(lag1_deriv_incr = ifelse(!is.na(incr),lag1_enso_deriv, NA),
         lag1_deriv_decr = ifelse(!is.na(decr),lag1_enso_deriv, NA))

save(lag1_enso_fig, enso_fig, clim_ratings, file = here::here("data/gam_changepoints_data.rdata"))


enso_gam_fit <- 
  ggplot(data = enso_fig) +
  geom_line(aes(x = enso, y = enso_pred)) +
  geom_line(aes(x = enso, y = incr), color = "blue", size =  2) +
  geom_ribbon(aes(x = enso, ymax = upper_enso, ymin = lower_enso), alpha = 0.1) +
  theme_minimal() +
  geom_rug(data = clim_ratings, aes(x = enso)) +
  geom_point(data = clim_ratings, aes(x = enso, 
                                      y = `cabernet sauvignon` - 
                                        mean(`cabernet sauvignon`, na.rm = T))) +
  ylab("Ratings (centered)") +
  xlab("Oceanic Niño Index (°C)")

enso_deriv <- 
  ggplot(data = enso_fig) +
  geom_line(aes(x = enso, y = enso_deriv)) +
  geom_ribbon(aes(x = enso, ymax = enso_deriv_upper, ymin = enso_deriv_lower), alpha = 0.1) +
  theme_minimal() +
  ylab(expression(italic("f'(x)"))) +
  geom_hline(yintercept = 0)

lag1_enso_gam_fit <- 
ggplot(data = lag1_enso_fig) +
  geom_line(aes(x = lag1_enso, y = lag1_enso_pred)) +
  geom_line(aes(x = lag1_enso, y = incr), color = "blue", size =  2) +
  geom_line(aes(x = lag1_enso, y = decr), color = "red", size =  2) +
  geom_ribbon(aes(x = lag1_enso, ymax = upper_temp, ymin = lower_temp), alpha = 0.1) +
  theme_minimal() +
  geom_rug(data = clim_ratings, aes(x = lag1_enso)) +
  geom_point(data = clim_ratings, aes(x = lag1_enso, 
                                      y = `cabernet sauvignon` - 
                                        mean(`cabernet sauvignon`, na.rm = T))) +
  ylab("Ratings (centered)") +
  xlab("lag-1 Oceanic Niño Index (°C)")


lag1_enso_deriv <- 
  ggplot(data = lag1_enso_fig) +
  geom_line(aes(x = lag1_enso, y = lag1_enso_deriv)) +
  geom_line(aes(x = lag1_enso, y = lag1_deriv_incr), color = "blue", size =  2) +
  geom_line(aes(x = lag1_enso, y = lag1_deriv_decr), color = "red", size =  2) +
  geom_ribbon(aes(x = lag1_enso, ymax = lag1_enso_deriv_upper, ymin = lag1_enso_deriv_lower),
              alpha = 0.1) +
  theme_minimal() +
  ylab(expression(italic("f'(x)"))) +
  geom_hline(yintercept = 0) +
  xlab("lag-1 Oceanic Niño Index (°C)")

lag1_enso_changepoints <- lag1_enso_gam_fit + lag1_enso_deriv + plot_layout(nrow = 1)


