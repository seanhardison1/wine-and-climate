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
  left_join(.,enso)

#most parsimonious model identified via selection process

mod <- gam(get(paste("cabernet sauvignon")) ~ 
            s(tmax_avg_gs, bs = "ts") + 
            s(enso, bs = "ts") + 
            s(year, bs = "ts"), select = T,
          data = clim_ratings)

summary(mod)
anova(mod)

#predict from model
newdata <- data.frame(enso = seq(min(clim_ratings$enso, na.rm = T),
                                 max(clim_ratings$enso, na.rm = T),
                                 length.out = 200),
                      year = seq(min(clim_ratings$year, na.rm = T),
                                 max(clim_ratings$year, na.rm = T),
                                 length.out = 200),
                      tmax_avg_gs = seq(min(clim_ratings$tmax_avg_gs, na.rm = T),
                                        max(clim_ratings$tmax_avg_gs, na.rm = T),
                                        length.out = 200))
pred <- predict(mod, newdata = newdata, type = "terms", se.fit = TRUE)

out <- newdata %>% 
  mutate(tmax_avg_gs_pred = pred$fit[,1],
         enso_pred = pred$fit[,2],
         tmax_se = pred$se.fit[,1],
         enso_se = pred$se.fit[,2])

df.res <- df.residual(mod)
crit.t <- qt(0.025, df.res, lower.tail = FALSE)
out2 <- out %>% mutate(upper_temp = tmax_avg_gs_pred + (crit.t * tmax_se),
                       lower_temp = tmax_avg_gs_pred - (crit.t * tmax_se),
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

Term <- "tmax_avg_gs"
mod.d <- Deriv(mod)
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(out2$tmax_avg_gs_pred, d = mod.d[[Term]]$deriv,
                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)
tmax_fig <- out2 %>% 
  mutate(incr = unlist(mod.dsig$incr),
         decr = unlist(mod.dsig$decr),
         tmax_avg_gs_deriv_lower = confint.Deriv(mod.d, term = "tmax_avg_gs")$tmax_avg_gs$lower,
         tmax_avg_gs_deriv_upper = confint.Deriv(mod.d, term = "tmax_avg_gs")$tmax_avg_gs$upper,
         tmax_avg_gs_deriv = mod.d$tmax_avg_gs$deriv)

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
  xlab("ENSO index")

enso_deriv <- 
  ggplot(data = enso_fig) +
  geom_line(aes(x = enso, y = enso_deriv)) +
  geom_ribbon(aes(x = enso, ymax = enso_deriv_upper, ymin = enso_deriv_lower), alpha = 0.1) +
  theme_minimal() +
  ylab(expression(italic("f'(x)"))) +
  geom_hline(yintercept = 0)

tmax_gam_fit <- 
ggplot(data = tmax_fig) +
  geom_line(aes(x = tmax_avg_gs, y = tmax_avg_gs_pred)) +
  geom_line(aes(x = tmax_avg_gs, y = incr), color = "blue", size =  2) +
  geom_ribbon(aes(x = tmax_avg_gs, ymax = upper_temp, ymin = lower_temp), alpha = 0.1) +
  theme_minimal() +
  geom_rug(data = clim_ratings, aes(x = tmax_avg_gs)) +
  geom_point(data = clim_ratings, aes(x = tmax_avg_gs, 
                                      y = `cabernet sauvignon` - 
                                        mean(`cabernet sauvignon`, na.rm = T))) +
  ylab("Ratings (centered)") +
  xlab("Average Maximum Growing Season Temperature (°C)")

tmax_deriv <- 
  ggplot(data = tmax_fig) +
  geom_line(aes(x = tmax_avg_gs, y = tmax_avg_gs_deriv)) +
  geom_ribbon(aes(x = tmax_avg_gs, ymax = tmax_avg_gs_deriv_upper, ymin = tmax_avg_gs_deriv_lower),
              alpha = 0.1) +
  theme_minimal() +
  ylab(expression(italic("f'(x)"))) +
  geom_hline(yintercept = 0) +
  xlab("Average Maximum Growing Season Temperature (°C)")



