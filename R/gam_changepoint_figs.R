library(mgcv)
library(tidyverse)
library(magrittr)
library(patchwork)

load(here::here("data/gam_changepoints_data.rdata"))

# # lag1_enso_fig <- out2 %>% 
# #   mutate(incr = unlist(mod.dsig$incr),
# #          decr = unlist(mod.dsig$decr),
# #          lag1_enso_deriv_lower = confint.Deriv(mod.d, term = "lag1_enso")$lag1_enso$lower,
# #          lag1_enso_deriv_upper = confint.Deriv(mod.d, term = "lag1_enso")$lag1_enso$upper,
# #          lag1_enso_deriv = mod.d$lag1_enso$deriv) %>% 
#   mutate(lag1_deriv_incr = ifelse(!is.na(incr),lag1_enso_deriv, NA),
# #          lag1_deriv_decr = ifelse(!is.na(decr),lag1_enso_deriv, NA))



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

lab_df1 <- data.frame(x = -1.25,
                     y = 7.5)
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
  xlab("lag-1 Oceanic Niño Index (°C)") +
  geom_text(data = lab_df1, 
            aes(x = x, y = y,label = "A"), vjust = "inward", hjust = "inward")

  lab_df2 <- data.frame(x = -1.25,
                       y = 75)
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
  xlab("lag-1 Oceanic Niño Index (°C)") +
  geom_text(data = lab_df2, 
            aes(x = x, y = y,label = "B"), vjust = "inward", hjust = "inward")
lag1_enso_gam_fit + lag1_enso_deriv + plot_layout(nrow = 1) + 
  plot_annotation(title = "Neutral lag-1 ENSO conditions favor North Coast Cabernet Sauvignon wine ratings")
