#ts feature

library(tsfeatures)
library(anomalous)
library(tidyverse)
# - 

yahoo <- cbind(dat0, dat1, dat2, dat3)
hwl <- bind_cols(
  tsfeatures(yahoo,
             c("acf_features", "entropy", "lumpiness",
               "flat_spots", "crossing_points")),
  tsfeatures(yahoo, "stl_features",
             s.window = "periodic", robust = T),
  tsfeatures(yahoo, "max_kl_shift", width = 48),
  tsfeatures(yahoo,
             c("mean", "var"), scale = F, na.rm = T),
  tsfeatures(yahoo,
             c("max_level_shift", "max_var_shifyt"), trim = T)) %>%
  select(mean, var, x_acf1, trend,
         seasonal_strength, peak, trough,
         entropy, lumpiness, spike, max_level_shift,
         max_var_shift, flat_spots, crossing_points,
         max_kl_shift, time_kl_shift)

pr <- prcomp(na.omit(hwl), scale = T)$x %>% as.tibble()
