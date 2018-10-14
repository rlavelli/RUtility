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
             c("mean", "var"))
)