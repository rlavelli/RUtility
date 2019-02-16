library(dplyr)
library(xts)
#---

# example
set.seed(123)
v <- sample(750)
t <- seq.POSIXt(from=as.POSIXct("2019-01-01"), 
                length.out = 750, by="day")

ts <- xts(v, order.by=t)

# generic s3 method
resample <- function (x, ...) {
  date <- index(x) #xts
  split_date <- stringr::str_split(date[1], "-| |:", simplify=T)
  
  if(length(split_date)==3) {
    class(x) <- c(class(x), "daily")
  }
  #... if() then...
  
  UseMethod("resample", x)
}


# daily method
# 
resample.daily <- function(x, to, fun = "mean") {
  
  date <- index(x) #xts
  FUN <- match.fun(fun) 
  
  # to defines the resampling method
  if (to == "M") {
    r_index <- strftime(date, format="%Y-%m")
  }
  
  if (to == "Y") {
    r_index <- strftime(date, format="%Y")
  }
  
  # resampling
  res <- tibble(time = r_index, 
                values = as.vector(coredata(x))) %>%
                                  group_by(time) %>% summarise(values = FUN(values))
  
  # returns a tibble
  return(res)
}


# example call
resample(ts, to="Y", fun="mean")

