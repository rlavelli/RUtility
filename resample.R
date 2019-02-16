library(dplyr)
library(xts)
#---

# example
#1
set.seed(123)
v <- sample(750)
t <- seq.POSIXt(from=as.POSIXct("2019-01-01"), 
                length.out = 750, by="day")

ts <- xts(v, order.by=t)

#2
set.seed(123)
v <- sample(10*750)
t <- seq.POSIXt(from=as.POSIXct("2019-01-01 01:00"), 
                length.out = 10*750, by="min")

ts <- xts(v, order.by=t)

# generic s3 method
resample <- function (x, ...) {
  date <- index(x) #xts
  split_date <- stringr::str_split(date[1], "-| |:", simplify=T)
  
  if(length(split_date)==3) {
    class(x) <- c(class(x), "daily")
  }
  if(length(split_date)>=5) {
    class(x) <- c(class(x), "minutes")
  }
  
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

resample.minutes <- function(x, to, fun="mean") {
  date <- index(x) #xts
  FUN <- match.fun(fun) 
  
  # to defines the resampling method
  if (to == "H") {
    r_index <- strftime(date, format="%Y-%m-%d %H")
  }
  
  # to defines the resampling method
  if (to == "D") {
    r_index <- strftime(date, format="%Y-%m-%d")
  }
  
  res <- tibble(time = r_index,
                values = as.vector(coredata(x))) %>% 
                                  group_by(time) %>% summarise(values = FUN(values))
  
  # returns a tibble
  return(res)
}

# example call
#1
resample(ts, to="Y", fun="mean")
#2
resample(ts, to="H", fun="mean")
