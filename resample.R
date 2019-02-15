library(dplyr)
library(xts)
library(zoo)
library(lubridate)
# ---

set.seed(123)

v <- sample(730)
t <- seq.Date(from = as.Date("2019-01-01"), by="day", length.out = 730)

ts <- xts::xts(v, order.by=t)
#class(ts) <- c("xts", "zoo", "daily") # add class 
# ---


# the class should be added by a function before the UseMethod()?

# generic method
resample <- function (x, ...) {
  date <- index(x) #xts
  split_date <- stringr::str_split("2019-01-01", "-| |:", simplify=T)
  
  if(length(split_date)==3) {
    class(x) <- c(class(x), "daily")
  }
  #... if() then...
  
  UseMethod("resample", x)
}
# ---


# 
resample.daily <- function(x, to, fun = "mean") {
  
  date <- index(x) #xts
  FUN <- match.fun(fun) 
  
  # to defines the resampling method
  if (to == "M") {
    r_index <- month(as.POSIXlt(date, format="%Y-%m-%d"), abbr=T, label=T) #lubridate
    r_index2 <- unique(year(as.POSIXlt(date, format="%Y-%m-%d")))
    
    # resampling
    
    # tt <- as.vector(sapply(paste0(r_index2, "-%s"),
    #                       function(x) sprintf(x, unique(r_index)), USE.NAMES = F))
    # lvl <- as.vector(sapply(paste0(r_index2, "-%s"),
    #                       function(x) sprintf(x, levels(r_index)), USE.NAMES = F))
    
    #
    
    # res <- tibble(time = factor(tt, #!!!
    #                             levels=lvl), #!!!
    #               values = as.vector(coredata(x))) %>% group_by(time) %>% summarise(values = FUN(values))
    
  }
  
  if (to == "Y") {
    r_index <- year(as.POSIXlt(date, format="%Y-%m-%d"))
    
    # resampling
    res <- tibble(time = r_index, 
                  values = as.vector(coredata(x))) %>% group_by(time) %>% summarise(values = FUN(values))
  }
  

  #dplyr-xts
  
  # returns a tibble
  return(res)
  
}

resample.monthly <- function(x, to, fun = "mean") {
  date <- index(x) #xts
  FUN <- match.fun(fun) 
  
  if (to == "Y") {
    r_index <- year(as.POSIXlt(date, format="%Y-%m-%d"))
  }
  
  # resampling
  res <- tibble(time = r_index, 
                values = as.vector(coredata(x))) %>% group_by(time) %>% summarise(values = FUN(values))
  #dplyr-xts
  
  # returns a tibble
  return(res)
}

# example call ---
resample(ts, to="M", fun="mean")
resample(ts, to="Y", fun="mean")
