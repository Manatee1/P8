
# Fit Normal Inverse Gaussian ---------------------------------------------
library(GeneralizedHyperbolic); library(tidyverse); library(magrittr); library(lubridate)

#setwd("C:/Users/toke/Dropbox/P6/P8/R filer/P8/Rdata")
{load("Clean_data.Rdata"); WFC = WFC.data; rm(WFC.data); rm(BAC.returns,AIG.returns,WFC.returns)}

#Add Lubridate Time Format for Easy Subsetting 
AIG %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price)
BAC %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price)
WFC %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price)

#Visualize Data
AIG %>% ggplot(aes(x = Time, y = Price)) + geom_line()
BAC %>% ggplot(aes(x = Time, y = Price)) + geom_line()
WFC %>% ggplot(aes(x = Time, y = Price)) + geom_line()

#Subset Every 5th minute 
AIG_5minute = AIG %>% filter(minute(Time) %% 5 == 0)
BAC_5minute = BAC %>% filter(minute(Time) %% 5 == 0)
WFC_5minute = WFC %>% filter(minute(Time) %% 5 == 0)

#Add Returns
AIG_5minute %<>% mutate(Returns = c(0,diff(log(Price))))
BAC_5minute %<>% mutate(Returns = c(0,diff(log(Price))))
WFC_5minute %<>% mutate(Returns = c(0,diff(log(Price))))

#Fix Stock-Split
stock_split = which(WFC_5minute[["Returns"]] == min(WFC_5minute[["Returns"]]))
WFC_5minute[["Returns"]][stock_split] = log((2*WFC_5minute[["Price"]][stock_split])/WFC_5minute[["Price"]][stock_split-1])
rm(stock_split)


# Create Observation Scheme -----------------------------------------------

#Observations per Day
O = which(WFC_5minute[["Time"]] == WFC_5minute[["Time"]][1] + days(1)) - 1

#Window and Refit
Window = 21*O
Refit = O
rm(O)

#Interval Matrix and Interval 
Interval_Matrix = function(data,window,refit){
  Time <- data$Time ; data <- data$Return
  #browser()
  number <- ceiling((length(data)-window)/refit)
  intervals <- matrix(0,nrow = number,ncol = 2)
  intervals[1,] <- c(1,window-1)
  for(i in 2:(number)){
    intervals[i,] <- intervals[i-1,] + refit
  }
  return(intervals)
}

Interval = function(i, Window, Refit, Data){
  I = 1:Window + (i-1)*Refit
  return(Data[I,])
}


nigFit(Interval(,Window,Refit,AIG_5minute) %>% pull(Returns))


