
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



# Fit NIGs ----------------------------------------------------------------

n = nrow(AIG_5minute) %/% Refit

AIG_fits = lapply(1:n, function(x){try(nigFit(Interval(x,Window,Refit,AIG_5minute) %>% pull(Returns)))})
BAC_fits = lapply(1:n, function(x){try(nigFit(Interval(x,Window,Refit,BAC_5minute) %>% pull(Returns)))})
WFC_fits = lapply(1:n, function(x){try(nigFit(Interval(x,Window,Refit,WFC_5minute) %>% pull(Returns)))})


failed = function(list){
  failed = rep(0,length(list))
  j = 0
  for(i in list){
    j = j + 1
    if("try-error" %in% class(i)){failed[j] = 1}
  }
  if(sum(failed) == 0){print("No Errors")}else{return(failed)}
}

save(AIG_fits, BAC_fits, WFC_fits, file = "Tokes_Fits.RData")


# Fit Copula --------------------------------------------------------------

Make_Uniform = function(i,Window,Refit,Data,Fit, tol = 1e-16){
  Local_Data = pull(Interval(i,Window,Refit,Data),Returns)
  U = try(pnig(Local_Data, param = Fit[[i]][["param"]], intTol = tol),silent =T)
  
  while("try-error" %in% class(U)){
    U = try(pnig(Local_Data, param = Fit[[i]][["param"]],intTol = tol), silent = T)
    if("try-error" %in% class(U)) tol = tol*10
    if(tol > 1e-6) stop("Tolerance greater than 1e-6")
  }
  
  name = deparse(substitute(Data)) %>% strsplit("_") %>% unlist %>% .[1]
  
  attr(U, "name") =  name
  
  return(U)
}


library(copula)

AIG_Uniform = Make_Uniform(1,Window,Refit,AIG_5minute,AIG_fits)
WFC_Uniform = Make_Uniform(1,Window,Refit,WFC_5minute,AIG_fits)
BAC_Uniform = Make_Uniform(1,Window,Refit,BAC_5minute,AIG_fits)

fit_copula = function(i,Window, Refit, Data_1, Data_2, Fit_1, Fit_2, Copula, tolerance = 1e-8){
  name_1 = unlist(strsplit(x = deparse(substitute(Data_1)),split = "_"))[1]
  name_2 = unlist(strsplit(x = deparse(substitute(Data_2)),split = "_"))[1]
  Make_Uniform = function(i,Window,Refit,Data,Fit, tol = 1e-16){
    library(dplyr);library(GeneralizedHyperbolic)
    Interval = function(i, Window, Refit, Data){
      I = 1:Window + (i-1)*Refit
      return(Data[I,])
    }
    Local_Data = pull(Interval(i,Window,Refit,Data),Returns)
    U = try(pnig(Local_Data, param = Fit[[i]][["param"]], intTol = tol),silent =T)
    
    while("try-error" %in% class(U)){
      U = try(pnig(Local_Data, param = Fit[[i]][["param"]],intTol = tol), silent = T)
      if("try-error" %in% class(U)) tol = tol*10
      if(tol > 1e-6) stop("Tolerance greater than 1e-6")
    }
    
    name = deparse(substitute(Data)) %>% strsplit("_") %>% unlist %>% .[1]
    
    attr(U, "name") =  name
    
    return(U)
  }
  library(copula)
  
  Uniform_1 = try(Make_Uniform(i,Window,Refit,Data_1,Fit_1,tolerance))
  Uniform_2 = try(Make_Uniform(i,Window,Refit,Data_2,Fit_2,tolerance))
  
  U = try(cbind(Uniform_1,Uniform_2))
  colnames(U) = c(name_1,name_2)
  
  return(suppressWarnings(try(fitCopula(Copula, U),silent = T)))
}

{
tictoc::tic()
Test_Fit = fit_copula(1,Window,Refit, WFC_5minute, BAC_5minute, WFC_fits, BAC_fits,tCopula(dim = 2))
tictoc::toc()
}

library(parallel)

cl = makePSOCKcluster(4)

Copula_fit = parLapply(cl,
                       1:n,
                       fit_copula,
                       Window = Window, 
                       Refit = Refit, 
                       Data_1 = WFC_5minute,
                       Data_2 = BAC_5minute,
                       Fit_1 = WFC_fits,
                       Fit_2 = BAC_fits,
                       Copula = tCopula(dim = 2))
stopCluster(cl)

getwd()
