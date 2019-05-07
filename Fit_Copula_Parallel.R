#Copula Fit

# Load Data ---------------------------------------------------------------
library(tidyverse) ; library(GeneralizedHyperbolic) ; library(parallel)

{load("BAC_NiG_forecasts.RData")
BAC_fit = fits}

{load("WFC_NiG_forecasts.RData")
WFC_fit = fits}

rm(fits)

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

load("Clean_data.RData")

rm(AIG,BAC,WFC.data,AIG.returns)

# Subsetting Data ---------------------------------------------------------

window = 5*391
refit = 30

Interval = Interval_Matrix(BAC.returns, window,refit)


# Copula Fit --------------------------------------------------------------
library(copula)



Copula_Parallel = function(return_1, return_2, fit_1, fit_2, cl, window = 5*391, refit = 30, copula = tCopula){
  library(parallel)
  return_1 <- return_1; return_2 <- return_2
  fit_1 <- fit_1; fit_2 <- fit_2
  #browser()
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
  
  Interval = Interval_Matrix(return_1, window, refit)
  
  Fit_Interval = function(i, r_1 = return_1, r_2 = return_2, f_1 = fit_1, f_2 = fit_2, cop = copula, Int = Interval){
    #browser()
    library(GeneralizedHyperbolic);library(copula);
    I = Int[i,]
    
    start = as.character(r_1$Time[I[1]]) 
    end = as.character(r_1$Time[I[2]])
    
    X = r_1[I[1]:I[2],2]
    Y = r_2[I[1]:I[2],2]
    
    p_1 = unlist(f_1[i,2:5])
    p_2 = unlist(f_2[i,2:5])
    
    U = as.numeric(try(pnig(X, param = p_1)))
    V = as.numeric(try(pnig(Y, param = p_2)))
    W = cbind(U,V)
    
    C = cop(dim = 2)
    fit = suppressWarnings(fitCopula(C,W))
    
    theta = fit@copula@parameters
    
    return(c(start,end,theta))
  }
  
  Result = parSapply(cl,1:nrow(Interval),function(x){try(Fit_Interval(x))})
  
  return(Result)
}

cl = makePSOCKcluster(20)
Cop_est <- Copula_Parallel(BAC.returns,WFC.returns,BAC_fit,WFC_fit,cl)
stopCluster(cl)

save(Cop_est,file = "Estimated_Copula.RData")

