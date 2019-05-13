
# Loading packages --------------------------------------------------------

library(tidyverse) 
library(GeneralizedHyperbolic) 
library(parallel)
library(copula)

#load("./Rdata/Clean_data.RData")
load("./Rdata/Clean_data_5min.rdata")


# Fitting the marginals ---------------------------------------------------

rolling.nig <- function(data,window,refit,cl){
  
  browser()
  
  #Separating timestamps and returns
  Time <- data$Time ; data <- data$Return
  
  #Number of estimations required
  number <- ceiling((length(data)-window)/refit)
  
  #Matrix of estimation window intervals.
  intervals <- matrix(0,nrow = number,ncol = 4)
  intervals[1,] <- c(1,window-1)
  for(i in 2:(number)){
    intervals[i,1:2] <- intervals[i-1,1:2] + refit
  }
  
  
  # Function for estimation to be applied on all windows.
  fitting <- function(x){
    library(GeneralizedHyperbolic)
    start <- try(nigFitStart(data[x[1]:x[2]],startValues = "MoM",startMethodMoM = "Nelder-Mead"))
    if("try-error" %in% class(start)){return("Error in MoM")}
    fit <- try(nigFit(data[x[1]:x[2]], paramStart = start,method = "Nelder-Mead"))
    if("try-error" %in% class(fit)){par <- (as.numeric(start$paramStart))}else{par <- (as.numeric(fit$param))}
    return(par)
  }
  
  # Parallel application of estimation function to windows.
  parameters <- parApply(cl,intervals,MARGIN = 1,FUN = fitting)
  
  # Check and fix weirdness.
  if(class(parameters) == "list"){
    parameters <- do.call(rbind,parameters)
    parameters <- parameters[,1:4]
  }
  
  parameters <- as.data.frame(parameters)
  
  if(dim(parameters)[2]>4 & dim(parameters)[1]<=4){
    parameters <- t(parameters)
  }else if(dim(parameters)[2]>4 & dim(parameters)[1]>4){
    return("Wrong parameter dimensions")
  }
  
  # Add timestamps and combine objects
  times <- Time[intervals[,2]+1]
  result <- data.frame(times,parameters);colnames(result) <- c("Time","mu","delta","alpha","beta")
  
  
  return(result)
}

#Setting window and refit
window <- 21*78 
refit <- 78

#Applying rolling forecasts:

cl <- makePSOCKcluster(2)
WFC_NiG_forecasts_month <- rolling.nig(WFC.returns.5min,window,refit,cl)
stopCluster(cl)


# Estimating Copulas ------------------------------------------------------

Copula_Parallel = function(return_1, return_2, fit_1, fit_2, cl, window, refit, copula = tCopula){
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
    
    U = try(pnig(X, param = p_1))
    
    if("try-error" %in% class(U)){
      f = ecdf(X)
      U = rep(0,length(X))
      for(i in 1:length(X)){
        temp = try(pnig(X[i], param = p_1)) 
        
        if("try-error" %in% class(temp)){
          U[i] = f(X[i])
        }else{
          U[i] = temp
        }
      }
    }
    
    V = as.numeric(try(pnig(Y, param = p_2)))
    
    if("try-error" %in% class(V)){
      f = ecdf(Y)
      V = rep(0,length(Y))
      for(i in 1:length(Y)){
        temp = try(pnig(Y[i], param = p_2)) 
        
        if("try-error" %in% class(temp)){
          V[i] = f(Y[i])
        }else{
          V[i] = temp
        }
      }
    }
    W = cbind(U,V)
    
    C = cop(dim = 2)
    fit = suppressWarnings(fitCopula(C,W))
    
    theta = fit@copula@parameters
    
    return(c(start,end,theta))
  }
  
  Result = parSapply(cl,1:nrow(Interval),function(x){try(Fit_Interval(x))})
  
  return(Result)
}



