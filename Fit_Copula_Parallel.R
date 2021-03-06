#Copula Fit

# Load Data ---------------------------------------------------------------
library(tidyverse) ; library(GeneralizedHyperbolic) ; library(parallel)

# {load("BAC_NiG_forecasts.RData")
# BAC_fit = fits}
# {load("WFC_NiG_forecasts.RData")
# WFC_fit = fits}
# rm(fits)

load("~/P8/WFC_NiG_forecasts_month.Rdata")
load("~/P8/BAC_NiG_forecasts_month.Rdata")


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

window = 21*391
refit = 391

Interval = Interval_Matrix(BAC.returns, window,refit)


# Copula Fit --------------------------------------------------------------
library(copula)



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

cl = makePSOCKcluster(20)
Cop_est <- Copula_Parallel(BAC.returns,WFC.returns,BAC_NiG_forecasts_month,WFC_NiG_forecasts_month,cl,window = window,refit = refit)
stopCluster(cl)

save(Cop_est,file = "Estimated_Copula_month.RData")
load("Estimated_Copula_month.RData")

cl = makePSOCKcluster(20)
Cop_est <- Copula_Parallel(BAC.returns,AIG.returns,BAC_NiG_forecasts_month,AIG_NiG_forecasts_month,cl,window = window,refit = refit)
stopCluster(cl)

save(Cop_est,file = "Estimated_Copula_AIG-BAC.RData")

load("WFC_NiG_5min.Rdata")
load("BAC_NiG_5min.Rdata")
load("AIG_NiG_5min.Rdata")
load("Clean_data_5min.Rdata")

library(tictoc)
window <- 21*78 ; refit <- 78
Interval = Interval_Matrix(BAC.returns.5min, window,refit)
cl = makePSOCKcluster(20)
{tic();Cop_est <- Copula_Parallel(BAC.returns.5min,WFC.returns.5min,BAC_NiG_5min,WFC_NiG_5min,
                                  cl,window = window,refit = refit);toc()}
save(Cop_est,file = "Estimated_Copula_5min.RData")
stopCluster(cl)


cl = makePSOCKcluster(20)
{tic();Cop_est_AIG_BAC <- Copula_Parallel(BAC.returns.5min,AIG.returns.5min,BAC_NiG_5min,AIG_NiG_5min,
                                  cl,window = window,refit = refit);toc()}
save(Cop_est_AIG_BAC,file = "Estimated_Copula_5min_AIG.RData")
stopCluster(cl)






# Fit Failed --------------------------------------------------------------
# 
# load("Estimated_Copula_month.RData")

failed = rep(0, length(Cop_est))
for(i in 1:length(Cop_est)){if("try-error" %in% class(Cop_est[[i]])){failed[i] = 1}}
sum(failed)
plot(failed)

index = 1:length(Cop_est)
failed_index = which(failed == 1)

Interval = Interval_Matrix(BAC.returns,window,refit)


cl <- makePSOCKcluster(20)
failed_copulas <- parSapply(cl = cl,failed_index,Fit_Interval,r_1 = BAC.returns, r_2 = WFC.returns, 
                            f_1 = BAC_NiG_forecasts_month, f_2 = WFC_NiG_forecasts_month,
                            cop = tCopula, Int = Interval)
stopCluster(cl)
save(failed_copulas,file = "failed_copulas_month.rdata")

















