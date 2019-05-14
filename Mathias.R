
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

# cl <- makePSOCKcluster(2)
# WFC_NiG_forecasts_month <- rolling.nig(WFC.returns.5min,window,refit,cl)
# stopCluster(cl)

load("./Rdata/NIG_estimates/BAC_NiG_5min.Rdata")
load("./Rdata/NIG_estimates/WFC_NiG_5min.Rdata")

# Estimating Copulas ------------------------------------------------------

Copula_Parallel = function(return_1, return_2, fit_1, fit_2, cl, window, refit, copula = tCopula){
browser()
  return_1 <- return_1; return_2 <- return_2
  fit_1 <- fit_1; fit_2 <- fit_2

  Interval_Matrix = function(data,window,refit){
    Time <- data$Time ; data <- data$Return
    number <- ceiling((length(data)-window)/refit)
    intervals <- matrix(0,nrow = number,ncol = 2)
    intervals[1,] <- c(1,window-1)
    for(i in 2:(number)){
      intervals[i,] <- intervals[i-1,] + refit
    }
    return(intervals)
  }
  
  Interval = Interval_Matrix(return_1, window, refit)
  
  Fit_Interval = function(i, r_1 = return_1, r_2 = return_2, f_1 = fit_1, 
                          f_2 = fit_2, cop = copula, Int = Interval){
    
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

# Cop_est <- Copula_Parallel(BAC.returns.5min,WFC.returns.5min,BAC_NiG_5min,WFC_NiG_5min,
#                            cl,window = window,refit = refit)



#Testing interval function:

Fit_Interval = function(i, r_1 = return_1, r_2 = return_2, f_1 = fit_1, 
                        f_2 = fit_2, cop = copula, Int = Interval){
  
  browser()
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

# Test <- Fit_Interval(1,r_1 = BAC.returns.5min, 
#                      r_2 = WFC.returns.5min,
#                      f_1 = BAC_NiG_5min, 
#                      f_2 = WFC_NiG_5min, 
#                      cop = tCopula, 
#                      Int = Interval_Matrix(BAC.returns.5min, window, refit))



# Making draws for CVaR ---------------------------------------------------

load("./Rdata/Copula_Estimates/Estimated_Copula_5min.RData")


#Simulation of Copula

N_0 = 1e4

Copula_NIG = function(N,copula, NIG_param_1, NIG_param_2, debug = F){
  eps_1 = eps_2 = 1e-16
  if(debug) browser()
  
  Distribution = mvdc(copula = copula, margins = c("unif","unif"), 
                      paramMargins = list(list(min = 0, max = 1),list(min = 0, max = 1)))
  
  Uniform = rMvdc(N,Distribution)
  
  
  NIG_1 = try(qnig(Uniform[,1],param = NIG_param_1),silent = T)
  NIG_2 = try(qnig(Uniform[,2],param = NIG_param_2),silent = T)
  while("try-error" %in% class(NIG_1)){
    NIG_1 = try(qnig(Uniform[,1],param = NIG_param_1,intTol = eps_1),silent = T)
    
    eps_1 = eps_1 * 10
    if(eps_1 > 1e-6){
      stop("Tolerance is greater than 1e-6")
    }
  }
  while("try-error" %in% class(NIG_2)){
    NIG_2 = try(qnig(Uniform[,2],param = NIG_param_2,intTol = eps_2),silent = T)
    
    eps_2 = eps_2 * 10
    
    if(eps_2 > 1e-6){
      stop("Tolerance is greater than 1e-6")
    }
  }
  
  
  NIG = cbind(NIG_1,NIG_2)
  
  return(NIG)
}

draws = list()

# pb = txtProgressBar(min = 0, max = length(Cop_est), style = 3)
# for(i in 1:length(Cop_est)){
#   if("try-error" %in% class(Cop_est[[i]])){
#     draws[[i]] = draws[[i-1]]
#   }else{
#     Parameters = as.numeric(Cop_est[[i]][3:4])
#     Copula = tCopula(param = Parameters[1], dim = 2,df = Parameters[2])
#     NIG_1 = BAC_NiG_5min[i,2:5]
#     NIG_2 = WFC_NiG_5min[i,2:5]
#     NIG = Copula_NIG(N_0, Copula, NIG_1, NIG_2 , debug = T)
#     draws[[i]] = NIG
#   }
#   setTxtProgressBar(pb,i)
# }

load("../tCopula_draws.Rdata")


# Finding weights that minimize CVaR --------------------------------------

VaR = function(x,gamma = 0.05){
  L = -x
  q = quantile(L,1-gamma)
  return(q)
}

CVaR = function(x, alpha = 0.05){
  x_alpha = quantile(x,alpha)
  CVaR = -(1/alpha)*mean(x*(x<= x_alpha))
  return(CVaR)
}



#Finding set of weights.
W = matrix(0,nrow = length(tCopula_draws),ncol = 2)
w_1 = seq(from = -10, to = 10, length.out = 500)
w_2 = 1 - w_1
w = cbind(w_1, w_2)

pb = txtProgressBar(min = 0, max = length(tCopula_draws), style = 3)
for(i in 1:length(tCopula_draws)){
  draw = tCopula_draws[[i]]
  CVaR_draw = apply(w,1,function(x){CVaR(draw %*% x)})
  W[i,] = w[which(CVaR_draw == min(CVaR_draw)),][1]
  setTxtProgressBar(pb,i)
}

plot(W[,1] , type = "l")
plot(W[,2] , type = "l")
