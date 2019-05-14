library(tidyverse) ; library(GeneralizedHyperbolic) ; library(parallel)
#load("Clean_data.RData")

load("Rdata/Clean_Data_5minutes.rdata")


rolling.nig <- function(data,window,refit,cl){
  #browser()
  Time <- pull(data,Time) ; data <- pull(data,Returns) # Separating timestamps and returns

  number <- ceiling((length(data)-window)/refit) # Number of estimations required
  
  # Matrix of estimation window intervals.
  intervals <- list()
  intervals[[1]] <- c(1,window-1)
  for(i in 2:(number)){
    intervals[[i]] <- intervals[[i-1]] + refit
  }
  
  
  # Function for estimation to be applied on all windows.
  fitting <- function(x){
    dat <- data[x[1]:x[2]]
    library(e1071)
    S <- skewness(dat) ; kurt <- kurtosis(dat)
    
    if(3*kurt > 5*S^2){
      library(GeneralizedHyperbolic)
      
      
      # Method of moments:
      start <- try(nigFitStartMoM(data[x[1]:x[2]],startMethodMoM = "Nelder-Mead"))
      if("try-error" %in% class(start)){return("Error in MoM")}
      # Maximum likelihood method:
      fit <- try(nigFit(data[x[1]:x[2]], paramStart = start,method = "Nelder-Mead"))
      if("try-error" %in% class(fit)){par <- (as.numeric(start))}else{par <- (as.numeric(fit$param))}
      return(par)
    }else{
      return("Previous")
    }
  }
  
  # Parallel application of estimation function to windows.
  parameters <- parLapply(cl,intervals,fun = fitting)
  
  for(i in 1:number){
    if("Previous" %in% parameters[[i]]){
      parameters[[i]] <- parameters[[i-1]]
    }
  }
  
  parameters <- do.call(rbind,parameters)
  parameters <- parameters[,1:4]

  
  # Add timestamps and combine objects
  Interval.mat <- do.call(rbind,intervals)
  times <- Time[Interval.mat[,2]+1]
  result <- data.frame(times,parameters);colnames(result) <- c("Time","mu","delta","alpha","beta")
  
  
  return(result)
}

# 
# library(tictoc)
# cl <- makePSOCKcluster(20)
# tic();WFC_NiG_forecasts_month <- rolling.nig(WFC.returns,21*391,391,cl);toc()
# save(WFC_NiG_forecasts_month,file = "WFC_NiG_forecasts_month.Rdata")
# tic();BAC_NiG_forecasts_month <- rolling.nig(BAC.returns,21*391,391,cl);toc()
# save(BAC_NiG_forecasts_month,file = "BAC_NiG_forecasts_month.Rdata")
# AIG_NiG_forecasts_month <- rolling.nig(AIG.returns,21*391,391,cl);toc()
# save(AIG_NiG_forecasts_month,file = "AIG_NiG_forecasts_month.Rdata")
# stopCluster(cl)



library(tictoc)
cl <- makePSOCKcluster(20)
tic();WFC_NiG_5min <- rolling.nig(WFC_5minute,Window,Refit,cl);toc()
save(WFC_NiG_5min,file = "WFC_NiG_5min.Rdata")
tic();BAC_NiG_5min <- rolling.nig(BAC_5minute,Window,Refit,cl);toc()
save(BAC_NiG_5min,file = "BAC_NiG_5min.Rdata")
tic();AIG_NiG_5min <- rolling.nig(AIG_5minute,Window,Refit,cl);toc()
save(AIG_NiG_5min,file = "AIG_NiG_5min.Rdata")
stopCluster(cl)



# ;    sigma <- sd(dat);    mean <- mean(dat);
# 
# rho <- 3*(kurt-3)/S^2 - 4
# mu <- mean - 3*sigma/(rho*S)
# delta <- (3*sigma*sqrt(rho-1))/(abs(S)*rho)
# alpha <- (3*sqrt(rho))/(sigma*(rho-1)*abs(S))
# beta <- 3/(sigma*(rho-1)*S)
# 
# start <- c(mu,delta,alpha,beta)

# Check and fix weirdness.
# if(class(parameters) == "list"){
#   parameters <- do.call(rbind,parameters)
#   parameters <- parameters[,1:4]
# }
# 
# parameters <- as.data.frame(parameters)
# 
# if(dim(parameters)[2]>4 & dim(parameters)[1]<=4){
#   parameters <- t(parameters)
# }else if(dim(parameters)[2]>4 & dim(parameters)[1]>4){
#   return("Wrong parameter dimensions")
# }





