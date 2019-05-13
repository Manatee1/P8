library(tidyverse) ; library(GeneralizedHyperbolic) ; library(parallel)
load("Clean_data.RData")
load("Clean_data_5min.rdata")
rownames(AIG.5min) <- 1:length(AIG.5min)


rolling.nig <- function(data,window,refit,cl){
  
  Time <- data$Time ; data <- data$Return# Separating timestamps and returns

  number <- ceiling((length(data)-window)/refit) # Number of estimations required
  
  # Matrix of estimation window intervals.
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

  #browser()
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



library(tictoc)
cl <- makePSOCKcluster(20)
tic();WFC_NiG_forecasts_month <- rolling.nig(WFC.returns,21*391,391,cl);toc()
save(WFC_NiG_forecasts_month,file = "WFC_NiG_forecasts_month.Rdata")
tic();BAC_NiG_forecasts_month <- rolling.nig(BAC.returns,21*391,391,cl);toc()
save(BAC_NiG_forecasts_month,file = "BAC_NiG_forecasts_month.Rdata")
AIG_NiG_forecasts_month <- rolling.nig(AIG.returns,21*391,391,cl);toc()
save(AIG_NiG_forecasts_month,file = "AIG_NiG_forecasts_month.Rdata")
stopCluster(cl)



window <- 21*78 ; refit <- 78
library(tictoc)
cl <- makePSOCKcluster(20)
tic();WFC_NiG_5min <- rolling.nig(WFC.returns.5min,window,refit,cl);toc()
save(WFC_NiG_5min,file = "WFC_NiG_5min.Rdata")
tic();BAC_NiG_5min <- rolling.nig(BAC.returns.5min,window,refit,cl);toc()
save(BAC_NiG_5min,file = "BAC_NiG_5min.Rdata")
AIG_NiG_5min <- rolling.nig(AIG.returns.5min,window,refit,cl);toc()
save(AIG_NiG_5min,file = "AIG_NiG_5min.Rdata")
stopCluster(cl)



