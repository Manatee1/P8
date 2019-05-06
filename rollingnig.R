library(tidyverse) ; library(GeneralizedHyperbolic) ; library(parallel)
load("Clean_data.RData")


rolling.nig <- function(data,window,refit.every,cl){

  Time <- data$Time ; data <- data$Return
  #browser()
  number <- ceiling((length(data)-window)/refit.every)
  intervals <- matrix(0,nrow = number,ncol = 2)
  intervals[1,] <- c(1,window-1)
  for(i in 2:(number)){
    intervals[i,] <- intervals[i-1,] + refit.every
  }
  
  fitting <- function(x){
    library(GeneralizedHyperbolic)
    start <- try(nigFitStart(data[x[1]:x[2]],startValues = "MoM",startMethodMoM = "Nelder-Mead"))
    if("try-error" %in% class(start)){return("Error in MoM")}
    fit <- try(nigFit(data[x[1]:x[2]], paramStart = start,method = "Nelder-Mead"))
    if("try-error" %in% class(fit)){par <- start}else{par <- as.numeric(fit$param)}
    return(par)
  }
  
  
  parameters <- parApply(cl,intervals,MARGIN = 1,FUN = fitting)
  stopCluster(cl)
  
  if(class(parameters) == "list"){
    parameters <- do.call(rbind,parameters)
    parameters <- parameters[,1:4]
  }
  
  if(dim(parameters)[2]>4 & dim(parameters)[1]<=4){
    parameters <- t(parameters)
  }else if(dim(parameters)[2]>4 & dim(parameters)[1]>4){
    return("Wrong parameter dimensions")
  }
  
  times <- Time[intervals[,2]+1]
  result <- data.frame(times,parameters);colnames(result) <- c("Time","mu","delta","alpha","beta")
  
  
  return(result)
}



# x <- BAC.returns[1:(10*391),]; w <- 5*391;refit.every <- 391

library(tictoc)
cl <- makePSOCKcluster(20)
tic();fits <- rolling.nig(WFC.returns,5*391,30,cl);toc()
save(fits,file = "WFC_NiG_forecasts.Rdata")















