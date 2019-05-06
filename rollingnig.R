library(tidyverse) ; library(GeneralizedHyperbolic) ; library(parallel)
load("Clean_data.RData")


rolling.nig <- function(data,window,refit.every,cl){

  Time <- data$Time ; data <- data$Return
  #browser()
  number <- ceiling((length(data)-window)/refit.every)
  intervals <- matrix(0,nrow = number,ncol = 2)
  intervals[1,] <- c(1,window)
  for(i in 2:(number)){
    intervals[i,] <- intervals[i-1,] + refit.every
  }
  
  fitting <- function(x){
    library(GeneralizedHyperbolic)
    start <- nigFitStart(data[x[1]:x[2]],startValues = "MoM",startMethodMoM = "Nelder-Mead")
    fit <- try(nigFit(data[x[1]:x[2]], paramStart = start,method = "Nelder-Mead"))
    if(class(fit) == "try-error"){par <- start}else{par <- as.numeric(fit$param)}
    return(par)
  }
  
  #browser()
  parameters <- parApply(cl,intervals,MARGIN = 1,FUN = fitting) %>% t()
  stopCluster(cl)
  

  times <- Time[intervals[,2]+1]
  result <- data.frame(times,parameters);colnames(result) <- c("Time","mu","delta","alpha","beta")
  
  
  return(result)
}



# x <- BAC.returns[1:(10*391),]; w <- 5*391;refit.every <- 391


cl <- makePSOCKcluster(4)
tic();fits <- rolling.nig(BAC.returns[1:3910,],5*391,60,cl);toc()
save(fits,file = "Rolling_forecasts.Rdata")















