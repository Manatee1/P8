library(tidyverse) ; library(GeneralizedHyperbolic) ; library(parallel)



rolling.nig <- function(data,window,refit.every,cl){
  
  number <- ceiling(length(data)/refit.every)-(window/refit.every)
  intervals <- matrix(0,nrow = number,ncol = 2)
  intervals[1,] <- c(1,window)
  for(i in 2:(number)){
    intervals[i,] <- intervals[i-1,] + refit.every
  }
  
  fitting <- function(x){
    library(GeneralizedHyperbolic)
    start <- nigFitStart(data[x[1]:x[2]],startValues = "MoM",startMethodMoM = "Nelder-Mead")
    fit <- try(nigFit(data[x[1]:x[2]], paramStart = start,method = "Nelder-Mead"))
    if("try-error" %in% class(fit)) par <- start else{par <- as.numeric(fit$param)}
    return(par)
  }
  
  
  parameters <- parApply(cl,intervals,MARGIN = 1,FUN = fitting) %>% t()
  
  stopCluster(cl)
  
  return(parameters)
}

cl <- makePSOCKcluster(4)
tic();fits <- rolling.nig(x,w,refit,cl);toc()
















