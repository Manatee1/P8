library(VineCopula);library(copula)
library(tidyverse) ; library(GeneralizedHyperbolic)




w <- 2*391;refit <- 20
x <- BAC.returns$Return[1:(10*391+1)]


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
  library(parallel)
  
  parameters <- parApply(cl,intervals,MARGIN = 1,FUN = fitting) %>% t()
  
  stopCluster(cl)
  
  return(parameters)
}

cl <- makePSOCKcluster(4)
tic();fits <- rolling.nig(x,w,refit,cl);toc()

f_local_BAC = function(x){dnig(x,param = fits[1,])}



temp <- data.frame(x[1:w])

h1 <- ggplot(temp,aes(temp$x.1.w.)) + geom_histogram(aes(y = stat(density)),
                                                       color="blue", fill="blue",bins = 30) + 
  xlim(c(-0.005,0.005)) + stat_function(
    fun = f_local_BAC, 
    #args = list(x = x, param = sd(df$x)), 
    lwd = 1, 
    col = 'red'
  ) +
  ggtitle("Histogram of Bank of America Returns")
h1

















