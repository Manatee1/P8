


Interval_Matrix = function(data,window,refit){ # Constructs estimation window indexes
  Time <- data$Time ; data <- data$Returns
  number <- ceiling((length(data)-window)/refit)
  intervals <- matrix(0,nrow = number,ncol = 2)
  intervals[1,] <- c(1,window-1)
  for(i in 2:(number)){
    intervals[i,] <- intervals[i-1,] + refit
  }
  return(intervals)
}



Min_var_pf <- function(data1,data2,intervals){
  # Separate returns and time
  return1 <- data1[["Returns"]] ; return2 <- data2[["Returns"]]; Time <- data1[["Time"]]
  data <- data.frame(return1,return2)
  
  
  
  markowitz <- function(x){ # Function returning minimum variance weights
    covariance <- cov(data[x[1]:x[2],])
    weights <- (solve(covariance,c(1,1)))/drop(t(c(1,1))%*%solve(covariance,c(1,1)))
    return(weights)
  }
  
  weights <- apply(X = intervals,MARGIN = 1,FUN = markowitz) # Finding weights for each day.
  
  # Fix dimensions if necessary
  if(dim(weights)[2]>2 & dim(weights)[1]<=4){
    weights <- t(weights)
  }else if(dim(weights)[2]>2 & dim(weights)[1]>4){
    return("Wrong dimensions")
  }
  
  
  #browser()
  #Formatting for return object.
  times <- Time[intervals[,2]+1]
  weights <- data.frame(times,weights);colnames(weights) <- c("Time","w1","w2")
  weights$Time <- lubridate::ymd_hms(weights$Time)
  
  W <- data.frame(Time,rep(0,length(Time)),rep(0,length(Time)))
  PF <- W ; colnames(W) <- c("Time","w1","w2") ; colnames(PF) <- c("Time","Price","Returns")
  rebalance_times <- Time[intervals[,2]]+1 ; w <- 1
  current <- c(0,0)
  for(i in as.character(Time)){
    W[w,c(2,3)] <- current
    index <- which(Time == i);
    if(ymd_hms(i) %within% interval(Time[1],Time[intervals[1,2]])){next}
    if(i %in% rebalance_times){
      index_w <- which(weights[["Time"]] == i)
      current <- weights[index_w,c(2,3)]
      W[w,c(2,3)] <- current
    }
    PF[index,][["Price"]] <- W[w,2]*data1[index,][["Price"]] + W[w,3]*data2[index,][["Price"]]
    
    w <- w+1
  }
  
  
  PF[["Returns"]] <- c(rep(0,intervals[1,2]),diff(log(PF[["Price"]][intervals[1,2]:length(Time)])))
  
  
  # Calculating portfolio returns for each observation.
  # PF_returns <- data.frame(Time,rep(0,length(Time))) ; colnames(PF_returns) <- c("Time","Returns")
  # for(i in 1:(nrow(intervals)-1)){
  #   return1 <- (exp(data1[(intervals[i,2]+1):intervals[(i+1),2],]$Returns)-1)
  #   return2 <- (exp(data2[(intervals[i,2]+1):intervals[(i+1),2],]$Returns)-1)
  # 
  #   PF_returns[(intervals[i,2]+1):intervals[(i+1),2],2] <- log(1 +
  #                                                 (weights[i,2]*return1 + weights[i,3]*return2))
  # }
  
  # Return portfolio weights, returns and the total profit.
  return(list(Weights = W,PF = PF,Profit = exp(sum(PF[["Returns"]]))-1))
}



# 5min --------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse,lubridate)
load("Rdata/Clean_Data_5minutes.RData")


WFC_split <- 31918
Interval = Interval_Matrix(BAC_5minute[,c(1,3)], Window,Refit)


Markowitz_portfolio <- Min_var_pf(WFC_5minute,BAC_5minute,Interval)



ggplot(data = Markowitz_portfolio$Weights) + geom_line(aes(x = Time,y = w1,group = 1,colour = "WFC")) +
  geom_line(aes(x = Time,y = w2,group = 1,colour = "BAC")) + ylab("Weights") +
  theme(legend.title = element_blank() ) + geom_hline(yintercept = 0.5) +
  scale_y_continuous(name = "Weights",breaks = c(-0.2,0,0.2,0.4,0.6,0.8,1,1.2),limits = c(-0.2,1.2))# +
#scale_color_manual(values = c("Red","Blue"))


plot(Markowitz_portfolio$PF_returns[,2],type = "l")


plot(exp(cumsum(Markowitz_portfolio$PF_returns[,2]))-1,type = "l",ylim = c(-1,0.25),col ="green")
lines(exp(cumsum(BAC_5minute$Returns))-1,type = "l",col = "red")
lines(exp(cumsum(WFC_5minute$Returns))-1,type = "l",col = "black")



min(exp(cumsum(BAC_5minute$Returns))-1)
min(exp(cumsum(Markowitz_portfolio$PF_returns$rep.0..length.Time..))-1)



