

Interval_Matrix = function(data,window,refit){
  #browser()
  Time <- data$Time ; data <- data$Returns
  #browser()
  number <- ceiling((length(data)-window)/refit)
  intervals <- matrix(0,nrow = number,ncol = 2)
  intervals[1,] <- c(1,window-1)
  for(i in 2:(number)){
    intervals[i,] <- intervals[i-1,] + refit
  }
  return(intervals)
}



Min_var_pf <- function(data1,data2,intervals){
  
  return1 <- data1$Returns ; return2 <- data2$Returns; Time <- data1$Time
  data <- data.frame(return1,return2)
  
  markowitz <- function(x){
    covariance <- cov(data[x[1]:x[2],])
    weights <- (solve(covariance,c(1,1)))/drop(t(c(1,1))%*%solve(covariance,c(1,1)))
    return(weights)
  }
  #browser()
  
  
  weights <- apply(X = intervals,MARGIN = 1,FUN = markowitz)
  
  if(dim(weights)[2]>2 & dim(weights)[1]<=4){
    weights <- t(weights)
  }else if(dim(weights)[2]>2 & dim(weights)[1]>4){
    return("Wrong dimensions")
  }
  
  times <- Time[intervals[,2]+1]
  weights <- data.frame(times,weights);colnames(weights) <- c("Time","w1","w2")
  weights$Time <- lubridate::ymd_hms(weights$Time)
  
  #browser()
  
  PF_returns <- data.frame(Time,rep(0,length(Time)))
  
  for(i in 1:(nrow(intervals)-1)){
    PF_returns[(intervals[i,2]+1):intervals[(i+1),2],2] <- log(1 + 
      weights[i,2]*(exp(data1[(intervals[i,2]+1):intervals[(i+1),2],2])-1) +
      weights[i,3]*(exp(data2[(intervals[i,2]+1):intervals[(i+1),2],2])-1) 
    )
  }
  
  
  
  return(list(Weights = weights,PF_returns = PF_returns,Profit = exp(sum(PF_returns[,2]))-1))
}



# 5min --------------------------------------------------------------------

pacman::p_load(tidyverse)
load("Rdata/Clean_Data_5minutes.RData")
rm(AIG_5min,BAC_5min,WFC_5min,AIG_returns_5min)


window = 21*78
refit = 78

Interval = Interval_Matrix(BAC_5minute[,c(1,3)], window,refit)


Markowitz_portfolio <- Min_var_pf(WFC_5minute,BAC_5minute,Interval)



ggplot(data = Markowitz_portfolio$Weights) + geom_line(aes(x = Time,y = w1,group = 1,colour = "WFC")) +
  geom_line(aes(x = Time,y = w2,group = 1,colour = "BAC")) + ylim(c(-0.2,1.2)) + ylab("Weights") +
  theme(legend.title = element_blank() ) + geom_hline(yintercept = 0.5)# +
#scale_color_manual(values = c("Red","Blue"))


plot(Markowitz_portfolio$PF_returns[,2],type = "l")


plot(exp(cumsum(Markowitz_portfolio$PF_returns[,2]))-1,type = "l",ylim = c(-0.8,0.2),col ="green")
lines(exp(cumsum(BAC_5minute$Return))-1,type = "l",col = "red")
lines(exp(cumsum(WFC_5minute$Return))-1,type = "l",col = "black")





# 1min  ---------------------------------------------------------

load("Rdata/Clean_data.RData")

rm(AIG,BAC,WFC.data,AIG.returns)

window = 21*391
refit = 391

Interval = Interval_Matrix(BAC.returns, window,refit)


Markowitz_portfolio <- Min_var_pf(WFC.returns,BAC.returns,Interval)



ggplot(data = Markowitz_portfolio$Weights) + geom_line(aes(x = Time,y = w1,group = 1,colour = "WFC")) +
  geom_line(aes(x = Time,y = w2,group = 1,colour = "BAC")) + ylim(c(-0.2,1.2)) + ylab("Weights") +
  theme(legend.title = element_blank() ) + geom_hline(yintercept = 0.5)# +
#scale_color_manual(values = c("Red","Blue"))











