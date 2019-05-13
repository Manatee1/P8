

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

Min_var_pf <- function(data1,data2,intervals){
  
  return1 <- data1[,2] ; return2 <- data2[,2]; Time <- data1[,1]
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
  
  PF_returns <- data.frame(Time,rep(0,length(Time)))
  for(i in 1:nrow(intervals)){
    PF_returns[intervals[i,1]:intervals[i,2],2] <- 
      weights[i,2]*data[intervals[i,1]:intervals[i,2],1] + 
      weights[i,3]*data[intervals[i,1]:intervals[i,2],2]
  }
  
  
  
  return(list(Weights = weights,PF_returns = PF_returns,Profit = exp(sum(PF_returns[,2]))-1))
}


Markowitz_portfolio <- Min_var_pf(WFC.returns,BAC.returns,Interval)



ggplot(data = Markowitz_portfolio$Weights) + geom_line(aes(x = Time,y = w1,group = 1,colour = "WFC")) +
  geom_line(aes(x = Time,y = w2,group = 1,colour = "BAC")) + ylim(c(-0.2,1.2)) + ylab("Weights") +
  theme(legend.title = element_blank() ) + geom_hline(yintercept = 0.5)# +
  #scale_color_manual(values = c("Red","Blue"))













