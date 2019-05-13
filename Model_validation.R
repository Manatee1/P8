library(tidyverse) ; library(GeneralizedHyperbolic)



# 1min --------------------------------------------------------------------




load("Rdata/Clean_data.RData")
load("Rdata/NIG_estimates/BAC_NIG_forecasts_month.rdata")

rm(AIG,BAC,WFC.data)


window <- 391*21 ; refit <- 391
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

Interval = Interval_Matrix(BAC.returns, window,refit)



time1 <- 1 ; time2 <- 900 ; 

f_local_BAC1 = function(x){dnig(x,param = BAC_NiG_forecasts_month[time1,2:5])}

h1 <- ggplot(BAC.returns[Interval[time1,1]:Interval[time1,2],],aes(Return)) + geom_histogram(aes(y = stat(density)),
                                                       color="blue", fill="blue",bins = 100) + 
  xlim(c(-0.01,0.01)) + stat_function(
    fun = f_local_BAC1, 
    #args = list(x = x, param = sd(df$x)), 
    lwd = 1, 
    col = 'red'
  ) #+   ggtitle("Histogram of Bank of America Returns")
h1





f_local_BAC2 = function(x){dnig(x,param = BAC_NiG_forecasts_month[time2,2:5])}

h2 <- ggplot(BAC.returns[Interval[time2,1]:Interval[time2,2],],aes(Return)) + geom_histogram(aes(y = stat(density)),
                                                                  color="blue", fill="blue",bins = 100) + 
  xlim(c(-0.05,0.05)) + stat_function(
    fun = f_local_BAC2, 
    #args = list(x = x, param = sd(df$x)), 
    lwd = 1, 
    col = 'red'
  ) +  ggtitle("Histogram of Bank of America Returns")
h2



qq1 <- ggplot(BAC.returns[Interval[time1,1]:Interval[time1,2],], aes(sample = Return)) +
  stat_qq(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_NiG_forecasts_month[time1,2:5]) + 
  stat_qq_line(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_NiG_forecasts_month[time1,2:5])

qq2 <- ggplot(BAC.returns[Interval[time2,1]:Interval[time2,2],], aes(sample = Return)) +
  stat_qq(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_NiG_forecasts_month[time2,2:5]) + 
  stat_qq_line(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_NiG_forecasts_month[time2,2:5])


# source("Multiplot.r")
# 
# multiplot(h1,qq1,cols = 2)
# multiplot(h2,qq2,cols = 2)


library(ggpubr)
ggarrange(h1+ ggtitle(NULL), qq1 , 
          labels = NULL,
          ncol = 2, nrow = 1)


ggarrange(h2+ ggtitle(NULL), qq2 , 
          labels = NULL,
          ncol = 2, nrow = 1)



# 5min --------------------------------------------------------------------



load("Rdata/Clean_data_5min.RData")
load("Rdata/NIG_estimates/BAC_NIG_5min.rdata")

rm(AIG.5min,BAC.5min,WFC.5min)


window <- 78*21 ; refit <- 78
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

Interval = Interval_Matrix(BAC.returns.5min, window,refit)



time1 <- 1 ; time2 <- 1075 ; 

f_local_BAC1 = function(x){dnig(x,param = BAC_NiG_5min[time1,2:5])}

h1 <- ggplot(BAC.returns.5min[Interval[time1,1]:Interval[time1,2],],aes(Return)) + geom_histogram(aes(y = stat(density)),
                                                                                             color="blue", fill="blue",bins = 100) + 
  xlim(c(-0.01,0.01)) + stat_function(
    fun = f_local_BAC1, 
    #args = list(x = x, param = sd(df$x)), 
    lwd = 1, 
    col = 'red'
  ) #+   ggtitle("Histogram of Bank of America Returns")
h1





f_local_BAC2 = function(x){dnig(x,param = BAC_NiG_5min[time2,2:5])}

h2 <- ggplot(BAC.returns.5min[Interval[time2,1]:Interval[time2,2],],aes(Return)) + geom_histogram(aes(y = stat(density)),
                                                                                             color="blue", fill="blue",bins = 100) + 
  xlim(c(-0.05,0.05)) + stat_function(
    fun = f_local_BAC2, 
    #args = list(x = x, param = sd(df$x)), 
    lwd = 1, 
    col = 'red'
  ) +  ggtitle("Histogram of Bank of America Returns")
h2



qq1 <- ggplot(BAC.returns.5min[Interval[time1,1]:Interval[time1,2],], aes(sample = Return)) +
  stat_qq(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_NiG_5min[time1,2:5]) + 
  stat_qq_line(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_NiG_5min[time1,2:5])

qq2 <- ggplot(BAC.returns.5min[Interval[time2,1]:Interval[time2,2],], aes(sample = Return)) +
  stat_qq(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_NiG_5min[time2,2:5]) + 
  stat_qq_line(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_NiG_5min[time2,2:5])


# source("Multiplot.r")
# 
# multiplot(h1,qq1,cols = 2)
# multiplot(h2,qq2,cols = 2)


library(ggpubr)
ggarrange(h1+ ggtitle(NULL), qq1 , 
          labels = NULL,
          ncol = 2, nrow = 1)


ggarrange(h2+ ggtitle(NULL), qq2 , 
          labels = NULL,
          ncol = 2, nrow = 1)




BAC.returns.5min[c(Interval[time1,1],Interval[time1,2]),1]
BAC.returns.5min[c(Interval[time2,1],Interval[time2,2]),1]






