library(tidyverse) ; library(GeneralizedHyperbolic)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))




# 5min --------------------------------------------------------------------



load("Rdata/Clean_Data_5minutes.RData")
load("Rdata/Tokes_Fits.rdata")

rm(AIG,BAC,WFC)

# AIG_fits %<>% lapply(function(x)x[["param"]]) %>% do.call(rbind,.)
# WFC_fits %<>% lapply(function(x)x[["param"]]) %>% do.call(rbind,.)
# BAC_fits %<>% lapply(function(x)x[["param"]]) %>% do.call(rbind,.)

Marginal_validation_plot <- function(Data,start.time,Window,Refit,fits){
  
  fits %<>% lapply(function(x)x[["param"]]) %>% do.call(rbind,.)
  
  
  Interval_Matrix = function(data,window,refit){
    Time <- data[["Time"]] ; data <- data[["Returns"]]
    #browser()
    number <- ceiling((length(data)-window)/refit)
    intervals <- matrix(0,nrow = number,ncol = 2)
    intervals[1,] <- c(1,window-1)
    for(i in 2:(number)){
      intervals[i,] <- intervals[i-1,] + refit
    }
    return(intervals)
  }
  
  Interval = Interval_Matrix(BAC_5minute, Window,Refit)
  
  f <- function(x){dnig(x,param = fits[start.time,])}
  
  
  histogram <- ggplot(Data[Interval[start.time,1]:Interval[start.time,2],],aes(Returns)) + 
    geom_histogram(aes(y = stat(density),fill = 1,colour = 1),bins = 100) + 
    #xlim(c(-0.01,0.01)) + 
    stat_function(
      fun = f, 
      lwd = 1, colour = 1
    ) + theme(legend.position = "none",legend.title = element_blank() )
  
  quants <- function(p){
    GeneralizedHyperbolic::qnig(p,param = fits[time1,],intTol = 1e-12)
  }
  
  qqplot <- ggplot(Data[Interval[time1,1]:Interval[time1,2],], aes(sample = Returns)) +
    stat_qq(distribution = quants) + 
    stat_qq_line(distribution = quants)
  
  
  ggpubr::ggarrange(histogram + ggtitle(NULL), qqplot , 
                    labels = NULL,
                    ncol = 2, nrow = 1)
}

time1 <- 1 ; time2 <- 1072 ; 

Marginal_validation_plot(BAC_5minute,time1,Window,Refit,BAC_fits)
Marginal_validation_plot(BAC_5minute,time2,Window,Refit,BAC_fits)

Marginal_validation_plot(WFC_5minute,time1,Window,Refit,WFC_fits)
Marginal_validation_plot(WFC_5minute,time2,Window,Refit,WFC_fits)

Marginal_validation_plot(AIG_5minute,time1,Window,Refit,AIG_fits)
Marginal_validation_plot(AIG_5minute,time2,Window,Refit,AIG_fits)





# 
# 
# f_local_BAC1 = function(x){dnig(x,param = BAC_fits[time1,])}
# 
# h1 <- ggplot(BAC_5minute[Interval[time1,1]:Interval[time1,2],],aes(Returns)) + 
#   geom_histogram(aes(y = stat(density),fill = 1,colour = 1),bins = 100) + 
#   xlim(c(-0.01,0.01)) + stat_function(
#     fun = f_local_BAC1, 
#     lwd = 1, colour = 1
#   ) + theme(legend.position = "none",legend.title = element_blank() )
# 
# 
# h1
# 
# 
# 
# 
# 
# f_local_BAC2 = function(x){dnig(x,param = BAC_fits[time2,])}
# 
# h2 <- ggplot(BAC_5minute[Interval[time2,1]:Interval[time2,2],],aes(Returns)) + 
#   geom_histogram(aes(y = stat(density),color=1, fill=1), bins = 100) + 
#   xlim(c(-0.05,0.05)) + stat_function(
#     fun = f_local_BAC2, 
#     #args = list(x = x, param = sd(df$x)), 
#     lwd = 1, color=1
#   ) + theme(legend.position = "none",legend.title = element_blank() )
# h2
# 
# 
# 
# qq1 <- ggplot(BAC_5minute[Interval[time1,1]:Interval[time1,2],], aes(sample = Returns)) +
#   stat_qq(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_fits[time1,]) + 
#   stat_qq_line(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_fits[time1,])
# 
# qq2 <- ggplot(BAC_5minute[Interval[time2,1]:Interval[time2,2],], aes(sample = Returns)) +
#   stat_qq(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_fits[time2,]) + 
#   stat_qq_line(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_fits[time2,])
# 
# 
# # source("Multiplot.r")
# # 
# # multiplot(h1,qq1,cols = 2)
# # multiplot(h2,qq2,cols = 2)
# 
# 
# library(ggpubr)
# ggarrange(h1+ ggtitle(NULL), qq1 , 
#           labels = NULL,
#           ncol = 2, nrow = 1)
# 
# 
# ggarrange(h2+ ggtitle(NULL), qq2 , 
#           labels = NULL,
#           ncol = 2, nrow = 1)
# 
# 
# Interval_f = function(i, Window, Refit, Data){
#   I = 1:Window + (i-1)*Refit
#   return(Data[I,])
# }
# Interval_f(1072,Window,Refit,BAC_5minute)
# 
# 










# 1min --------------------------------------------------------------------


# 
# 
# load("Rdata/Clean_data.RData")
# load("Rdata/NIG_estimates/BAC_NIG_forecasts_month.rdata")
# 
# rm(AIG,BAC,WFC.data)
# 
# 
# window <- 391*21 ; refit <- 391
# Interval_Matrix = function(data,window,refit){
#   Time <- data$Time ; data <- data$Return
#   #browser()
#   number <- ceiling((length(data)-window)/refit)
#   intervals <- matrix(0,nrow = number,ncol = 2)
#   intervals[1,] <- c(1,window-1)
#   for(i in 2:(number)){
#     intervals[i,] <- intervals[i-1,] + refit
#   }
#   return(intervals)
# }
# 
# Interval = Interval_Matrix(BAC.returns, window,refit)
# 
# 
# 
# time1 <- 1 ; time2 <- 900 ; 
# 
# f_local_BAC1 = function(x){dnig(x,param = BAC_NiG_forecasts_month[time1,2:5])}
# 
# h1 <- ggplot(BAC.returns[Interval[time1,1]:Interval[time1,2],],aes(Return)) + geom_histogram(aes(y = stat(density)),
#                                                                                              color="blue", fill="blue",bins = 100) + 
#   xlim(c(-0.01,0.01)) + stat_function(
#     fun = f_local_BAC1, 
#     #args = list(x = x, param = sd(df$x)), 
#     lwd = 1, 
#     col = 'red'
#   ) #+   ggtitle("Histogram of Bank of America Returns")
# h1
# 
# 
# 
# 
# 
# f_local_BAC2 = function(x){dnig(x,param = BAC_NiG_forecasts_month[time2,2:5])}
# 
# h2 <- ggplot(BAC.returns[Interval[time2,1]:Interval[time2,2],],aes(Return)) + geom_histogram(aes(y = stat(density)),
#                                                                                              color="blue", fill="blue",bins = 100) + 
#   xlim(c(-0.05,0.05)) + stat_function(
#     fun = f_local_BAC2, 
#     #args = list(x = x, param = sd(df$x)), 
#     lwd = 1, 
#     col = 'red'
#   ) +  ggtitle("Histogram of Bank of America Returns")
# h2
# 
# 
# 
# qq1 <- ggplot(BAC.returns[Interval[time1,1]:Interval[time1,2],], aes(sample = Return)) +
#   stat_qq(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_NiG_forecasts_month[time1,2:5]) + 
#   stat_qq_line(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_NiG_forecasts_month[time1,2:5])
# 
# qq2 <- ggplot(BAC.returns[Interval[time2,1]:Interval[time2,2],], aes(sample = Return)) +
#   stat_qq(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_NiG_forecasts_month[time2,2:5]) + 
#   stat_qq_line(distribution = GeneralizedHyperbolic::qnig,dparams = BAC_NiG_forecasts_month[time2,2:5])
# 
# 
# # source("Multiplot.r")
# # 
# # multiplot(h1,qq1,cols = 2)
# # multiplot(h2,qq2,cols = 2)
# 
# 
# library(ggpubr)
# ggarrange(h1+ ggtitle(NULL), qq1 , 
#           labels = NULL,
#           ncol = 2, nrow = 1)
# 
# 
# ggarrange(h2+ ggtitle(NULL), qq2 , 
#           labels = NULL,
#           ncol = 2, nrow = 1)

