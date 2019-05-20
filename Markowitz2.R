

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse,lubridate,magrittr)
load("Rdata/Clean_Data_5minutes.RData")

Interval = function(i, Window, Refit, Data){
  I = 1:Window + (i-1)*Refit
  return(Data[I,])
}


Interval_Forecast = function(i,Window,Refit,Data_1,Data_2){
  #browser()
  I_1 = Interval(i-1,Window,Refit,Data_1)
  I_2 = Interval(i-1,Window,Refit,Data_2)
  
  J_1 = Interval(i,Window,Refit,Data_1)
  J_2 = Interval(i,Window,Refit,Data_2)
  
  New = !(J_2[["Time"]] %in% I_2[["Time"]])
  Index = which(New)
  
  D_1 = J_1[Index,"Price"]
  D_2 = J_2[Index,"Price"]
  Time = J_1[Index, "Time"]
  
  D = cbind(Time,D_1, D_2)
  names(D) = c("Time","Price_1","Price_2")
  return(D)
}

markowitz <- function(x){ # Function returning minimum variance weights
  covariance <- cov(data[x[1]:x[2],])
  weights <- (solve(covariance,c(1,1)))/drop(t(c(1,1))%*%solve(covariance,c(1,1)))
  return(list("Weights" = weights))
}


data <- data.frame(BAC_5minute[["Returns"]],WFC_5minute[["Returns"]])

n <- ceiling((length(data[,1])-Window)/Refit)


Collected_Data = lapply(2:n,Interval_Forecast,Window = Window, Refit = Refit, Data_1 = WFC_5minute, Data_2 = BAC_5minute)



intervals <- list()
intervals[[1]] <- c(1,Window-1)
for(i in 2:(number)){
  intervals[[i]] <- intervals[[i-1]] + Refit
}

weights_markowitz <- lapply(X = intervals,FUN = markowitz)

W = weights_markowitz %>% lapply(function(x){x[["Weights"]]}) %>% do.call(rbind,.)



Get_Price = function(i, Window,Refit, Data_1, Data_2, Weights){
  library(tidyselect)
  w = Weights[[i]][["Weights"]]
  
  i = i + 1
  
  D = Interval_Forecast(i,Window,Refit,Data_1,Data_2)
  
  P = D %>% mutate(Price = w[1]*Price_1 + w[2]*Price_2) %>% select(Time,Price)
  
  return(P)
}

Price_markowitz = lapply(1:(n-1), Get_Price, Window = Window, Refit = Refit,Data_1 = WFC_5minute, Data_2 = BAC_5minute, Weights = weights_markowitz)
M_Prices = do.call(rbind,Price_markowitz)




# Calculate Returns -------------------------------------------------------


M_Prices %<>% mutate(Logreturns = c(0,diff(log(Price)))) 
WFC_5minute %<>% mutate(Logreturns = c(0,diff(log(Price)))) 
BAC_5minute %<>% mutate(Logreturns = c(0,diff(log(Price)))) 



WFC_split = with(WFC_5minute, which(Logreturns == min(Logreturns)))
M_Prices_split = with(M_Prices, which(Logreturns == min(Logreturns)))

#Replace Split in Portfolio
Split_Time = M_Prices[M_Prices_split,"Time"]
j = 0
for(i in Price_markowitz){
  j = j + 1
  if(Split_Time %in% i[,"Time"]) break
}

P_t = (W[j,1]*2*WFC_5minute[WFC_split,"Price"] + W[j,2]*BAC_5minute[WFC_split,"Price"]) 
P_s = (W[j,1]*WFC_5minute[WFC_split-1,"Price"] + W[j,2]*BAC_5minute[WFC_split-1,"Price"]) 
Logreturns_Split = log(P_t) - log(P_s)

M_Prices[M_Prices_split,"Logreturns"] = Logreturns_Split
WFC_5minute[WFC_split,"Logreturns"] = log(2*WFC_5minute[WFC_split,"Price"]) - log(WFC_5minute[WFC_split-1,"Price"])



with(WFC_5minute, plot(Time, Logreturns, type = "l"))
with(BAC_5minute, lines(Time, Logreturns, col = "red"))
with(M_Prices, lines(Time, Logreturns, col = "green"))


with(WFC_5minute, sum(Logreturns))
with(BAC_5minute, sum(Logreturns))
with(M_Prices, sum(Logreturns))

with(WFC_5minute, plot(Time,cumsum(Logreturns), type = "l",ylim = c(-2.5,0.3)))
with(BAC_5minute, lines(Time, cumsum(Logreturns), col = "red"))
with(M_Prices, lines(Time, cumsum(Logreturns), col = "green"))





annual_return <- function(data,year){
  #browser()
  str <-  str_split(as.character(data[["Time"]]),"-"); n <- length(str)
  index <- lapply(str, function(x)if(as.character(year) %in% x[1]){return(1)}else{return(0)})
  
  New_data <- data[which(index == 1),]
  Return <- exp(sum(New_data[["Logreturns"]]))-1
  return(Return)
}


for(i in 2005:2009){
  print(annual_return(M_Prices,i))
}


exp(sum(M_Prices$Logreturns))-1

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

int_mat <- Interval_Matrix(BAC_5minute[,c(1,3)], Window,Refit)
times <- WFC_5minute[["Time"]][int_mat[,2]+1]

Weights <- data.frame(times,W); colnames(Weights) <- c("Time","w1","w2")

ggplot(data = Weights) + geom_line(aes(x = Time,y = w1,group = 1,colour = "BAC")) +
  geom_line(aes(x = Time,y = w2,group = 1,colour = "WFC")) + ylab("Weights") +
  theme(legend.title = element_blank() ) + geom_hline(yintercept = 0.5) +
  scale_y_continuous(name = "Weights",breaks = c(-0.2,0,0.2,0.4,0.6,0.8,1,1.2),limits = c(-0.2,1.2))# +
#scale_color_manual(values = c("Red","Blue"))


ggplot() + 

with(WFC_5minute, plot(Time,cumsum(Logreturns), type = "l",ylim = c(-2.5,0.3)))
with(BAC_5minute, lines(Time, cumsum(Logreturns), col = "red"))
with(M_Prices, lines(Time, cumsum(Logreturns), col = "green"))