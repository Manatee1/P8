#Calculate and Minimize Value at Risk 


# Load Data ---------------------------------------------------------------

load("Clean_Data_5minutes.RData")
load("Draws_tCopula.RData")
VaR = function(x,gamma = 0.05){
  L = -x
  q = quantile(L,1-gamma)
  return(q)
}
CVaR = function(x, alpha = 0.05){
  x_alpha = quantile(x,alpha)
  CVaR = -(1/alpha)*mean(x*(x<= x_alpha))
  return(CVaR)
}
Interval = function(i, Window, Refit, Data){
  I = 1:Window + (i-1)*Refit
  return(Data[I,])
}
failed = function(list){
  failed = rep(0,length(list))
  j = 0
  for(i in list){
    j = j + 1
    if("try-error" %in% class(i)){failed[j] = 1}
  }
  if(sum(failed) == 0){print("No Errors")}else{return(failed)}
}

# Define Function 
Get_CVaR = function(i, Refit, Window, Data_1,Data_2, Draws){
  #browser()
  w_1 = seq(from =-10, to = 11, length.out = 1000)
  w_2 = 1 - w_1
  w = cbind(w_1, w_2)
  
  VaR = function(x,gamma = 0.05){
    L = -x
    q = quantile(as.numeric(L),1-gamma,na.rm = T)
    return(q)
  }
  CVaR = function(x, alpha = 0.05){
    x_alpha = quantile(as.numeric(x),alpha, na.rm = T)
    CVaR = -(1/alpha)*mean(x*(x<= x_alpha))
    return(CVaR)
  }
  Interval = function(i, Window, Refit, Data){
    I = 1:Window + (i-1)*Refit
    return(Data[I,])
  }
  
  Sample = Draws[[i]]
  
  CVaR_hat = apply(w,1, function(x){CVaR(Sample %*% x)})
  index_star = which(CVaR_hat == min(CVaR_hat))
  
  Returns_1 = Interval(i,Window,Refit,Data_1)[["Returns"]]
  Returns_2 = Interval(i,Window,Refit,Data_2)[["Returns"]]
  
  Empirical = cbind(Returns_1,Returns_2)
  
  w_star = w[index_star,]
  CVaR_star = CVaR_hat[index_star]
  VaR_star = VaR(Sample %*% w_star)
  Empirical_VaR = VaR(Empirical %*% w_star)
  Empirical_CVaR = CVaR(Empirical %*% w_star)
  
  return(list("Weights" = w_star, "VaR" = VaR_star, "CVaR" = CVaR_star,"Empirical VaR" = Empirical_VaR, "Empirical CVaR" = Empirical_CVaR))
}



# Find Weights, VaR and CVaR ----------------------------------------------

n = length(Draws_tCopula)

cl = makePSOCKcluster(36)

Weights_tCopula = parLapply(cl,
                            1:n, 
                            try(Get_CVaR),
                            Refit = Refit, 
                            Window = Window, 
                            Data_1 = WFC_5minute, 
                            Data_2 = BAC_5minute, 
                            Draws = Draws_tCopula)

save(Weights_tCopula, file = "Weights_tCopula.RData")

W = Weights_tCopula %>% lapply(function(x){x[["Weights"]]}) %>% do.call(rbind,.)

any(is.na(W))

# Get Profit --------------------------------------------------------------

load("Weights_tCopula.RData")

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

Collected_Data = lapply(2:n,Interval_Forecast,Window = Window, Refit = Refit, Data_1 = WFC_5minute, Data_2 = BAC_5minute)

Get_Price = function(i, Window,Refit, Data_1, Data_2, Weights){
  library(tidyselect)
  w = Weights[[i]][["Weights"]]
  
  i = i + 1
  
  D = Interval_Forecast(i,Window,Refit,Data_1,Data_2)
  
  P = D %>% mutate(Price = w[1]*Price_1 + w[2]*Price_2) %>% select(Time,Price)
  
  return(P)
}


Price_tCopula = lapply(1:(n-1), Get_Price, Window = Window, Refit = Refit,Data_1 = WFC_5minute, Data_2 = BAC_5minute, Weights = Weights_tCopula)
Prices = do.call(rbind,Price_tCopula)


Prices %<>% na.omit()

# Plots -------------------------------------------------------------------

with(WFC_5minute, plot(Time, Price, type = "l", ylim = c(0, max(Price))))
with(BAC_5minute, lines(Time, Price, col = "red"))
with(Prices, lines(Time, Price, col = "green"))


# Calculate Returns -------------------------------------------------------

Prices %<>% mutate(Returns = c(0,diff(Price))) 
WFC_5minute %<>% mutate(Returns = c(0,diff(Price))) 
BAC_5minute %<>% mutate(Returns = c(0,diff(Price))) 

Prices %<>% mutate(Logreturns = c(0,diff(log(Price)))) 
WFC_5minute %<>% mutate(Logreturns = c(0,diff(log(Price)))) 
BAC_5minute %<>% mutate(Logreturns = c(0,diff(log(Price)))) 

with(WFC_5minute, plot(Time, Returns, type = "l"))
with(BAC_5minute, lines(Time, Returns, col = "red"))
with(Prices, lines(Time, Returns, col = "green"))

WFC_split = with(WFC_5minute, which(Returns == min(Returns)))
Prices_split = with(Prices, which(Returns == min(Returns)))

#Replace Split in Portfolio
Split_Time = Prices[Prices_split,"Time"]
j = 0
for(i in Price_tCopula){
  j = j + 1
  if(Split_Time %in% i[,"Time"]) break
}

P_t = (W[j,1]*2*WFC_5minute[WFC_split,"Price"] + W[j,2]*BAC_5minute[WFC_split,"Price"]) 
P_s = (W[j,1]*WFC_5minute[WFC_split-1,"Price"] + W[j,2]*BAC_5minute[WFC_split-1,"Price"]) 
Return_Split = (P_t/P_s) - 1
Logreturns_Split = log(P_t) - log(P_s)

Prices[Prices_split,"Returns"] = Return_Split
Prices[Prices_split,"Logreturns"] = Logreturns_Split
WFC_5minute[WFC_split,"Returns"] = ((2*WFC_5minute[WFC_split,"Price"])/(WFC_5minute[WFC_split-1,"Price"])) -1
WFC_5minute[WFC_split,"Logreturns"] = log(2*WFC_5minute[WFC_split,"Price"]) - log(WFC_5minute[WFC_split-1,"Price"])

# New_Plots ---------------------------------------------------------------

with(WFC_5minute, plot(Time, Logreturns, type = "l"))
with(BAC_5minute, lines(Time, Logreturns, col = "red"))
with(Prices, lines(Time, Logreturns, col = "green"))


with(WFC_5minute, sum(Logreturns))
with(BAC_5minute, sum(Logreturns))
with(Prices, sum(Logreturns))

with(WFC_5minute, plot(Time,cumsum(Logreturns), type = "l"))
with(BAC_5minute, lines(Time, cumsum(Logreturns), col = "red"))
with(Prices, lines(Time, cumsum(Logreturns), col = "green"))



# Calculate Yearly Returns ------------------------------------------------


