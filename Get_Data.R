library(GeneralizedHyperbolic)
path = "./Databases"

getdbdata = function(series,path = ""){
  library(dbplyr);library(RSQLite);library(DBI);library(tidyverse)
  con = dbConnect(RSQLite::SQLite(), dbname = paste0(path,"/",series,".db"))
  result = tbl(con, series) %>% collect() %>% unique()
  dbDisconnect(con)
  return(result)
}
returns = function(df){
  library(magrittr)
  df %<>% mutate(returns = c(0,price %>% log %>% diff))
  return(df)
}


AIG = getdbdata("AIG",path)
BAC = getdbdata("BAC",path)
WFC <- getdbdata("WFC",path)

library(tidyverse);library(magrittr)


AIG %<>% returns
BAC %<>% returns



#Full Sample: 
AIG_fit = AIG %>% pull(returns) %>% .[-1] %>% nigFit(startValues = "MoM")
BAC_fit = BAC %>% pull(returns) %>% .[-1] %>% nigFit(startValues = "MoM")

x = seq(from = -0.01, to = 0.01, length.out = 1000)
y_aig = sapply(x,function(x){dnig(x,param = AIG_fit$param)})
y_bac = sapply(x,function(x){dnig(x,param = BAC_fit$param)})

hist(AIG$returns,breaks = 10000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_aig, col = "red")

hist(BAC$returns,breaks = 1000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_bac, col = "red")




idx = 1:3000

AIG_sample = AIG$returns[idx]
BAC_sample = BAC$returns[idx]

plot(AIG_sample,BAC_sample)

U = pnig(AIG_sample,param = AIG_fit$param)
V = pnig(BAC_sample,param = BAC_fit$param)

plot(U,V)

library(copula)
fit_Cop = tCopula(dim = 2)

fitted = fitCopula(fit_Cop,cbind(U,V))
Copula = tCopula(param= fitted@copula@parameters[1])

Construct = mvdc(copula = Copula, margins = c("unif","unif"), paramMargins = list(list(min = 0, max = 1),
                                                                                              list(min = 0, max = 1)))


Uniform = rMvdc(10000,Construct)


library(parallel)
detectCores()
cl = makePSOCKcluster(4)

{tictoc::tic()
  NIG = parApply(cl,Uniform,1,FUN = function(x){
    library(GeneralizedHyperbolic)
    param = c(0, 0.01, 50,3)
    pnig_inverse = function(u, lower = -1, upper = 1, ...){
      f = function(x){(pnig(x, ...)-u)^2}
      o = optim(0,f,method = "Brent", lower = lower, upper = upper)
      return(o$par)
    }
    
    param_1 = c(-2.233446e-06, 2.611934e-04, 1.758572e+00,  1.757023e+00)
    param_2 = c(-2.342621e-06,2.829926e-04,5.170427e-01,5.168077e-01)
    
    x_1 = pnig_inverse(x[1],lower=-0.2, upper =0.2, param = param_1)
    x_2 = pnig_inverse(x[2],lower=-0.2, upper =0.2, param = param_2)
    return(c(x_1,x_2))})
  tictoc::toc()}

plot(AIG_sample,BAC_sample)
points(t(NIG), col = "red")
NIG =t(NIG)

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

nigFit(NIG[,1])
nigFit(NIG[,2])
param_1
param_2
beta_1 = seq(from = -6, to = 6, length.out = 10000)
beta_2 = 1-beta_1
beta = cbind(beta_1, beta_2)

CVaR_beta = apply(beta,1,function(x){y = NIG %*% x; CVaR(y,.95)})
plot(CVaR_beta)
beta_star = beta[which(CVaR_beta == min(CVaR_beta)),]
mean(NIG %*% beta[9000,])
mean(cbind(AIG_sample,BAC_sample) %*% beta_star)
mean(cbind(AIG_sample,BAC_sample) %*% beta[9000,])

