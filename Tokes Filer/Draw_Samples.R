#Draw Random Samples
library(copula); library(parallel); library(GeneralizedHyperbolic)

load("Clean_Data_5minutes.RData")
load("Tokes_Fits.RData")
load("Copula_fit.RData")

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

#Which copulas failed?
Failed_Copulas = failed(Copula_fit)

#Get their Index
Index = which(Failed_Copulas == 1)

#Make corrections
Cadlag_Copula = Copula_fit
for(i in Index){
  Cadlag_Copula[[i]] = Cadlag_Copula[[i-1]]
}

n = length(Cadlag_Copula)

#1: WFC, 2: BAC
Draw_Samples = function(i,Window,Refit,Data_1,Data_2,Fit_1,Fit_2,Fit_Copula,N = 10000, tolerance = 1e-16, max_tol = 1e-3){
  library(GeneralizedHyperbolic)
  library(copula)
  
  browser()
  name_1 = unlist(strsplit(deparse(substitute(Data_1)),"_"))[1]
  name_2 = unlist(strsplit(deparse(substitute(Data_2)),"_"))[1]
  
  tol_1 = tol_2 = tolerance
  
  Param_1 = Fit_1[[i]][["param"]]
  Param_2 = Fit_2[[i]][["param"]]
  
  Copula = Fit_Copula[[i]]@copula
  
  Distribution = mvdc(Copula, margins = c("unif","unif"),paramMargins = list(list("min" = 0, max = 1),list("min" = 0, max = 1)))
  
  U = rMvdc(N,Distribution)
  
  NIG_1 = try(qnig(U[,1],param = Param_1,intTol = tol_1, uniTol = tol_1),silent = T)
  
  while("try-error" %in% class(NIG_1)){ 
    tol_1 = tol_1*10
    if(tol_1 > max_tol) return(paste0("Tolerance larger than ",max_tol))
    NIG_1 = try(qnig(U[,1],param = Param_1,intTol = tol_1, uniTol = tol_1),silent = T)
  }
  
  NIG_2 = try(qnig(U[,2],param = Param_2, intTol = tol_2, uniTol = tol_2),silent = T)
  
  while("try-error" %in% class(NIG_2)){ 
    tol_2 = tol_2*10
    if(tol_2 > max_tol) return(paste0("Tolerance larger than ",max_tol))
    NIG_2 = try(qnig(U[,2],param = Param_2, intTol = tol_2,uniTol = tol_2),silent = T)
  }
  
  NIG = cbind(NIG_1,NIG_2)
  colnames(NIG) = c(name_1,name_2)
  
  return(NIG)
}

cl = makePSOCKcluster(36)

Draws_tCopula = parLapply(cl, 1:n, Draw_Samples, 
          Window = Window, 
          Refit = Refit, 
          Data_1 = WFC_5minute, 
          Data_2 = BAC_5minute, 
          Fit_1 = WFC_fits, 
          Fit_2 = BAC_fits, 
          Fit_Copula = Cadlag_Copula)

save(Draws_tCopula, file = "Draws_tCopula.RData")


