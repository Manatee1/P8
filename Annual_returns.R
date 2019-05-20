setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse)
load("Rdata/Clean_Data_5minutes.RData")



annual_return <- function(data,year){
  str <-  str_split(as.character(data[["Time"]]),"-"); n <- length(str)
  index <- lapply(str, function(x)if(as.character(year) %in% x[1]){return(1)}else{return(0)})
  
  New_data <- data[which(index == 1),]
  Return <- exp(sum(New_data[["Returns"]]))-1
  return(Return)
}
dat <- annual_return(BAC_5minute,2005)

returns <- matrix(0,nrow = 3,ncol = 5)
for(i in 2005:2009){
  returns[1,i-2004] <- annual_return(BAC_5minute,i)
  returns[2,i-2004] <- annual_return(WFC_5minute,i)
  returns[3,i-2004] <- annual_return(Markowitz_portfolio$PF_returns,i)
}



