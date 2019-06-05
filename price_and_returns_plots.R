setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#pacman::p_load(dbplyr,RSQLite,DBI,tidyverse,GeneralizedHyperbolic,tictoc,lubridate)
library(tidyverse) ; library(GeneralizedHyperbolic); library(ggpubr)
# 
# load("../WFC.5min.Rdata")
# load("../BAC.5min.data.Rdata")
# 
# 
# return.df <- function(df){
#   return <- diff(log(df$price))
#   return <- data.frame(cbind(df$utcsec[2:length(df$utcsec)],return))
#   colnames(return) <- c("Time","Return")
#   return$Return <- as.numeric(levels(return$Return))[return$Return]
#   return(return)
# }
# WFC.returns.5min <- return.df(WFC.5min)
# BAC.5min.returns.5min <- return.df(BAC.5min)

# load("../WFC.returns.5min.Rdata")
# load("../BAC.5min.returns.5min.Rdata")

load("../P8/Rdata/Clean_Data_5minutes.rdata")

# 


# WFC.returns.5min$Return[157966] <- log(2*WFC.5min$price[157967])-log(WFC.5min$price[157966]) %>% as.numeric()


# SPlit sker mellem 157966 og 157967


#install.packages("ggpubr")
library(ggpubr)

p1 <- ggplot(WFC_5minute) + geom_line(aes(y = Returns , x = Time, colour = "Returns",group = 1)) +
  scale_color_manual(values = c("black")) + 
  theme(legend.position = "none",legend.title = element_blank() )+
  xlab("Time")+ ylab("Returns")+ ggtitle("Wells Fargo Stock Returns")


p2 <- ggplot() + geom_line(aes(y = WFC_5minute$Price[1:31917] , x = WFC_5minute$Time[1:31917], 
                               colour = "Price",group = 1)) +
  geom_line(aes(y = WFC_5minute$Price[31918:99303] , 
                x = WFC_5minute$Time[31918:99303], 
                colour = "Price",group = 1)) + ylim(2, max(WFC_5minute$Price)+1) +
  scale_color_manual(values = c("black","black")) + 
  theme(legend.position = "none",legend.title = element_blank() )+
  xlab("Time")+ ylab("Price") + ggtitle("Wells Fargo Stock Prices")


ggarrange(p2 + rremove("x.text") + xlab(NULL) + 
            ggtitle("Wells Fargo stock prices and returns"), p1 + ggtitle(NULL), 
          labels = NULL, align = "v",
          ncol = 1, nrow = 2)

p3 <- ggplot(BAC_5minute) + geom_line(aes(y = Returns , x = Time, colour = "Returns",group = 1)) +
  scale_color_manual(values = c("black")) + 
  theme(legend.position = "none",legend.title = element_blank() )+
  xlab("Time")+ ylab("Returns")+ ggtitle("Bank of America Stock Returns")


p4 <- ggplot(BAC_5minute) + geom_line(aes(y = Price , x = Time, colour = "Price",group = 1)) +
  scale_color_manual(values = c("black")) + 
  theme(legend.position = "none",legend.title = element_blank() ) +
  xlab("Time")+ ylab("Price") + ggtitle("Bank of America Stock Prices") 


ggarrange(p4 + rremove("x.text") + xlab(NULL)+ 
            ggtitle("Bank of America stock prices and returns"), p3+ ggtitle(NULL), 
          labels = NULL, align = "v",
          ncol = 1, nrow = 2)



split <- which.max(AIG_5minute[["Returns"]])

p6 <- ggplot() + geom_line(aes(y = AIG_5minute$Price[1:(split-1)] , x = AIG_5minute$Time[1:(split-1)], 
                               colour = "Price", group = 1)) +
  geom_line(aes(y = AIG_5minute$Price[split:99303] , 
                x = AIG_5minute$Time[split:99303], 
                colour = "Price",group = 1)) +
  scale_color_manual(values = c("black","black")) + 
  theme(legend.position = "none",legend.title = element_blank() )+
  xlab("Time")+ ylab("Price")+ ggtitle("American International Group Stock Returns")


AIG_5minute[["Returns"]][split] <- log(AIG_5minute[["Price"]][split])- log(20*AIG_5minute[["Price"]][split-1])

p5 <- ggplot(AIG_5minute) + geom_line(aes(y = Returns , x = Time, colour = "Returns", group = 1)) +
  scale_color_manual(values = c("black")) + 
  theme(legend.position = "none",legend.title = element_blank() ) +
  xlab("Time")+ ylab("Returns") + ggtitle("American International Group Stock Prices") 


ggarrange(p6 + rremove("x.text") + xlab(NULL)+ 
            ggtitle("American International Group stock prices and returns"), p5 + ggtitle(NULL), 
          labels = NULL, align = "v",
          ncol = 1, nrow = 2)

