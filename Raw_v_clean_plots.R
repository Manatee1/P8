
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(dbplyr,RSQLite,DBI,tidyverse,chron,lubridate)
#DB <- dbConnect(RSQLite::SQLite(), dbname = "../DataBases/WFC.db")

#list <- list.files("D:/P8Data/WFC/")
# data1 <- read.csv(paste0("WFC_20081014.csv"),stringsAsFactors = F)
# save(data1,file = "WFC20081014.rdata")
load("../P8/Rdata/Clean_Data_5minutes.Rdata")
load("WFC20081014.rdata")
data1 <- data1[,c(1,3)]
#collect <- tbl(DB, "WFC") %>%  collect() %>% arrange(utcsec)
data2 <- WFC_5minute[which(WFC_5minute$Time == ymd_hms("2008-10-14 09:30:00")):which(WFC_5minute$Time == ymd_hms("2008-10-14 16:00:00")),]
data2 <- data2[,1:2]
data1$Time <- lubridate::ymd_hms(paste0("2008-10-14 ",data1$utcsec))
data1 <- data.frame(ymd_hms(data1$Time),data1$price)
data1 <- data1 %>% cbind(rep("raw",length(data1[,1])))
colnames(data1) <- c(colnames(data2),"type")
data2 <- data2[,1:2] %>% cbind(rep("clean",length(data2[,1])))
colnames(data2) <- colnames(data1)
data <- rbind(data1,data2)

#dbDisconnect(DB)

ggplot(data,aes(x = Time,y = Price)) + ggtitle("Wells Fargo: Clean vs raw data") +
  geom_line(aes(colour = type),size = 0.1) +
  #geom_line(aes(y =  data2$price , x = data2$utcsec, colour = "Cleaned")) +
  scale_color_manual(values = c("red", "black")) + 
  #scale_linetype_manual(values = c("solid","dashed")) +
  theme(legend.title = element_blank() )+
  xlab("Time")+ ylab("Price")


interval <- lubridate::interval(start = "2008-10-14 10:00:00",end = "2008-10-14 12:00:00")
slice1 <- data1[which(data1$Time %within% interval),]
slice2 <- data2[which(data2$Time %within% interval),]
slice <- rbind(slice1,slice2)

ggplot(slice,aes(x = Time,y = Price)) + ggtitle("Wells Fargo: Clean vs raw data") +
  geom_line(aes(colour = type),size = 0.1) +
  #geom_line(aes(y =  data2$price , x = data2$utcsec, colour = "Cleaned")) +
  scale_color_manual(values = c("red", "black")) + 
  #scale_linetype_manual(values = c("solid","dashed")) +
  theme(legend.title = element_blank() )+
  xlab("Time")+ ylab("Price")



# ggplot() + geom_line(aes(y = slice1$price , x = slice1$utcsec, colour = "Uncleaned")) +
#   geom_line(aes(y =  slice2$price , x = slice2$utcsec, colour = "Cleaned")) +
#   scale_color_manual(values = c("black", "red")) + theme(legend.title = element_blank() )+
#   xlab("Time")+ ylab("Price")
  





