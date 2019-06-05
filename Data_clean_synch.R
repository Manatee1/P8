setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(dbplyr,RSQLite,DBI,tidyverse)


DB <- dbConnect(RSQLite::SQLite(), dbname = "./WFC.db")






time.index <- function(data){
  Index <- c()
  split <- str_split(as.character(data$utcsec),":")
  for(i in 1:nrow(data)){
    if(as.numeric(split[[i]][1])<9){ #Check if before 9am
      Index <- c(Index,i)
    }
    else if(as.numeric(split[[i]][1])==9 & 
            as.numeric(split[[i]][2]) < 30){ #Check if 9-9.30
      Index <- c(Index,i)
    }
    else{break} #Break forward loop if one obs later that 9.30
  }
  for(i in nrow(data):1){ #Start backward loop
    if(as.numeric(split[[i]][1])>16){ #Check if 5pm or later
      Index <- c(Index,i)
    }
    else if(as.numeric(split[[i]][1])==16 & #Check if #16.01-17
            as.numeric(split[[i]][2]) > 0){
      Index <- c(Index,i)
    }
    else if(as.numeric(split[[i]][1])==16 & #Check if 16.00-16.01
            as.numeric(split[[i]][2]) == 0 &
            as.numeric(split[[i]][3]) > 0){ 
      Index <- c(Index,i)
    }
    else{break} # Break if one obs earlier than 16.
  }
  return(Index)
}

synchronize1 <- function(data){
  timestamp <- data$utcsec
  split <- str_split(as.character(timestamp),":")
  for(i in 1:length(timestamp)){
    h <- split[[i]][1]
    m <- split[[i]][2]
    s <- as.numeric(split[[i]][3])
    
    if(s<30){
      data$utcsec[i] <- paste0(h,":",m,":00")
    }
    else if(s>=30){
      if(m == "59"){
        m = "00"
        h <- as.character(as.numeric(h)+1)
        data$utcsec[i] <- paste0(h,":",m,":00") 
      }
      else{
        if((as.numeric(m)+1)>=10){
          data$utcsec[i] <- paste0(h,":",as.numeric(m)+1,":00") 
        }
        else{
          data$utcsec[i] <- paste0(h,":0",as.numeric(m)+1,":00") 
        }
      }
    }
  }
  return(data)
}

synchronize2 <- function(data,close){
  #browser()
  
  #Check if there is a price at opening, else replace with yesterdays close.
  if(!(data$utcsec[1]=="09:30:00")){
    close$utcsec <- "09:30:00"
    data <- rbind(data,close)
  }
  # Iterate through times
  for(j in 9:16){
    for(i in 0:59){
      if(j == 9 & i<=30){next} #Skip 9.00-9.30
      if(j ==9){
        if(i<10){
          Index <- which(data$utcsec == paste0("0",j,":","0",i,":00"))
          if(length(Index) == 0){
            if((i == 0)){
              add <- data[data$utcsec == paste0("0",j-1,":","59",":00"),]
            }
            else{
              add <- data[data$utcsec == paste0("0",j,":",i-1,":00"),]
            }
            add$utcsec <- paste0("0",j,":0",i,":00")
            data <- rbind(data,add)
          }
        }
        else{
          Index <- which(data$utcsec == paste0("0",j,":",i,":00"))
          if(length(Index) == 0){
            if((i == 0)){
              add <- data[data$utcsec == paste0("0",j-1,":","59",":00"),]
            }
            else{
              if((i-1) < 10){
                add <- data[data$utcsec == paste0("0",j,":0",i-1,":00"),]
              }
              else{
                add <- data[data$utcsec == paste0("0",j,":",i-1,":00"),]
              }
            }
            add$utcsec <- paste0("0",j,":",i,":00")
            data <- rbind(data,add)
          }
        }
      }
      else{
        if(j==16 & i>0){
          break
        }
        if(i<10){
          Index <- which(data$utcsec == paste0(j,":","0",i,":00"))
          if(length(Index) == 0){
            if((i == 0)){
              if((j-1)<10){
                add <- data[data$utcsec == paste0("0",j-1,":59:00"),]
              }
              else{
                add <- data[data$utcsec == paste0(j-1,":59:00"),]
              }
            }
            else{
              add <- data[data$utcsec == paste0(j,":0",i-1,":00"),]
            }
            add$utcsec <- paste0(j,":0",i,":00")
            data <- rbind(data,add)
          }
        }
        else{
          Index <- which(data$utcsec == paste0(j,":",i,":00"))
          if(length(Index) == 0){
              if((i-1) < 10){
                add <- data[data$utcsec == paste0(j,":0",i-1,":00"),]
              }
              else{
                add <- data[data$utcsec == paste0(j,":",i-1,":00"),]
              }
            add$utcsec <- paste0(j,":",i,":00")
            data <- rbind(data,add)
            }
          }
        }
      }
      
  }
  
  return(data %>% arrange(utcsec))
}

fix.duplicates <- function(data){
  # Identify multiple observations at same time stamp
  n.occur <- data.frame(table(data$utcsec))
  mult.times <- n.occur[(n.occur$Freq > 0), 1]
  # Replace prices with median
  for(time in mult.times){
    Duplicate <- which(data$utcsec == time)
    #Duplicate <- rownames(data[data$utcsec == time,])
    data[Duplicate,]$price <- median(data[Duplicate,]$price)
  }
  # Remove duplicate observations
  return(data[!duplicated(data$utcsec), ])
}

clean <- function(list,stock = "WFC",dir,close,DB){
  # Loop through list of data files
  for(file in list){
    
    # Read data file
    data <- read.csv(paste0(dir,stock,"/",file),stringsAsFactors = F)
    
    # Remove data from other exchanges
    data <- data[(data$ex == "N"|data$ex == "T"), ]  
    if(length(data[,1])==0){
      print(paste0("No observations from the exchange ",best.ex))
      next
    }
    rownames(data) <- 1:nrow(data)
    
    
    # Identify observations outside trading hours 9.30-16.00
    Index <- time.index(data = data)
    # Remove data outside trading hours
    if(!(length(Index)==0)){
      data <- data[-Index, ]
      rownames(data) <- 1:nrow(data)
    }
    
    # Remove data with non-positive prices
    if(min(data$price) <= 0){
      Index <- which(data$price <= 0)
      if(!(length(Index)==0)){
        data <- data[-Index,] ; rownames(data) <- 1:nrow(data)
      }
    }
    # Remove corrected trades
    data <- data[(data$corr == 0), ] ; rownames(data) <- 1:nrow(data)
    
    # Remove trades with abnormal sales condition
    data$cond <- as.character(data$cond) %>% trimws("both")
    data <- data[(data$cond == ""|is.na(data$cond)|data$cond == "E"|
                    data$cond == "F"), ]
    rownames(data) <- 1:nrow(data)
    
    # Remove outliers
    Index <- c()
    for(i in 1:nrow(data)){
      if(i <= 25){
        id <- 1:(i+25)
        id <- id[-i]
        med <- median(data$price[id])
      }
      else if((nrow(data)-i) <= 25){
        id <- (i-25):nrow(data)
        id <- id[-i]
        med <- median(data$price[id])
      }
      else{
        id <- (i-25):(i+25)
        id <- id[-i]
        med <- median(data$price[id])
      }
      mad <- mean(abs(data$price[id]-med))
      if(data$price[i] > (med + 10*mad) | data$price[i] < (med - 10*mad)){
        Index <- c(Index,i)
        #cat("Observation related to ",data$utcsec[i]," has been removed","\n")
      }
    }
    
    if(!(length(Index)==0)){
      data <- data[-Index,] ; rownames(data) <- 1:nrow(data)
    }
    
    
    # Shifts observations to lie at minute intervals
    data <- synchronize1(data);rownames(data) <- 1:nrow(data)
    
    # Replace multiple observations with the same timestamp by one with the median price.
    data <- fix.duplicates(data = data)
    
    # Fill missing data points
    data <- synchronize2(data = data,close = close)
    rownames(data) <- 1:nrow(data)
    
    close <- tail(data,1)
    
    date <- str_split(file,"_")[[1]][2] %>% str_split(string = .,pattern = "\\.") %>% 
      .[[1]] %>% .[1] %>%
      as.Date(format = "%Y%m%d") %>% as.character() %>% rep(nrow(data))
    
    data <- data %>% mutate(utcsec = paste(date,utcsec))
    
    # Write new file for cleaned data.
    # directory <- paste0(dir,stock,"_cleaned","/",file)
    # write.csv(data,file = directory,sep = ",",row.names = F)
    
    # Write to database.
    dbWriteTable(DB,name = stock,value = data,append = T,overwrite = F)
    cat("File completed = ", file, "\n")
  }
}

Directory <- "D:/P8Data/"
stock <- "WFC"
list <- list.files(paste0(Directory,stock,"/"))


#best.ex <- which.exchange(list = list,stock = stock,exchanges = ex) # Best.ex = "T" for "WFC"

table <- read.csv(paste0("D:/P8Data/",stock,"/",list[979]),stringsAsFactors = F) %>% tail(1)

clean(list[980:1033],stock = stock,dir = Directory,close=tail(table,1),DB = DB)

#   File completed = WFC_20081120.csv

dbDisconnect(DB)







