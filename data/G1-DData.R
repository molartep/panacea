library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(bizdays)
G1_D_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Daryl",
                         col_types = c("text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

G1_D_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Daryl",
                         col_types = c("date", "numeric", "numeric"))


G1_D_first_hours <- G1_D_hours[1:4, c(2,6)]
G1_D_second_hours <- G1_D_hours[6:9, c(2,6)]
G1_D_third_hours <- G1_D_hours[11:14, c(2,6)]
G1_D_first_hours[,2] <- cumsum(G1_D_first_hours[,2])
G1_D_second_hours[,2] <- cumsum(G1_D_second_hours[,2])
G1_D_third_hours[,2] <- cumsum(G1_D_third_hours[,2])

maxP1 <- max(G1_D_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(G1_D_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(G1_D_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(G1_D_second_hours$`Polarity level`, na.rm = T)
#maxP3 <- max(G1_D_third_hours$`Polarity level`, na.rm = T)
#minP3 <- min(G1_D_third_hours$`Polarity level`, na.rm = T)

holidays <- c("2017-12-08", "2018-12-08", "2019-12-08")
create.calendar("Default", holidays, weekdays = c("sunday"), adjust.from = adjust.next, adjust.to = adjust.previous)
bizdays.options$set(default.calendar="Default")
G1D_dates1 <- bizdays(G1_D_dates[which(G1_D_dates$p_level == maxP1),1]$date,
                      G1_D_dates[which(G1_D_dates$p_level == minP1),1]$date[1])
G1D_dates2 <- bizdays(G1_D_dates[which(G1_D_dates$p_level == maxP2),1]$date,
                      G1_D_dates[which(G1_D_dates$p_level == minP2),1]$date)
#G1D_dates3 <- bizdays(G1_D_dates[which(G1_D_dates$p_level == maxP3),1]$date,
                      #G1_D_dates[which(G1_D_dates$p_level == minP3),1]$date)

totalG1DHours <- c(max(G1_D_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(G1_D_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   #NA,
                   #max(G1_D_third_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA)

totalG1DDays <- c(G1D_dates1 + 1, 
                  G1_D_dates[which(G1_D_dates$p_level == maxP2),1]$date -
                     G1_D_dates[which(G1_D_dates$p_level == minP1),1]$date[1],
                  G1D_dates2, #First polarity value was measured before treatment (Day 0)
                  #(G1_D_dates[which(G1_D_dates$p_level == maxP3),1] -
                  #   G1_D_dates[which(G1_D_dates$p_level == minP2),1])$date,
                  #G1D_dates3 + 1, 
                  NA)


data.frame(Interval = c("1st session", "1st break", "2nd session", "2nd break"), #"3rd session", "3rd break"),
           Hours = totalG1DHours,
           Days = totalG1DDays,
           Polarity = c(maxP1,minP1,maxP2,minP2), #,maxP3,minP3),
           Change = c(minP1-maxP1, maxP2-minP1, minP2-maxP2, NA)) #maxP3-minP2, minP3-maxP3,NA))

