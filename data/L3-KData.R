library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(bizdays)
L3_K_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Kevin",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

L3_K_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Kevin",
                         col_types = c("date", "numeric", "numeric"))


L3_K_first_hours <- L3_K_hours[1:7, c(1,2,6)]
L3_K_second_hours <- L3_K_hours[9:20, c(1,2,6)]
L3_K_third_hours <- L3_K_hours[22:28, c(1,2,6)]

L3K_dates1 <- sum(L3_K_first_hours[,3] > 0)
L3K_dates2 <- sum(L3_K_second_hours[,3] > 0)
L3K_dates3 <- sum(L3_K_third_hours[,3] > 0)

L3_K_first_hours[,3] <- cumsum(L3_K_first_hours[,3])
L3_K_second_hours[,3] <- cumsum(L3_K_second_hours[,3])
L3_K_third_hours[,3] <- cumsum(L3_K_third_hours[,3])

maxP1 <- max(L3_K_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(L3_K_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(L3_K_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(L3_K_second_hours$`Polarity level`, na.rm = T)
maxP3 <- max(L3_K_third_hours$`Polarity level`, na.rm = T)
minP3 <- min(L3_K_third_hours$`Polarity level`, na.rm = T)

totalL3KHours <- c(max(L3_K_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(L3_K_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(L3_K_third_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA
                   )

totalL3KDays <- c(L3K_dates1, 
                  (L3_K_second_hours[L3_K_second_hours[,3] > 0, 1][1,1]-
                     tail(L3_K_first_hours[L3_K_first_hours[,3] > 0, 1], n = 1))$Day,
                  L3K_dates2,
                  (L3_K_third_hours[L3_K_third_hours[,3] > 0, 1][1,1]-
                     tail(L3_K_second_hours[L3_K_second_hours[,3] > 0, 1], n = 1))$Day,
                  L3K_dates3,
                  NA)

data.frame(Interval = c("1st session", "1st break", "2nd session", "2nd break", "3rd session", "3rd break"),
           Hours = totalL3KHours,
           Days = totalL3KDays,
           Polarity = c(maxP1,minP1,maxP2,minP2,maxP3,minP3),
           Change = c(minP1-maxP1, maxP2-minP1, minP2-maxP2, maxP3-minP2, minP3-maxP3, NA))

