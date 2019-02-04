library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(bizdays)
C1_J_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Jim",
                         col_types = c("date", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "numeric", "numeric"), skip = 2)

C1_J_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Jim",
                         col_types = c("date", "numeric", "numeric"))


C1_J_first_hours <- C1_J_hours[1:18, c(1,2,8)]
C1_J_second_hours <- C1_J_hours[20:32, c(1,2,8)]
C1_J_third_hours <- C1_J_hours[34:45, c(1,2,8)]
C1_J_fourth_hours <- C1_J_hours[47:62, c(1,2,8)]
C1_J_fifth_hours <- C1_J_hours[64:79, c(1,2,8)]

C1J_dates1 <- sum(C1_J_first_hours[,3] > 0)
C1J_dates2 <- sum(C1_J_second_hours[,3] > 0)
C1J_dates3 <- sum(C1_J_third_hours[,3] > 0)
C1J_dates4 <- sum(C1_J_fourth_hours[,3] > 0)
C1J_dates5 <- sum(C1_J_fifth_hours[,3] > 0)

C1_J_first_hours[,3] <- cumsum(C1_J_first_hours[,3])
C1_J_second_hours[,3] <- cumsum(C1_J_second_hours[,3])
C1_J_third_hours[,3] <- cumsum(C1_J_third_hours[,3])
C1_J_fourth_hours[,3] <- cumsum(C1_J_fourth_hours[,3])
C1_J_fifth_hours[,3] <- cumsum(C1_J_fifth_hours[,3])

maxP1 <- max(C1_J_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(C1_J_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(C1_J_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(C1_J_second_hours$`Polarity level`, na.rm = T)
maxP3 <- max(C1_J_third_hours$`Polarity level`, na.rm = T)
minP3 <- min(C1_J_third_hours$`Polarity level`, na.rm = T)
maxP4 <- max(C1_J_fourth_hours$`Polarity level`, na.rm = T)
minP4 <- min(C1_J_fourth_hours$`Polarity level`, na.rm = T)
maxP5 <- max(C1_J_fifth_hours$`Polarity level`, na.rm = T)
minP5 <- min(C1_J_fifth_hours$`Polarity level`, na.rm = T)

totalC1JHours <- c(max(C1_J_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(C1_J_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(C1_J_third_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(C1_J_fourth_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA)

totalC1JDays <- c(C1J_dates1, 
                  (C1_J_second_hours[C1_J_second_hours[,3] > 0, 1][1,1]-
                     tail(C1_J_first_hours[C1_J_first_hours[,3] > 0, 1], n = 1))$Day,
                  C1J_dates2,
                  (C1_J_third_hours[C1_J_third_hours[,3] > 0, 1][1,1]-
                     tail(C1_J_second_hours[C1_J_second_hours[,3] > 0, 1], n = 1))$Day,
                  C1J_dates3,
                  (C1_J_fourth_hours[C1_J_fourth_hours[,3] > 0, 1][1,1]-
                     tail(C1_J_third_hours[C1_J_third_hours[,3] > 0, 1], n = 1))$Day,
                  C1J_dates4,
                  NA)

data.frame(Interval = c("1st session", "1st break", "2nd session", "2nd break", "3rd session", "3rd break", "4th session", "4th break"),
           Hours = totalC1JHours,
           Days = totalC1JDays,
           Polarity = c(maxP1,minP1,maxP2,minP2,maxP3,minP3,maxP4,minP4),
           Change = c(minP1-maxP1, maxP2-minP1, minP2-maxP2, maxP3-minP2, minP3-maxP3, maxP4-minP3, minP4-maxP4, NA))

