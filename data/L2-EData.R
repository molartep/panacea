library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(bizdays)
L2_E_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Eric",
                         col_types = c("date", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "numeric"), skip = 2)

L2_E_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Eric",
                         col_types = c("date", "numeric", "numeric"))


L2_E_first_hours <- L2_E_hours[1:11, c(1,2,7)]
L2_E_second_hours <- L2_E_hours[13:25, c(1,2,7)]
L2_E_third_hours <- L2_E_hours[27:39, c(1,2,7)]
L2_E_fourth_hours <- L2_E_hours[41:52, c(1,2,7)]
L2_E_fifth_hours <- L2_E_hours[54:65, c(1,2,7)]

L2E_dates1 <- sum(L2_E_first_hours[,3] > 0)
L2E_dates2 <- sum(L2_E_second_hours[,3] > 0)
L2E_dates3 <- sum(L2_E_third_hours[,3] > 0)
L2E_dates4 <- sum(L2_E_fourth_hours[,3] > 0)
L2E_dates5 <- sum(L2_E_fifth_hours[,3] > 0)

L2_E_first_hours[,3] <- cumsum(L2_E_first_hours[,3])
L2_E_second_hours[,3] <- cumsum(L2_E_second_hours[,3])
L2_E_third_hours[,3] <- cumsum(L2_E_third_hours[,3])
L2_E_fourth_hours[,3] <- cumsum(L2_E_fourth_hours[,3])
L2_E_fifth_hours[,3] <- cumsum(L2_E_fifth_hours[,3])

maxP1 <- max(L2_E_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(L2_E_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(L2_E_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(L2_E_second_hours$`Polarity level`, na.rm = T)
maxP3 <- max(L2_E_third_hours$`Polarity level`, na.rm = T)
minP3 <- min(L2_E_third_hours$`Polarity level`, na.rm = T)
maxP4 <- max(L2_E_fourth_hours$`Polarity level`, na.rm = T)
minP4 <- min(L2_E_fourth_hours$`Polarity level`, na.rm = T)
maxP5 <- max(L2_E_fifth_hours$`Polarity level`, na.rm = T)
minP5 <- min(L2_E_fifth_hours$`Polarity level`, na.rm = T)

totalL2EHours <- c(max(L2_E_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(L2_E_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(L2_E_third_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(L2_E_fourth_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(L2_E_fifth_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA)

totalL2EDays <- c(L2E_dates1, 
                  (L2_E_second_hours[L2_E_second_hours[,3] > 0, 1][1,1]-
                     tail(L2_E_first_hours[L2_E_first_hours[,3] > 0, 1], n = 1))$Day,
                  L2E_dates2,
                  (L2_E_third_hours[L2_E_third_hours[,3] > 0, 1][1,1]-
                     tail(L2_E_second_hours[L2_E_second_hours[,3] > 0, 1], n = 1))$Day,
                  L2E_dates3,
                  (L2_E_fourth_hours[L2_E_fourth_hours[,3] > 0, 1][1,1]-
                     tail(L2_E_third_hours[L2_E_third_hours[,3] > 0, 1], n = 1))$Day,
                  L2E_dates4,
                  (L2_E_fifth_hours[L2_E_fifth_hours[,3] > 0, 1][1,1]-
                     tail(L2_E_fourth_hours[L2_E_fourth_hours[,3] > 0, 1], n = 1))$Day,
                  L2E_dates5,
                  NA)

data.frame(Interval = c("1st session", "1st break", "2nd session", "2nd break", "3rd session", "3rd break",
                        "4th session", "4th break", "5th session", "5th break"),
           Hours = totalL2EHours,
           Days = totalL2EDays,
           Polarity = c(maxP1,minP1,maxP2,minP2,maxP3,minP3,maxP4,minP4,maxP5,minP5),
           Change = c(minP1-maxP1, maxP2-minP1, minP2-maxP2, maxP3-minP2, minP3-maxP3, maxP4-minP3, minP4-maxP4, maxP5-minP4, minP5-maxP5, NA))

