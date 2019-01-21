library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(bizdays)
G1_D_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Daryl",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

G1_D_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Daryl",
                         col_types = c("date", "numeric", "numeric"))


G1_D_first_hours <- G1_D_hours[1:4, c(1,2,6)]
G1_D_second_hours <- G1_D_hours[6:9, c(1,2,6)]
G1_D_third_hours <- G1_D_hours[11:14, c(1,2,6)]
G1_D_fourth_hours <- G1_D_hours[16:19, c(1,2,6)]

G1D_dates1 <- sum(G1_D_first_hours[,3] > 0)
G1D_dates2 <- sum(G1_D_second_hours[,3] > 0)
G1D_dates3 <- sum(G1_D_third_hours[,3] > 0)
G1D_dates4 <- sum(G1_D_fourth_hours[,3] > 0)

G1_D_first_hours[,3] <- cumsum(G1_D_first_hours[,3])
G1_D_second_hours[,3] <- cumsum(G1_D_second_hours[,3])
G1_D_third_hours[,3] <- cumsum(G1_D_third_hours[,3])
G1_D_fourth_hours[,3] <- cumsum(G1_D_fourth_hours[,3])

maxP1 <- max(G1_D_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(G1_D_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(G1_D_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(G1_D_second_hours$`Polarity level`, na.rm = T)
maxP3 <- max(G1_D_third_hours$`Polarity level`, na.rm = T)
minP3 <- min(G1_D_third_hours$`Polarity level`, na.rm = T)
maxP4 <- max(G1_D_fourth_hours$`Polarity level`, na.rm = T)
minP4 <- min(G1_D_fourth_hours$`Polarity level`, na.rm = T)

totalG1DHours <- c(max(G1_D_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(G1_D_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(G1_D_third_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(G1_D_fourth_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA)

totalG1DDays <- c(G1D_dates1, 
                  (G1_D_second_hours[G1_D_second_hours[,3] > 0, 1][1,1]-
                    tail(G1_D_first_hours[G1_D_first_hours[,3] > 0, 1], n = 1))$Day,
                  G1D_dates2,
                  (G1_D_third_hours[G1_D_third_hours[,3] > 0, 1][1,1]-
                    tail(G1_D_second_hours[G1_D_second_hours[,3] > 0, 1], n = 1))$Day,
                  G1D_dates3,
                  (G1_D_fourth_hours[G1_D_fourth_hours[,3] > 0, 1][1,1]-
                    tail(G1_D_third_hours[G1_D_third_hours[,3] > 0, 1], n = 1))$Day,
                  G1D_dates4,
                  NA)

data.frame(Interval = c("1st session", "1st break", "2nd session", "2nd break", "3rd session", "3rd break", "4th session", "4th break"),
           Hours = totalG1DHours,
           Days = totalG1DDays,
           Polarity = c(maxP1,minP1,maxP2,minP2,maxP3,minP3,maxP4,minP4),
           Change = c(minP1-maxP1, maxP2-minP1, minP2-maxP2, maxP3-minP2, minP3-maxP3, maxP4-minP3, minP4-maxP4, NA))

