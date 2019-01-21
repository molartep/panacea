library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(bizdays)
B1_S_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Stephanie",
                                col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric"), skip = 2)

B1_S_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Stephanie",
                         col_types = c("date", "numeric", "numeric"))


B1_S_first_hours <- B1_S_hours[1:17, c(1,2,7)]
B1_S_second_hours <- B1_S_hours[19:30, c(1,2,7)]
B1_S_third_hours <- B1_S_hours[32:37, c(1,2,7)]

B1S_dates1 <- sum(B1_S_first_hours[,3] > 0)
B1S_dates2 <- sum(B1_S_second_hours[,3] > 0)
B1S_dates3 <- sum(B1_S_third_hours[,3] > 0)

B1_S_first_hours[,3] <- cumsum(B1_S_first_hours[,3])
B1_S_second_hours[,3] <- cumsum(B1_S_second_hours[,3])
B1_S_third_hours[,3] <- cumsum(B1_S_third_hours[,3])

maxP1 <- max(B1_S_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(B1_S_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(B1_S_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(B1_S_second_hours$`Polarity level`, na.rm = T)
maxP3 <- max(B1_S_third_hours$`Polarity level`, na.rm = T)
minP3 <- min(B1_S_third_hours$`Polarity level`, na.rm = T)

totalB1SHours <- c(max(B1_S_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(B1_S_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(B1_S_third_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA
)

totalB1SDays <- c(B1S_dates1, 
                  (B1_S_second_hours[B1_S_second_hours[,3] > 0, 1][1,1]-
                     tail(B1_S_first_hours[B1_S_first_hours[,3] > 0, 1], n = 1))$Day,
                  B1S_dates2,
                  (B1_S_third_hours[B1_S_third_hours[,3] > 0, 1][1,1]-
                     tail(B1_S_second_hours[B1_S_second_hours[,3] > 0, 1], n = 1))$Day,
                  B1S_dates3,
                  NA)

data.frame(Interval = c("1st session", "1st break", "2nd session", "2nd break", "3rd session", "3rd break"),
           Hours = totalB1SHours,
           Days = totalB1SDays,
           Polarity = c(maxP1,minP1,maxP2,minP2,maxP3,minP3),
           Change = c(minP1-maxP1, maxP2-minP1, minP2-maxP2, maxP3-minP2, minP3-maxP3, NA))

