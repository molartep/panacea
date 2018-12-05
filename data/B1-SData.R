library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
B1_S_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", 
                                col_types = c("text", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric"), skip = 2)

B1_S_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", 
                         col_types = c("date", "numeric", "numeric"))


B1_S_first_hours <- B1_S[1:17, c(2,7)]
B1_S_second_hours <- B1_S[19:30, c(2,7)]
B1_S_third_hours <- B1_S[32:37, c(2,7)]
B1_S_first_hours[,2] <- cumsum(B1_S_first_hours[,2])
B1_S_second_hours[,2] <- cumsum(B1_S_second_hours[,2])
B1_S_third_hours[,2] <- cumsum(B1_S_third_hours[,2])

maxP1 <- max(B1_S_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(B1_S_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(B1_S_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(B1_S_second_hours$`Polarity level`, na.rm = T)
maxP3 <- max(B1_S_third_hours$`Polarity level`, na.rm = T)
minP3 <- min(B1_S_third_hours$`Polarity level`, na.rm = T)

totalB1SHours <- c(max(B1_S_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(B1_S_second_hours$`Polarity level`, na.rm = T),
                   max(B1_S_third_hours$`Polarity level`, na.rm = T))

data.frame(Session = c("1st", "2nd", "3rd"), Hours = totalB1SHours,
           Max_Polarity = c(maxP1,maxP2,maxP3), Min_Polarity = c(minP1,minP2,minP3),
           Change = c(maxP1-minP1, maxP2-minP2, maxP3-minP3))

