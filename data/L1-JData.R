library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(bizdays)
L1_J_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Jeff",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

L1_J_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Jeff",
                         col_types = c("date", "numeric", "numeric"))


L1_J_first_hours <- L1_J_hours[1:20, c(1,2,6)]

L1J_dates1 <- sum(L1_J_first_hours[,3] > 0)

L1_J_first_hours[,3] <- cumsum(L1_J_first_hours[,3])

maxP1 <- max(L1_J_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(L1_J_first_hours$`Polarity level`, na.rm = T)

totalL1JHours <- c(max(L1_J_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA
                   
)

totalL1JDays <- c(L1J_dates1, 
                  NA)

data.frame(Interval = c("1st session", "1st break"),
           Hours = totalL1JHours,
           Days = totalL1JDays,
           Polarity = c(maxP1,minP1),
           Change = c(minP1-maxP1, NA))

