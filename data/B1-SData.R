library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(bizdays)
B1_S_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Stephanie",
                                col_types = c("text", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric"), skip = 2)

B1_S_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Stephanie",
                         col_types = c("date", "numeric", "numeric"))


B1_S_first_hours <- B1_S_hours[1:17, c(2,7)]
B1_S_second_hours <- B1_S_hours[19:30, c(2,7)]
B1_S_third_hours <- B1_S_hours[32:37, c(2,7)]
B1_S_first_hours[,2] <- cumsum(B1_S_first_hours[,2])
B1_S_second_hours[,2] <- cumsum(B1_S_second_hours[,2])
B1_S_third_hours[,2] <- cumsum(B1_S_third_hours[,2])

maxP1 <- max(B1_S_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(B1_S_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(B1_S_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(B1_S_second_hours$`Polarity level`, na.rm = T)
maxP3 <- max(B1_S_third_hours$`Polarity level`, na.rm = T)
minP3 <- min(B1_S_third_hours$`Polarity level`, na.rm = T)

holidays <- c("2017-12-08", "2018-12-08", "2019-12-08")
create.calendar("Default", holidays, weekdays = c("sunday"), adjust.from = adjust.next, adjust.to = adjust.previous)
bizdays.options$set(default.calendar="Default")
B1S_dates1 <- bizdays(B1_S_dates[which(B1_S_dates$p_level == maxP1),1]$date,
                      B1_S_dates[which(B1_S_dates$p_level == minP1),1]$date)
B1S_dates2 <- bizdays(B1_S_dates[which(B1_S_dates$p_level == maxP2),1]$date,
                      B1_S_dates[which(B1_S_dates$p_level == minP2),1]$date)
B1S_dates3 <- bizdays(B1_S_dates[which(B1_S_dates$p_level == maxP3),1]$date,
                      B1_S_dates[which(B1_S_dates$p_level == minP3),1]$date)

totalB1SHours <- c(max(B1_S_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(B1_S_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(B1_S_third_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA)

totalB1SDays <- c(B1S_dates1 - 1, #No therapies on saturdays
                  (B1_S_dates[which(B1_S_dates$p_level == maxP2),1] -
                     B1_S_dates[which(B1_S_dates$p_level == minP1),1])$date,
                  B1S_dates2 + 1,
                  (B1_S_dates[which(B1_S_dates$p_level == maxP3),1] -
                     B1_S_dates[which(B1_S_dates$p_level == minP2),1])$date,
                  B1S_dates3 + 2, #Last day of 3rd session had no polarity value
                  NA)


data.frame(Interval = c("1st session", "1st break", "2nd session", "2nd break", "3rd session", "3rd break"),
           Hours = totalB1SHours,
           Days = totalB1SDays,
           Polarity = c(maxP1,minP1,maxP2,minP2,maxP3,minP3),
           Change = c(minP1-maxP1, maxP2-minP1, minP2-maxP2, maxP3-minP2, minP3-maxP3,NA))

