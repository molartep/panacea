library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(bizdays)
C1_J_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Jim",
                         col_types = c("text", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "numeric", "numeric"), skip = 2)

C1_J_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Jim",
                         col_types = c("date", "numeric", "numeric"))


C1_J_first_hours <- C1_J_hours[1:18, c(2,8)]
C1_J_second_hours <- C1_J_hours[20:32, c(2,8)]
C1_J_third_hours <- C1_J_hours[34:45, c(2,8)]
C1_J_fourth_hours <- C1_J_hours[47:59, c(2,8)]
C1_J_first_hours[,2] <- cumsum(C1_J_first_hours[,2])
C1_J_second_hours[,2] <- cumsum(C1_J_second_hours[,2])
C1_J_third_hours[,2] <- cumsum(C1_J_third_hours[,2])
C1_J_fourth_hours[,2] <- cumsum(C1_J_fourth_hours[,2])

maxP1 <- max(C1_J_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(C1_J_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(C1_J_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(C1_J_second_hours$`Polarity level`, na.rm = T)
maxP3 <- max(C1_J_third_hours$`Polarity level`, na.rm = T)
minP3 <- min(C1_J_third_hours$`Polarity level`, na.rm = T)
maxP4 <- max(C1_J_fourth_hours$`Polarity level`, na.rm = T)
minP4 <- min(C1_J_fourth_hours$`Polarity level`, na.rm = T)

holidays <- c("2017-12-08", "2018-12-08", "2019-12-08")
create.calendar("Default", holidays, weekdays = c("sunday"), adjust.from = adjust.next, adjust.to = adjust.previous)
bizdays.options$set(default.calendar="Default")
C1J_dates1 <- bizdays(C1_J_dates[which(C1_J_dates$p_level == maxP1),1]$date,
                      C1_J_dates[which(C1_J_dates$p_level == minP1),1]$date)
C1J_dates2 <- bizdays(C1_J_dates[which(C1_J_dates$p_level == maxP2),1]$date,
                      C1_J_dates[which(C1_J_dates$p_level == minP2),1]$date)
C1J_dates3 <- bizdays(C1_J_dates[which(C1_J_dates$p_level == maxP3),1]$date,
                      C1_J_dates[which(C1_J_dates$p_level == minP3),1]$date)
C1J_dates4 <- bizdays(C1_J_dates[which(C1_J_dates$p_level == maxP4),1]$date,
                      C1_J_dates[which(C1_J_dates$p_level == minP4),1]$date)

totalC1JHours <- c(max(C1_J_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(C1_J_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(C1_J_third_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(C1_J_fourth_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA)

totalC1JDays <- c(C1J_dates1 + 1,
                  (C1_J_dates[which(C1_J_dates$p_level == maxP2),1] -
                     C1_J_dates[which(C1_J_dates$p_level == minP1),1])$date,
                  C1J_dates2 + 1,
                  (C1_J_dates[which(C1_J_dates$p_level == maxP3),1] -
                     C1_J_dates[which(C1_J_dates$p_level == minP2),1])$date,
                  C1J_dates3 + 1,
                  (C1_J_dates[which(C1_J_dates$p_level == maxP4),1] -
                     C1_J_dates[which(C1_J_dates$p_level == minP3),1])$date,
                  C1J_dates4 + 3, #First two days of 4th session had no polarity value
                  NA)


data.frame(Interval = c("1st session", "1st break", "2nd session", "2nd break", "3rd session", "3rd break",
                        "4th session", "4th break"),
           Hours = totalC1JHours,
           Days = totalC1JDays,
           Polarity = c(maxP1,minP1,maxP2,minP2,maxP3,minP3,maxP4,minP4),
           Change = c(minP1-maxP1, maxP2-minP1, minP2-maxP2, maxP3-minP2, minP3-maxP3, maxP4-minP3, minP4-maxP4, NA))

