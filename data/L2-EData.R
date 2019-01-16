library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(bizdays)
L2_E_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Eric",
                         col_types = c("text", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "numeric"), skip = 2)

L2_E_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Eric",
                         col_types = c("date", "numeric", "numeric"))


L2_E_first_hours <- L2_E_hours[1:11, c(2,7)]
L2_E_second_hours <- L2_E_hours[13:25, c(2,7)]
L2_E_third_hours <- L2_E_hours[27:39, c(2,7)]
L2_E_fourth_hours <- L2_E_hours[41:52, c(2,7)]
L2_E_fifth_hours <- L2_E_hours[54:65, c(2,7)]

L2_E_first_hours[,2] <- cumsum(L2_E_first_hours[,2])
L2_E_second_hours[,2] <- cumsum(L2_E_second_hours[,2])
L2_E_third_hours[,2] <- cumsum(L2_E_third_hours[,2])
L2_E_fourth_hours[,2] <- cumsum(L2_E_fourth_hours[,2])
L2_E_fifth_hours[,2] <- cumsum(L2_E_fifth_hours[,2])

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

holidays <- c("2017-12-08", "2018-12-08", "2019-12-08")
create.calendar("Default", holidays, weekdays = c("sunday"), adjust.from = adjust.next, adjust.to = adjust.previous)
bizdays.options$set(default.calendar="Default")
L2E_dates1 <- bizdays(L2_E_dates[which(L2_E_dates$p_level == maxP1),1]$date,
                      L2_E_dates[which(L2_E_dates$p_level == minP1),1]$date)
L2E_dates2 <- bizdays(L2_E_dates[which(L2_E_dates$p_level == maxP2),1]$date,
                      L2_E_dates[which(L2_E_dates$p_level == minP2),1]$date)
L2E_dates3 <- bizdays(L2_E_dates[which(L2_E_dates$p_level == maxP3),1]$date,
                      L2_E_dates[which(L2_E_dates$p_level == minP3),1][1,1]$date)
L2E_dates4 <- bizdays(L2_E_dates[which(L2_E_dates$p_level == maxP4),1]$date,
                      L2_E_dates[which(L2_E_dates$p_level == minP4),1]$date)
L2E_dates5 <- bizdays(L2_E_dates[which(L2_E_dates$p_level == maxP5),1]$date,
                      L2_E_dates[which(L2_E_dates$p_level == minP5),1]$date)

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

totalL2EDays <- c(L2E_dates1 - 1,
                  (L2_E_dates[which(L2_E_dates$p_level == maxP2),1] -
                     L2_E_dates[which(L2_E_dates$p_level == minP1),1])$date,
                  L2E_dates2 + 1,
                  (L2_E_dates[which(L2_E_dates$p_level == maxP3),1] -
                     L2_E_dates[which(L2_E_dates$p_level == minP2),1])$date,
                  L2E_dates3 + 1,
                  (L2_E_dates[which(L2_E_dates$p_level == maxP4),1] -
                     L2_E_dates[which(L2_E_dates$p_level == minP3),1][1,1])$date,
                  L2E_dates4 + 4,
                  (L2_E_dates[which(L2_E_dates$p_level == maxP5),1] -
                     L2_E_dates[which(L2_E_dates$p_level == minP4),1])$date,
                  L2E_dates5 + 1,
                  NA)

#Final addition in order to correct session limits

data.frame(Interval = c("1st session", "1st break", "2nd session", "2nd break", "3rd session", "3rd break",
                        "4th session", "4th break", "5th session", "5th break"),
           Hours = totalL2EHours,
           Days = totalL2EDays,
           Polarity = c(maxP1,minP1,maxP2,minP2,maxP3,minP3,maxP4,minP4,maxP5,minP5),
           Change = c(minP1-maxP1, maxP2-minP1, minP2-maxP2, maxP3-minP2, minP3-maxP3, maxP4-minP3, minP4-maxP4, maxP5-minP4, minP5-maxP5, NA))

