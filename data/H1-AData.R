library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(bizdays)
H1_A_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Anna",
                         col_types = c("text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

H1_A_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Anna",
                         col_types = c("date", "numeric", "numeric"))


H1_A_first_hours <- H1_A_hours[1:9, c(2,6)]
H1_A_second_hours <- H1_A_hours[11:23, c(2,6)]
H1_A_third_hours <- H1_A_hours[25:26, c(2,6)]
H1_A_fourth_hours <- H1_A_hours[28:39, c(2,6)]
H1_A_first_hours[,2] <- cumsum(H1_A_first_hours[,2])
H1_A_second_hours[,2] <- cumsum(H1_A_second_hours[,2])
H1_A_third_hours[,2] <- cumsum(H1_A_third_hours[,2])
H1_A_fourth_hours[,2] <- cumsum(H1_A_fourth_hours[,2])

maxP1 <- max(H1_A_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(H1_A_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(H1_A_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(H1_A_second_hours$`Polarity level`, na.rm = T)
maxP3 <- max(H1_A_third_hours$`Polarity level`, na.rm = T)
minP3 <- min(H1_A_third_hours$`Polarity level`, na.rm = T)
#maxP4 <- max(H1_A_fourth_hours$`Polarity level`, na.rm = T)
#minP4 <- min(H1_A_fourth_hours$`Polarity level`, na.rm = T)

holidays <- c("2017-12-08", "2018-12-08", "2019-12-08")
create.calendar("Default", holidays, weekdays = c("sunday"), adjust.from = adjust.next, adjust.to = adjust.previous)
bizdays.options$set(default.calendar="Default")
H1A_dates1 <- bizdays(H1_A_dates[which(H1_A_dates$p_level == maxP1),1]$date,
                      H1_A_dates[which(H1_A_dates$p_level == minP1),1]$date[1])
H1A_dates2 <- bizdays(H1_A_dates[which(H1_A_dates$p_level == maxP2),1]$date,
                      H1_A_dates[which(H1_A_dates$p_level == minP2),1]$date)
H1A_dates3 <- bizdays(H1_A_dates[which(H1_A_dates$p_level == maxP3),1]$date,
                      H1_A_dates[which(H1_A_dates$p_level == minP3),1]$date)
#H1A_dates4 <- bizdays(H1_A_dates[which(H1_A_dates$p_level == maxP4),1]$date,
                      #H1_A_dates[which(H1_A_dates$p_level == minP4),1]$date)

totalH1AHours <- c(max(H1_A_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(H1_A_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA,
                   max(H1_A_third_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   #NA,
                   #max(H1_A_fourth_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   NA)

totalH1ADays <- c(H1A_dates1, #No therapies on saturdays 
                  H1_A_dates[which(H1_A_dates$p_level == maxP2),1]$date -
                    H1_A_dates[which(H1_A_dates$p_level == minP1),1]$date[1],
                  H1A_dates2 + 1,
                  (H1_A_dates[which(H1_A_dates$p_level == maxP3),1] -
                    H1_A_dates[which(H1_A_dates$p_level == minP2),1])$date,
                  H1A_dates3 + 1,
                  #(H1_A_dates[which(H1_A_dates$p_level == maxP4),1] -
                     #H1_A_dates[which(H1_A_dates$p_level == minP3),1])$date,
                  #H1A_dates4 + 1,
                  NA)


data.frame(Interval = c("1st session", "1st break", "2nd session", "2nd break", "3rd session", "3rd break"),
                        #"4th session", "4th break"),
           Hours = totalH1AHours,
           Days = totalH1ADays,
           Polarity = c(maxP1,minP1,maxP2,minP2,maxP3,minP3),#maxP4,minP4),
           Change = c(minP1-maxP1, maxP2-minP1, minP2-maxP2, maxP3-minP2, minP3-maxP3, NA))
                      #,maxP4-minP3, minP4-maxP4, NA))

