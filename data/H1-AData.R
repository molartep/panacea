library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
H1_A_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Anna",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

H1_A_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Anna",
                         col_types = c("date", "numeric", "numeric"))


H1_A_first_hours <- H1_A_hours[1:9, c(1,2,6)]
H1_A_second_hours <- H1_A_hours[11:23, c(1,2,6)]
H1_A_third_hours <- H1_A_hours[25:26, c(1,2,6)]
H1_A_fourth_hours <- H1_A_hours[28:39, c(1,2,6)]
H1_A_fifth_hours <- H1_A_hours[41:52, c(1,2,6)]
H1_A_sixth_hours <- H1_A_hours[54:58, c(1,2,6)]

start1 <- H1_A_first_hours[H1_A_first_hours[,3] > 0, 1][1,1]
end1 <- tail(H1_A_first_hours[H1_A_first_hours[,3] > 0, 1], n = 1)
start2 <- H1_A_second_hours[H1_A_second_hours[,3] > 0, 1][1,1]
end2 <- tail(H1_A_second_hours[H1_A_second_hours[,3] > 0, 1], n = 1)
start3 <- H1_A_third_hours[H1_A_third_hours[,3] > 0, 1][1,1]
end3 <- tail(H1_A_third_hours[H1_A_third_hours[,3] > 0, 1], n = 1)
start4 <- H1_A_fourth_hours[H1_A_fourth_hours[,3] > 0, 1][1,1]
end4 <- tail(H1_A_fourth_hours[H1_A_fourth_hours[,3] > 0, 1], n = 1)
start5 <- H1_A_fifth_hours[H1_A_fifth_hours[,3] > 0, 1][1,1]
end5 <- tail(H1_A_fifth_hours[H1_A_fifth_hours[,3] > 0, 1], n = 1)
start6 <- H1_A_sixth_hours[H1_A_sixth_hours[,3] > 0, 1][1,1]
end6 <- tail(H1_A_sixth_hours[H1_A_sixth_hours[,3] > 0, 1], n = 1)

H1A_dates1 <- sum(H1_A_first_hours[,3] > 0)
H1A_dates2 <- sum(H1_A_second_hours[,3] > 0)
H1A_dates3 <- sum(H1_A_third_hours[,3] > 0)
H1A_dates4 <- sum(H1_A_fourth_hours[,3] > 0)
H1A_dates5 <- sum(H1_A_fifth_hours[,3] > 0)
H1A_dates6 <- sum(H1_A_sixth_hours[,3] > 0)

H1_A_first_hours[,3] <- cumsum(H1_A_first_hours[,3])
H1_A_second_hours[,3] <- cumsum(H1_A_second_hours[,3])
H1_A_third_hours[,3] <- cumsum(H1_A_third_hours[,3])
H1_A_fourth_hours[,3] <- cumsum(H1_A_fourth_hours[,3])
H1_A_fifth_hours[,3] <- cumsum(H1_A_fifth_hours[,3])
H1_A_sixth_hours[,3] <- cumsum(H1_A_sixth_hours[,3])

maxP1 <- max(H1_A_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(H1_A_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(H1_A_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(H1_A_second_hours$`Polarity level`, na.rm = T)
maxP3 <- max(H1_A_third_hours$`Polarity level`, na.rm = T)
minP3 <- min(H1_A_third_hours$`Polarity level`, na.rm = T)
maxP4 <- max(H1_A_fourth_hours$`Polarity level`, na.rm = T)
minP4 <- min(H1_A_fourth_hours$`Polarity level`, na.rm = T)
maxP5 <- max(H1_A_fifth_hours$`Polarity level`, na.rm = T)
minP5 <- min(H1_A_fifth_hours$`Polarity level`, na.rm = T)
maxP6 <- max(H1_A_sixth_hours$`Polarity level`, na.rm = T)
minP6 <- min(H1_A_sixth_hours$`Polarity level`, na.rm = T)

totalH1AHours <- c(max(H1_A_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(H1_A_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(H1_A_third_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(H1_A_fourth_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(H1_A_fifth_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(H1_A_sixth_hours$`Total therapy duration (Hrs)`, na.rm = T))

totalH1ADays <- c(H1A_dates1, 
                  (start2 - end1)$Day,
                  H1A_dates2,
                  (start3 - end2)$Day,
                  H1A_dates3,
                  (start4 - end3)$Day,
                  H1A_dates4,
                  (start5 - end4)$Day,
                  H1A_dates5,
                  (start6 - end5)$Day,
                  H1A_dates6)


H1A_df_sessions <- data.frame(Interval = c("1st", "2nd", "3rd", "4th", "5th", "6th"),
                              start_date = mapply(format, c(start1, start2, start3, start4, start5, start6), format = "%b %d %Y"),
                              end_date = mapply(format, c(end1, end2, end3, end4, end5, end6), format = "%b %d %Y"),
                              Hours = totalH1AHours,
                              Days = totalH1ADays[seq(1, length(totalH1ADays), 2)],
                              Starting_Polarity = c(maxP1,maxP2,maxP3,maxP4,maxP5,maxP6),
                              Final_Polarity = c(minP1,minP2,minP3,minP4,minP5,minP6),
                              Change = c(minP1-maxP1, minP2-maxP2, minP3-maxP3, minP4-maxP4, minP5-maxP5, minP6-maxP6))

H1A_df_sessions <- H1A_df_sessions %>% mutate(Change_per_hr = round(Change/Hours, digits = 3))

H1A_df_breaks <- data.frame(Interval = c("1st", "2nd", "3rd", "4th", "5th"),
                            Days = totalH1ADays[seq(2, length(totalH1ADays), 2)],
                            Increase_in_Polarity = c(maxP2-minP1, maxP3-minP2, maxP4-minP3, maxP5-minP4, maxP6-minP5))

H1A_df_breaks <- H1A_df_breaks %>% mutate(Increase_per_day = round(Increase_in_Polarity/Days, digits = 3))

H1A_df_sessions
H1A_df_breaks