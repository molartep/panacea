library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
L3_K_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Kevin",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

L3_K_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Kevin",
                         col_types = c("date", "numeric", "numeric"))


L3_K_first_hours <- L3_K_hours[1:7, c(1,2,6)]
L3_K_second_hours <- L3_K_hours[9:20, c(1,2,6)]
L3_K_third_hours <- L3_K_hours[22:28, c(1,2,6)]
L3_K_fourth_hours <- L3_K_hours[30:34, c(1,2,6)]

start1 <- L3_K_first_hours[L3_K_first_hours[,3] > 0, 1][1,1]
end1 <- tail(L3_K_first_hours[L3_K_first_hours[,3] > 0, 1], n = 1)
start2 <- L3_K_second_hours[L3_K_second_hours[,3] > 0, 1][1,1]
end2 <- tail(L3_K_second_hours[L3_K_second_hours[,3] > 0, 1], n = 1)
start3 <- L3_K_third_hours[L3_K_third_hours[,3] > 0, 1][1,1]
end3 <- tail(L3_K_third_hours[L3_K_third_hours[,3] > 0, 1], n = 1)
start4 <- L3_K_fourth_hours[L3_K_fourth_hours[,3] > 0, 1][1,1]
end4 <- tail(L3_K_fourth_hours[L3_K_fourth_hours[,3] > 0, 1], n = 1)

L3K_dates1 <- sum(L3_K_first_hours[,3] > 0)
L3K_dates2 <- sum(L3_K_second_hours[,3] > 0)
L3K_dates3 <- sum(L3_K_third_hours[,3] > 0)
L3K_dates4 <- sum(L3_K_fourth_hours[,3] > 0)

L3_K_first_hours[,3] <- cumsum(L3_K_first_hours[,3])
L3_K_second_hours[,3] <- cumsum(L3_K_second_hours[,3])
L3_K_third_hours[,3] <- cumsum(L3_K_third_hours[,3])
L3_K_fourth_hours[,3] <- cumsum(L3_K_fourth_hours[,3])

maxP1 <- max(L3_K_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(L3_K_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(L3_K_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(L3_K_second_hours$`Polarity level`, na.rm = T)
maxP3 <- max(L3_K_third_hours$`Polarity level`, na.rm = T)
minP3 <- min(L3_K_third_hours$`Polarity level`, na.rm = T)
maxP4 <- max(L3_K_fourth_hours$`Polarity level`, na.rm = T)
minP4 <- min(L3_K_fourth_hours$`Polarity level`, na.rm = T)

totalL3KHours <- c(max(L3_K_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(L3_K_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(L3_K_third_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(L3_K_fourth_hours$`Total therapy duration (Hrs)`, na.rm = T))

totalL3KDays <- c(L3K_dates1, 
                  (start2 - end1)$Day,
                  L3K_dates2,
                  (start3 - end2)$Day,
                  L3K_dates3,
                  (start4 - end3)$Day,
                  L3K_dates4)


L3K_df_sessions <- data.frame(Interval = c("1st", "2nd", "3rd", "4th"),
                              start_date = mapply(format, c(start1, start2, start3, start4), format = "%b %d %Y"),
                              end_date = mapply(format, c(end1, end2, end3, end4), format = "%b %d %Y"),
                              Hours = totalL3KHours,
                              Days = totalL3KDays[seq(1, length(totalL3KDays), 2)],
                              Starting_Polarity = c(maxP1,maxP2,maxP3,maxP4),
                              Final_Polarity = c(minP1,minP2,minP3,minP4),
                              Change = c(minP1-maxP1, minP2-maxP2, minP3-maxP3, minP4-maxP4))

L3K_df_sessions <- L3K_df_sessions %>% mutate(Change_per_hr = round(Change/Hours, digits = 3))

L3K_df_breaks <- data.frame(Interval = c("1st", "2nd", "3rd"),
                            Days = totalL3KDays[seq(2, length(totalL3KDays), 2)],
                            Increase_in_Polarity = c(maxP2-minP1, maxP3-minP2, maxP4-minP3))

L3K_df_breaks <- L3K_df_breaks %>% mutate(Increase_per_day = round(Increase_in_Polarity/Days, digits = 3))

L3K_df_sessions
L3K_df_breaks


