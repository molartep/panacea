library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
L2_E_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Eric",
                         col_types = c("date", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "numeric"), skip = 2)

L2_E_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Eric",
                         col_types = c("date", "numeric", "numeric"))


L2_E_first_hours <- L2_E_hours[1:11, c(1,2,7)]
L2_E_second_hours <- L2_E_hours[13:25, c(1,2,7)]
L2_E_third_hours <- L2_E_hours[27:39, c(1,2,7)]
L2_E_fourth_hours <- L2_E_hours[41:52, c(1,2,7)]
L2_E_fifth_hours <- L2_E_hours[54:65, c(1,2,7)]

start1 <- L2_E_first_hours[L2_E_first_hours[,3] > 0, 1][1,1]
end1 <- tail(L2_E_first_hours[L2_E_first_hours[,3] > 0, 1], n = 1)
start2 <- L2_E_second_hours[L2_E_second_hours[,3] > 0, 1][1,1]
end2 <- tail(L2_E_second_hours[L2_E_second_hours[,3] > 0, 1], n = 1)
start3 <- L2_E_third_hours[L2_E_third_hours[,3] > 0, 1][1,1]
end3 <- tail(L2_E_third_hours[L2_E_third_hours[,3] > 0, 1], n = 1)
start4 <- L2_E_fourth_hours[L2_E_fourth_hours[,3] > 0, 1][1,1]
end4 <- tail(L2_E_fourth_hours[L2_E_fourth_hours[,3] > 0, 1], n = 1)
start5 <- L2_E_fifth_hours[L2_E_fifth_hours[,3] > 0, 1][1,1]
end5 <- tail(L2_E_fifth_hours[L2_E_fifth_hours[,3] > 0, 1], n = 1)

L2E_dates1 <- sum(L2_E_first_hours[,3] > 0)
L2E_dates2 <- sum(L2_E_second_hours[,3] > 0)
L2E_dates3 <- sum(L2_E_third_hours[,3] > 0)
L2E_dates4 <- sum(L2_E_fourth_hours[,3] > 0)
L2E_dates5 <- sum(L2_E_fifth_hours[,3] > 0)

L2_E_first_hours[,3] <- cumsum(L2_E_first_hours[,3])
L2_E_second_hours[,3] <- cumsum(L2_E_second_hours[,3])
L2_E_third_hours[,3] <- cumsum(L2_E_third_hours[,3])
L2_E_fourth_hours[,3] <- cumsum(L2_E_fourth_hours[,3])
L2_E_fifth_hours[,3] <- cumsum(L2_E_fifth_hours[,3])

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

totalL2EHours <- c(max(L2_E_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(L2_E_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(L2_E_third_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(L2_E_fourth_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(L2_E_fifth_hours$`Total therapy duration (Hrs)`, na.rm = T))

totalL2EDays <- c(L2E_dates1, 
                  (start2 - end1)$Day,
                  L2E_dates2,
                  (start3 - end2)$Day,
                  L2E_dates3,
                  (start4 - end3)$Day,
                  L2E_dates4,
                  (start5 - end4)$Day,
                  L2E_dates5)

L2E_df_sessions <- data.frame(Interval = c("1st", "2nd", "3rd", "4th", "5th"),
                              start_date = mapply(format, c(start1, start2, start3, start4, start5), format = "%b %d %Y"),
                              end_date = mapply(format, c(end1, end2, end3, end4, end5), format = "%b %d %Y"),
                              Hours = totalL2EHours,
                              Days = totalL2EDays[seq(1, length(totalL2EDays), 2)],
                              Starting_Polarity = c(maxP1,maxP2,maxP3,maxP4,maxP5),
                              Final_Polarity = c(minP1,minP2,minP3,minP4,minP5),
                              Change = c(minP1-maxP1, minP2-maxP2, minP3-maxP3, minP4-maxP4, minP5-maxP5))

L2E_df_sessions <- L2E_df_sessions %>% mutate(Change_per_hr = round(Change/Hours, digits = 3))

L2E_df_breaks <- data.frame(Interval = c("1st", "2nd", "3rd", "4th"),
                            Days = totalL2EDays[seq(2, length(totalL2EDays), 2)],
                            Increase_in_Polarity = c(maxP2-minP1, maxP3-minP2, maxP4-minP3, maxP5-minP4))

L2E_df_breaks <- L2E_df_breaks %>% mutate(Increase_per_day = round(Increase_in_Polarity/Days, digits = 3))

L2E_df_sessions
L2E_df_breaks
