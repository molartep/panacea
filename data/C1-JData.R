library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(bizdays)
C1_J_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Jim",
                         col_types = c("date", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "numeric", "numeric"), skip = 2)

C1_J_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Jim",
                         col_types = c("date", "numeric", "numeric"))


C1_J_first_hours <- C1_J_hours[1:18, c(1,2,8)]
C1_J_second_hours <- C1_J_hours[20:32, c(1,2,8)]
C1_J_third_hours <- C1_J_hours[34:45, c(1,2,8)]
C1_J_fourth_hours <- C1_J_hours[47:62, c(1,2,8)]
C1_J_fifth_hours <- C1_J_hours[64:79, c(1,2,8)]

start1 <- C1_J_first_hours[C1_J_first_hours[,3] > 0, 1][1,1]
end1 <- tail(C1_J_first_hours[C1_J_first_hours[,3] > 0, 1], n = 1)
start2 <- C1_J_second_hours[C1_J_second_hours[,3] > 0, 1][1,1]
end2 <- tail(C1_J_second_hours[C1_J_second_hours[,3] > 0, 1], n = 1)
start3 <- C1_J_third_hours[C1_J_third_hours[,3] > 0, 1][1,1]
end3 <- tail(C1_J_third_hours[C1_J_third_hours[,3] > 0, 1], n = 1)
start4 <- C1_J_fourth_hours[C1_J_fourth_hours[,3] > 0, 1][1,1]
end4 <- tail(C1_J_fourth_hours[C1_J_fourth_hours[,3] > 0, 1], n = 1)
start5 <- C1_J_fifth_hours[C1_J_fourth_hours[,3] > 0, 1][1,1]
end5 <- tail(C1_J_fifth_hours[C1_J_fourth_hours[,3] > 0, 1], n = 1)

C1J_dates1 <- sum(C1_J_first_hours[,3] > 0)
C1J_dates2 <- sum(C1_J_second_hours[,3] > 0)
C1J_dates3 <- sum(C1_J_third_hours[,3] > 0)
C1J_dates4 <- sum(C1_J_fourth_hours[,3] > 0)
C1J_dates5 <- sum(C1_J_fifth_hours[,3] > 0)

C1_J_first_hours[,3] <- cumsum(C1_J_first_hours[,3])
C1_J_second_hours[,3] <- cumsum(C1_J_second_hours[,3])
C1_J_third_hours[,3] <- cumsum(C1_J_third_hours[,3])
C1_J_fourth_hours[,3] <- cumsum(C1_J_fourth_hours[,3])
C1_J_fifth_hours[,3] <- cumsum(C1_J_fifth_hours[,3])

maxP1 <- max(C1_J_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(C1_J_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(C1_J_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(C1_J_second_hours$`Polarity level`, na.rm = T)
maxP3 <- max(C1_J_third_hours$`Polarity level`, na.rm = T)
minP3 <- min(C1_J_third_hours$`Polarity level`, na.rm = T)
maxP4 <- max(C1_J_fourth_hours$`Polarity level`, na.rm = T)
minP4 <- min(C1_J_fourth_hours$`Polarity level`, na.rm = T)
maxP5 <- max(C1_J_fifth_hours$`Polarity level`, na.rm = T)
minP5 <- min(C1_J_fifth_hours$`Polarity level`, na.rm = T)

totalC1JHours <- c(max(C1_J_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(C1_J_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(C1_J_third_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(C1_J_fourth_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(C1_J_fifth_hours$`Total therapy duration (Hrs)`, na.rm = T))

totalC1JDays <- c(C1J_dates1, 
                  (start2 - end1)$Day,
                  C1J_dates2,
                  (start3 - end2)$Day,
                  C1J_dates3,
                  (start4 - end3)$Day,
                  C1J_dates4,
                  (start5 - end4)$Day,
                  C1J_dates5)


df_sessions <- data.frame(Interval = c("1st", "2nd", "3rd", "4th", "5th"),
                          start_date = mapply(format, c(start1, start2, start3, start4, start5), format = "%b %d %Y"),
                          end_date = mapply(format, c(end1, end2, end3, end4, end5), format = "%b %d %Y"),
                          Hours = totalC1JHours,
                          Days = totalC1JDays[seq(1, length(totalC1JDays), 2)],
                          Starting_Polarity = c(maxP1,maxP2,maxP3,maxP4,maxP5),
                          Final_Polarity = c(minP1,minP2,minP3,minP4,minP5),
                          Change = c(minP1-maxP1, minP2-maxP2, minP3-maxP3, minP4-maxP4, minP5-maxP5))

df_sessions <- df_sessions %>% mutate(Change_per_hr = round(Change/Hours, digits = 3))

df_breaks <- data.frame(Interval = c("1st", "2nd", "3rd", "4th"),
                        Days = totalC1JDays[seq(2, length(totalC1JDays), 2)],
                        Increase_in_Polarity = c(maxP2-minP1, maxP3-minP2, maxP4-minP3, maxP5-minP4))

df_breaks <- df_breaks %>% mutate(Increase_per_day = round(Increase_in_Polarity/Days, digits = 3))

df_sessions
df_breaks
