library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
B1_S_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Stephanie",
                                col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric"), skip = 2)

B1_S_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Stephanie",
                         col_types = c("date", "numeric", "numeric"))


B1_S_first_hours <- B1_S_hours[1:17, c(1,2,7)]
B1_S_second_hours <- B1_S_hours[19:30, c(1,2,7)]
B1_S_third_hours <- B1_S_hours[32:37, c(1,2,7)]

start1 <- B1_S_first_hours[B1_S_first_hours[,3] > 0, 1][1,1]
end1 <- tail(B1_S_first_hours[B1_S_first_hours[,3] > 0, 1], n = 1)
start2 <- B1_S_second_hours[B1_S_second_hours[,3] > 0, 1][1,1]
end2 <- tail(B1_S_second_hours[B1_S_second_hours[,3] > 0, 1], n = 1)
start3 <- B1_S_third_hours[B1_S_third_hours[,3] > 0, 1][1,1]
end3 <- tail(B1_S_third_hours[B1_S_third_hours[,3] > 0, 1], n = 1)

B1S_dates1 <- sum(B1_S_first_hours[,3] > 0)
B1S_dates2 <- sum(B1_S_second_hours[,3] > 0)
B1S_dates3 <- sum(B1_S_third_hours[,3] > 0)

B1_S_first_hours[,3] <- cumsum(B1_S_first_hours[,3])
B1_S_second_hours[,3] <- cumsum(B1_S_second_hours[,3])
B1_S_third_hours[,3] <- cumsum(B1_S_third_hours[,3])

maxP1 <- max(B1_S_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(B1_S_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(B1_S_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(B1_S_second_hours$`Polarity level`, na.rm = T)
maxP3 <- max(B1_S_third_hours$`Polarity level`, na.rm = T)
minP3 <- min(B1_S_third_hours$`Polarity level`, na.rm = T)

totalB1SHours <- c(max(B1_S_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(B1_S_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(B1_S_third_hours$`Total therapy duration (Hrs)`, na.rm = T))

totalB1SDays <- c(B1S_dates1, 
                  (start2 - end1)$Day,
                  B1S_dates2,
                  (start3 - end2)$Day,
                  B1S_dates3)

B1S_df_sessions <- data.frame(Interval = c("1st", "2nd", "3rd"),
                              start_date = mapply(format, c(start1, start2, start3), format = "%b %d %Y"),
                              end_date = mapply(format, c(end1, end2, end3), format = "%b %d %Y"),
                              Hours = totalB1SHours,
                              Days = totalB1SDays[seq(1, length(totalB1SDays), 2)],
                              Starting_Polarity = c(maxP1,maxP2,maxP3),
                              Final_Polarity = c(minP1,minP2,minP3),
                              Change = c(minP1-maxP1, minP2-maxP2, minP3-maxP3))

B1S_df_sessions <- B1S_df_sessions %>% mutate(Change_per_hr = round(Change/Hours, digits = 3))

B1S_df_breaks <- data.frame(Interval = c("1st", "2nd"),
                            Days = totalB1SDays[seq(2, length(totalB1SDays), 2)],
                            Increase_in_Polarity = c(maxP2-minP1, maxP3-minP2))

B1S_df_breaks <- B1S_df_breaks %>% mutate(Increase_per_day = round(Increase_in_Polarity/Days, digits = 3))

B1S_df_sessions
B1S_df_breaks