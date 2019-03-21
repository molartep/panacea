library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
G1_D_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Daryl",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

G1_D_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Daryl",
                         col_types = c("date", "numeric", "numeric"))


G1_D_first_hours <- G1_D_hours[1:4, c(1,2,6)]
G1_D_second_hours <- G1_D_hours[6:9, c(1,2,6)]
G1_D_third_hours <- G1_D_hours[11:14, c(1,2,6)]
G1_D_fourth_hours <- G1_D_hours[16:19, c(1,2,6)]
G1_D_fifth_hours <- G1_D_hours[21:25, c(1,2,6)]

start1 <- G1_D_first_hours[G1_D_first_hours[,3] > 0, 1][1,1]
end1 <- tail(G1_D_first_hours[G1_D_first_hours[,3] > 0, 1], n = 1)
start2 <- G1_D_second_hours[G1_D_second_hours[,3] > 0, 1][1,1]
end2 <- tail(G1_D_second_hours[G1_D_second_hours[,3] > 0, 1], n = 1)
start3 <- G1_D_third_hours[G1_D_third_hours[,3] > 0, 1][1,1]
end3 <- tail(G1_D_third_hours[G1_D_third_hours[,3] > 0, 1], n = 1)
start4 <- G1_D_fourth_hours[G1_D_fourth_hours[,3] > 0, 1][1,1]
end4 <- tail(G1_D_fourth_hours[G1_D_fourth_hours[,3] > 0, 1], n = 1)
start5 <- G1_D_fifth_hours[G1_D_fifth_hours[,3] > 0, 1][1,1]
end5 <- tail(G1_D_fifth_hours[G1_D_fifth_hours[,3] > 0, 1], n = 1)

G1D_dates1 <- sum(G1_D_first_hours[,3] > 0)
G1D_dates2 <- sum(G1_D_second_hours[,3] > 0)
G1D_dates3 <- sum(G1_D_third_hours[,3] > 0)
G1D_dates4 <- sum(G1_D_fourth_hours[,3] > 0)
G1D_dates5 <- sum(G1_D_fifth_hours[,3] > 0)

G1_D_first_hours[,3] <- cumsum(G1_D_first_hours[,3])
G1_D_second_hours[,3] <- cumsum(G1_D_second_hours[,3])
G1_D_third_hours[,3] <- cumsum(G1_D_third_hours[,3])
G1_D_fourth_hours[,3] <- cumsum(G1_D_fourth_hours[,3])
G1_D_fifth_hours[,3] <- cumsum(G1_D_fifth_hours[,3])

maxP1 <- max(G1_D_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(G1_D_first_hours$`Polarity level`, na.rm = T)
maxP2 <- max(G1_D_second_hours$`Polarity level`, na.rm = T)
minP2 <- min(G1_D_second_hours$`Polarity level`, na.rm = T)
maxP3 <- max(G1_D_third_hours$`Polarity level`, na.rm = T)
minP3 <- min(G1_D_third_hours$`Polarity level`, na.rm = T)
maxP4 <- max(G1_D_fourth_hours$`Polarity level`, na.rm = T)
minP4 <- min(G1_D_fourth_hours$`Polarity level`, na.rm = T)
maxP5 <- max(G1_D_fifth_hours$`Polarity level`, na.rm = T)
minP5 <- min(G1_D_fifth_hours$`Polarity level`, na.rm = T)

totalG1DHours <- c(max(G1_D_first_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(G1_D_second_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(G1_D_third_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(G1_D_fourth_hours$`Total therapy duration (Hrs)`, na.rm = T),
                   max(G1_D_fifth_hours$`Total therapy duration (Hrs)`, na.rm = T))

totalG1DDays <- c(G1D_dates1, 
                  (start2 - end1)$Day,
                  G1D_dates2,
                  (start3 - end2)$Day,
                  G1D_dates3,
                  (start4 - end3)$Day,
                  G1D_dates4,
                  (start5 - end4)$Day,
                  G1D_dates5)


G1D_df_sessions <- data.frame(Interval = c("1st", "2nd", "3rd", "4th", "5th"),
                              start_date = mapply(format, c(start1, start2, start3, start4, start5), format = "%b %d %Y"),
                              end_date = mapply(format, c(end1, end2, end3, end4, end5), format = "%b %d %Y"),
                              Hours = totalG1DHours,
                              Days = totalG1DDays[seq(1, length(totalG1DDays), 2)],
                              Starting_Polarity = c(maxP1,maxP2,maxP3,maxP4,maxP5),
                              Final_Polarity = c(minP1,minP2,minP3,minP4,minP5),
                              Change = c(minP1-maxP1, minP2-maxP2, minP3-maxP3, minP4-maxP4, minP5-maxP5))

G1D_df_sessions <- G1D_df_sessions %>% mutate(Change_per_hr = round(Change/Hours, digits = 3))

G1D_df_breaks <- data.frame(Interval = c("1st", "2nd", "3rd", "4th"),
                            Days = totalG1DDays[seq(2, length(totalG1DDays), 2)],
                            Increase_in_Polarity = c(maxP2-minP1, maxP3-minP2, maxP4-minP3, maxP5-minP4))

G1D_df_breaks <- G1D_df_breaks %>% mutate(Increase_per_day = round(Increase_in_Polarity/Days, digits = 3))

G1D_df_sessions
G1D_df_breaks
