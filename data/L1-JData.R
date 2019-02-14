library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
L1_J_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Jeff",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

L1_J_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Jeff",
                         col_types = c("date", "numeric", "numeric"))


L1_J_first_hours <- L1_J_hours[1:20, c(1,2,6)]

start1 <- L1_J_first_hours[L1_J_first_hours[,3] > 0, 1][1,1]
end1 <- tail(L1_J_first_hours[L1_J_first_hours[,3] > 0, 1], n = 1)

L1J_dates1 <- sum(L1_J_first_hours[,3] > 0)

L1_J_first_hours[,3] <- cumsum(L1_J_first_hours[,3])

maxP1 <- max(L1_J_first_hours$`Polarity level`, na.rm = T)
minP1 <- min(L1_J_first_hours$`Polarity level`, na.rm = T)

totalL1JHours <- c(max(L1_J_first_hours$`Total therapy duration (Hrs)`, na.rm = T))

totalL1JDays <- c(L1J_dates1)

L1J_df_sessions <- data.frame(Interval = c("1st"),
                              start_date = mapply(format, c(start1), format = "%b %d %Y"),
                              end_date = mapply(format, c(end1), format = "%b %d %Y"),
                              Hours = totalL1JHours,
                              Days = totalL1JDays[seq(1, length(totalL1JDays), 2)],
                              Starting_Polarity = c(maxP1),
                              Final_Polarity = c(minP1),
                              Change = c(minP1-maxP1))

L1J_df_sessions <- L1J_df_sessions %>% mutate(Change_per_hr = round(Change/Hours, digits = 3))

L1J_df_sessions


