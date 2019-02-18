library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
C1_J_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Jim",
                         col_types = c("date", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "numeric", "numeric"), skip = 2)

C1_J_first_hours <- C1_J_hours[1:18, c(1,2,8)]
C1_J_second_hours <- C1_J_hours[20:32, c(1,2,8)]
C1_J_third_hours <- C1_J_hours[34:45, c(1,2,8)]
C1_J_fourth_hours <- C1_J_hours[47:62, c(1,2,8)]
C1_J_fifth_hours <- C1_J_hours[64:79, c(1,2,8)]

C1_J_first_hours[,3] <- cumsum(C1_J_first_hours[,3])
C1_J_second_hours[,3] <- cumsum(C1_J_second_hours[,3])
C1_J_third_hours[,3] <- cumsum(C1_J_third_hours[,3])
C1_J_fourth_hours[,3] <- cumsum(C1_J_fourth_hours[,3])
C1_J_fifth_hours[,3] <- cumsum(C1_J_fifth_hours[,3])

corr.func <- function(data, method = "pearson"){
  
  cor.test(x = data$`Total therapy duration (Hrs)`, y = data$`Polarity level`, method = method)

}

C1_J_all <- list(C1_J_first_hours, C1_J_second_hours, C1_J_third_hours, C1_J_fourth_hours, C1_J_fifth_hours)

C1_J_all_stat <- lapply(X = C1_J_all, FUN = corr.func)

C1_J_all_stat