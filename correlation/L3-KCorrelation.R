library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
L3_K_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Kevin",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

L3_K_first_hours <- L3_K_hours[1:7, c(1,2,6)]
L3_K_second_hours <- L3_K_hours[9:20, c(1,2,6)]
L3_K_third_hours <- L3_K_hours[22:28, c(1,2,6)]
L3_K_fourth_hours <- L3_K_hours[30:34, c(1,2,6)]

L3_K_first_hours[,3] <- cumsum(L3_K_first_hours[,3])
L3_K_second_hours[,3] <- cumsum(L3_K_second_hours[,3])
L3_K_third_hours[,3] <- cumsum(L3_K_third_hours[,3])
L3_K_fourth_hours[,3] <- cumsum(L3_K_fourth_hours[,3])

corr.func <- function(data, method = "pearson"){
  
  cor.test(x = data$`Total therapy duration (Hrs)`, y = data$`Polarity level`, method = method)
  
}

L3_K_all <- list(L3_K_first_hours, L3_K_second_hours, L3_K_third_hours, L3_K_fourth_hours)

L3_K_all_stat <- lapply(X = L3_K_all, FUN = corr.func)

L3_K_all_stat