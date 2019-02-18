library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
L2_E_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Eric",
                         col_types = c("date", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "numeric"), skip = 2)

L2_E_first_hours <- L2_E_hours[1:11, c(1,2,7)]
L2_E_second_hours <- L2_E_hours[13:25, c(1,2,7)]
L2_E_third_hours <- L2_E_hours[27:39, c(1,2,7)]
L2_E_fourth_hours <- L2_E_hours[41:52, c(1,2,7)]
L2_E_fifth_hours <- L2_E_hours[54:65, c(1,2,7)]

L2_E_first_hours[,3] <- cumsum(L2_E_first_hours[,3])
L2_E_second_hours[,3] <- cumsum(L2_E_second_hours[,3])
L2_E_third_hours[,3] <- cumsum(L2_E_third_hours[,3])
L2_E_fourth_hours[,3] <- cumsum(L2_E_fourth_hours[,3])
L2_E_fifth_hours[,3] <- cumsum(L2_E_fifth_hours[,3])

corr.func <- function(data, method = "pearson"){
  
  cor.test(x = data$`Total therapy duration (Hrs)`, y = data$`Polarity level`, method = method)
  
}

L2_E_all <- list(L2_E_first_hours, L2_E_second_hours, L2_E_third_hours, L2_E_fourth_hours, L2_E_fifth_hours)

L2_E_all_stat <- lapply(X = L2_E_all, FUN = corr.func)

L2_E_all_stat
