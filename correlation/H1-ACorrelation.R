library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
H1_A_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Anna",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)


H1_A_first_hours <- H1_A_hours[1:9, c(1,2,6)]
H1_A_second_hours <- H1_A_hours[11:23, c(1,2,6)]
H1_A_third_hours <- H1_A_hours[25:26, c(1,2,6)]
H1_A_fourth_hours <- H1_A_hours[28:39, c(1,2,6)]
H1_A_fifth_hours <- H1_A_hours[41:52, c(1,2,6)]

H1_A_first_hours[,3] <- cumsum(H1_A_first_hours[,3])
H1_A_second_hours[,3] <- cumsum(H1_A_second_hours[,3])
H1_A_third_hours[,3] <- cumsum(H1_A_third_hours[,3])
H1_A_fourth_hours[,3] <- cumsum(H1_A_fourth_hours[,3])
H1_A_fifth_hours[,3] <- cumsum(H1_A_fifth_hours[,3])

corr.func <- function(data, method = "pearson"){
  
  cor.test(x = data$`Total therapy duration (Hrs)`, y = data$`Polarity level`, method = method)
  
}

H1_A_all <- list(H1_A_first_hours, H1_A_second_hours, H1_A_fourth_hours, H1_A_fifth_hours)

H1_A_all_statx <- lapply(X = H1_A_all, FUN = corr.func)

H1_A_all_stat <- list(H1_A_all_statx[[1]], H1_A_all_statx[[2]], NA, H1_A_all_statx[[3]], H1_A_all_statx[[4]])

H1_A_all_stat
