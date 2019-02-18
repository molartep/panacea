library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
B1_S_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Stephanie",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", "numeric"), skip = 2)

B1_S_first_hours <- B1_S_hours[1:17, c(1,2,7)]
B1_S_second_hours <- B1_S_hours[19:30, c(1,2,7)]
B1_S_third_hours <- B1_S_hours[32:37, c(1,2,7)]

B1_S_first_hours[,3] <- cumsum(B1_S_first_hours[,3])
B1_S_second_hours[,3] <- cumsum(B1_S_second_hours[,3])
B1_S_third_hours[,3] <- cumsum(B1_S_third_hours[,3])

corr.func <- function(data, method = "pearson"){
  
  cor.test(x = data$`Total therapy duration (Hrs)`, y = data$`Polarity level`, method = method)
  
}

B1_S_all <- list(B1_S_first_hours, B1_S_second_hours, B1_S_third_hours)

B1_S_all_stat <- lapply(X = B1_S_all, FUN = corr.func)

B1_S_all_stat

