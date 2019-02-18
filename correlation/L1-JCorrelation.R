library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
L1_J_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Jeff",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

L1_J_first_hours <- L1_J_hours[1:20, c(1,2,6)]

L1_J_first_hours[,3] <- cumsum(L1_J_first_hours[,3])

corr.func <- function(data, method = "pearson"){
  
  cor.test(x = data$`Total therapy duration (Hrs)`, y = data$`Polarity level`, method = method)
  
}

L1_J_all <- list(L1_J_first_hours)

L1_J_all_stat <- lapply(X = L1_J_all, FUN = corr.func)

L1_J_all_stat