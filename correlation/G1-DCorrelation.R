library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
G1_D_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Daryl",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

G1_D_first_hours <- G1_D_hours[1:4, c(1,2,6)]
G1_D_second_hours <- G1_D_hours[6:9, c(1,2,6)]
G1_D_third_hours <- G1_D_hours[11:14, c(1,2,6)]
G1_D_fourth_hours <- G1_D_hours[16:19, c(1,2,6)]

G1_D_first_hours[,3] <- cumsum(G1_D_first_hours[,3])
G1_D_second_hours[,3] <- cumsum(G1_D_second_hours[,3])
G1_D_third_hours[,3] <- cumsum(G1_D_third_hours[,3])
G1_D_fourth_hours[,3] <- cumsum(G1_D_fourth_hours[,3])

corr.func <- function(data, method = "pearson"){
  
  cor.test(x = data$`Total therapy duration (Hrs)`, y = data$`Polarity level`, method = method)
  
}

G1_D_all <- list(G1_D_first_hours, G1_D_second_hours, G1_D_third_hours, G1_D_fourth_hours)

G1_D_all_stat <- lapply(X = G1_D_all, FUN = corr.func)

G1_D_all_stat