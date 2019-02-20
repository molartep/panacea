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

model.func <- function(data){
  
  H1_A_model <- lm(formula = data$`Polarity level` ~ data$`Total therapy duration (Hrs)`)
  
  summary(H1_A_model)
  
}

H1_A_all <- list(H1_A_first_hours, H1_A_second_hours, H1_A_fourth_hours, H1_A_fifth_hours)

H1_A_all_statx <- lapply(X = H1_A_all, FUN = corr.func)
H1_A_all_stat_modelx <- lapply(X = H1_A_all, FUN = model.func)

H1_A_all_stat <- list(H1_A_all_statx[[1]], H1_A_all_statx[[2]], NA, H1_A_all_statx[[3]], H1_A_all_statx[[4]])
H1_A_all_stat_model <- list(H1_A_all_stat_modelx[[1]], H1_A_all_stat_modelx[[2]], NA, H1_A_all_stat_modelx[[3]], H1_A_all_stat_modelx[[4]])

H1_A_estims <- unlist(lapply(X = c(1,2,4,5), FUN = function(x){H1_A_all_stat[[x]]$estimate}))
H1_A_lower <- unlist(lapply(X = c(1,2,4,5), FUN = function(x){H1_A_all_stat[[x]]$conf.int[1]}))
H1_A_upper <- unlist(lapply(X = c(1,2,4,5), FUN = function(x){H1_A_all_stat[[x]]$conf.int[2]}))
H1_A_pvalues <- unlist(lapply(X = c(1,2,4,5), FUN = function(x){H1_A_all_stat[[x]]$p.value}))
H1_A_RSE <- unlist(lapply(X = c(1,2,4,5), FUN = function(x){H1_A_all_stat_model[[x]]$sigma}))
H1_A_dfs <- unlist(lapply(X = c(1,2,4,5), FUN = function(x){H1_A_all_stat_model[[x]]$df[2]}))
H1_A_adj_r_sq <- unlist(lapply(X = c(1,2,4,5), FUN = function(x){H1_A_all_stat_model[[x]]$adj.r.squared}))

H1A_df_stats <- data.frame(Interval = c("1st", "2nd", "3rd", "4th", "5th"),
                           R = c(signif(H1_A_estims[c(1,2)], 4), NA, signif(H1_A_estims[c(3,4)], 4)),
                           Pval = c(signif(H1_A_pvalues[c(1,2)], 4), NA, signif(H1_A_estims[c(3,4)], 4)),
                           Lower = c(signif(H1_A_lower[c(1,2)], 4), NA, signif(H1_A_estims[c(3,4)], 4)),
                           Upper = c(signif(H1_A_upper[c(1,2)], 4), NA, signif(H1_A_estims[c(3,4)], 4)))

H1A_df_stats1 <- data.frame(Interval = c("1st", "2nd", "3rd", "4th", "5th"),
                           Adj_R_sq = c(signif(H1_A_adj_r_sq[c(1,2)], 4), NA, signif(H1_A_adj_r_sq[c(3,4)], 4)),
                           RSE = c(signif(H1_A_RSE[c(1,2)], 4), NA, signif(H1_A_RSE[c(3,4)], 4)),
                           DFs = c(signif(H1_A_dfs[c(1,2)], 4), NA, signif(H1_A_dfs[c(3,4)], 4)))

H1A_df_stats
H1A_df_stats1


