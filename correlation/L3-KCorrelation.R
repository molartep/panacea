library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
library(MLmetrics)
L3_K_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Kevin",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

L3_K_first_hours <- L3_K_hours[1:7, c(1,2,6)]
L3_K_second_hours <- L3_K_hours[9:20, c(1,2,6)]
L3_K_third_hours <- L3_K_hours[22:28, c(1,2,6)]
L3_K_fourth_hours <- L3_K_hours[30:34, c(1,2,6)]
L3_K_fifth_hours <- L3_K_hours[36:42, c(1,2,6)]

L3_K_first_hours[,3] <- cumsum(L3_K_first_hours[,3])
L3_K_second_hours[,3] <- cumsum(L3_K_second_hours[,3])
L3_K_third_hours[,3] <- cumsum(L3_K_third_hours[,3])
L3_K_fourth_hours[,3] <- cumsum(L3_K_fourth_hours[,3])
L3_K_fifth_hours[,3] <- cumsum(L3_K_fifth_hours[,3])

corr.func <- function(data, method = "pearson"){
  
  cor.test(x = data$`Total therapy duration (Hrs)`, y = data$`Polarity level`, method = method)
  
}

model.func <- function(data){
  
  L3_K_model <- lm(formula = data$`Polarity level` ~ data$`Total therapy duration (Hrs)`)
  
  summary(L3_K_model)
  
}

L3_K_all <- list(L3_K_first_hours, L3_K_second_hours, L3_K_third_hours, L3_K_fourth_hours, L3_K_fifth_hours)

L3_K_all_stat <- lapply(X = L3_K_all, FUN = corr.func)
L3_K_all_stat_model <- lapply(X = L3_K_all, FUN = model.func)

L3_K_estims <- unlist(lapply(X = c(1:length(L3_K_all)), FUN = function(x){L3_K_all_stat[[x]]$estimate}))
L3_K_lower <- unlist(lapply(X = c(1:length(L3_K_all)), FUN = function(x){L3_K_all_stat[[x]]$conf.int[1]}))
L3_K_upper <- unlist(lapply(X = c(1:length(L3_K_all)), FUN = function(x){L3_K_all_stat[[x]]$conf.int[2]}))
L3_K_pvalues <- unlist(lapply(X = c(1:length(L3_K_all)), FUN = function(x){L3_K_all_stat[[x]]$p.value}))
L3_K_RSE <- unlist(lapply(X = c(1:length(L3_K_all)), FUN = function(x){L3_K_all_stat_model[[x]]$sigma}))
L3_K_dfs <- unlist(lapply(X = c(1:length(L3_K_all)), FUN = function(x){L3_K_all_stat_model[[x]]$df[2]}))
L3_K_adj_r_sq <- unlist(lapply(X = c(1:length(L3_K_all)), FUN = function(x){L3_K_all_stat_model[[x]]$adj.r.squared}))



L3K_df_stats <- data.frame(Interval = c("1st", "2nd", "3rd", "4th", "5th"),
                           R = signif(L3_K_estims, 4),
                           Pval = signif(L3_K_pvalues, 4),
                           Lower = signif(L3_K_lower, 4),
                           Upper = signif(L3_K_upper, 4))

L3K_df_stats1 <- data.frame(Interval = c("1st", "2nd", "3rd", "4th", "5th"),
                           Adj_R_sq = signif(L3_K_adj_r_sq, 4),
                           RSE = signif(L3_K_RSE, 4),
                           DFs = signif(L3_K_dfs, 4))

L3K_df_stats
L3K_df_stats1

