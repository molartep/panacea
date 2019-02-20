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

model.func <- function(data){
  
  L2_E_model <- lm(formula = data$`Polarity level` ~ data$`Total therapy duration (Hrs)`)
  
  summary(L2_E_model)
  
}

L2_E_all <- list(L2_E_first_hours, L2_E_second_hours, L2_E_third_hours, L2_E_fourth_hours)

L2_E_all_stat <- lapply(X = L2_E_all, FUN = corr.func)
L2_E_all_stat_model <- lapply(X = L2_E_all, FUN = model.func)

L2_E_estims <- unlist(lapply(X = c(1:length(L2_E_all)), FUN = function(x){L2_E_all_stat[[x]]$estimate}))
L2_E_lower <- unlist(lapply(X = c(1:length(L2_E_all)), FUN = function(x){L2_E_all_stat[[x]]$conf.int[1]}))
L2_E_upper <- unlist(lapply(X = c(1:length(L2_E_all)), FUN = function(x){L2_E_all_stat[[x]]$conf.int[2]}))
L2_E_pvalues <- unlist(lapply(X = c(1:length(L2_E_all)), FUN = function(x){L2_E_all_stat[[x]]$p.value}))
L2_E_RSE <- unlist(lapply(X = c(1:length(L2_E_all)), FUN = function(x){L2_E_all_stat_model[[x]]$sigma}))
L2_E_dfs <- unlist(lapply(X = c(1:length(L2_E_all)), FUN = function(x){L2_E_all_stat_model[[x]]$df[2]}))
L2_E_adj_r_sq <- unlist(lapply(X = c(1:length(L2_E_all)), FUN = function(x){L2_E_all_stat_model[[x]]$adj.r.squared}))



L2E_df_stats <- data.frame(Interval = c("1st", "2nd", "3rd", "4th"),
                           R = signif(L2_E_estims, 4),
                           Pval = signif(L2_E_pvalues, 4),
                           Lower = signif(L2_E_lower, 4),
                           Upper = signif(L2_E_upper, 4))

L2E_df_stats1 <- data.frame(Interval = c("1st", "2nd", "3rd", "4th"),
                           Adj_R_sq = signif(L2_E_adj_r_sq, 4),
                           RSE = signif(L2_E_RSE, 4),
                           DFs = signif(L2_E_dfs, 4))

L2E_df_stats
L2E_df_stats1

