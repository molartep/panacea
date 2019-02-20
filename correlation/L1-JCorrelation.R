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

model.func <- function(data){
  
  L1_J_model <- lm(formula = data$`Polarity level` ~ data$`Total therapy duration (Hrs)`)
  
  summary(L1_J_model)
  
}

L1_J_all <- list(L1_J_first_hours)

L1_J_all_stat <- lapply(X = L1_J_all, FUN = corr.func)
L1_J_all_stat_model <- lapply(X = L1_J_all, FUN = model.func)

L1_J_estims <- as.numeric(unlist(lapply(X = c(1:length(L1_J_all)), FUN = function(x){L1_J_all_stat[[x]]$estimate[1]})))
L1_J_lower <- unlist(lapply(X = c(1:length(L1_J_all)), FUN = function(x){L1_J_all_stat[[x]]$conf.int[1]}))
L1_J_upper <- unlist(lapply(X = c(1:length(L1_J_all)), FUN = function(x){L1_J_all_stat[[x]]$conf.int[2]}))
L1_J_pvalues <- unlist(lapply(X = c(1:length(L1_J_all)), FUN = function(x){L1_J_all_stat[[x]]$p.value}))
L1_J_RSE <- unlist(lapply(X = c(1:length(L1_J_all)), FUN = function(x){L1_J_all_stat_model[[x]]$sigma}))
L1_J_dfs <- unlist(lapply(X = c(1:length(L1_J_all)), FUN = function(x){L1_J_all_stat_model[[x]]$df[2]}))
L1_J_adj_r_sq <- unlist(lapply(X = c(1:length(L1_J_all)), FUN = function(x){L1_J_all_stat_model[[x]]$adj.r.squared}))

L1J_df_stats <- data.frame(Interval = c("1st"),
                           R = signif(L1_J_estims, 4),
                           Pval = signif(L1_J_pvalues, 4),
                           Lower = signif(L1_J_lower, 4),
                           Upper = signif(L1_J_upper, 4))

L1J_df_stats1 <- data.frame(Interval = c("1st"),
                           Adj_R_sq = signif(L1_J_adj_r_sq, 4),
                           RSE = signif(L1_J_RSE, 4),
                           DFs = signif(L1_J_dfs, 4))

L1J_df_stats
L1J_df_stats1

