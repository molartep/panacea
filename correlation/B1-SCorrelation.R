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

model.func <- function(data){
  
  B1_S_model <- lm(formula = data$`Polarity level` ~ data$`Total therapy duration (Hrs)`)
  
  summary(B1_S_model)
  
}

B1_S_all <- list(B1_S_first_hours, B1_S_second_hours, B1_S_third_hours)

B1_S_all_stat <- lapply(X = B1_S_all, FUN = corr.func)
B1_S_all_stat_model <- lapply(X = B1_S_all, FUN = model.func)

B1_S_estims <- unlist(lapply(X = c(1:length(B1_S_all)), FUN = function(x){B1_S_all_stat[[x]]$estimate}))
B1_S_lower <- unlist(lapply(X = c(1:length(B1_S_all)), FUN = function(x){B1_S_all_stat[[x]]$conf.int[1]}))
B1_S_upper <- unlist(lapply(X = c(1:length(B1_S_all)), FUN = function(x){B1_S_all_stat[[x]]$conf.int[2]}))
B1_S_pvalues <- unlist(lapply(X = c(1:length(B1_S_all)), FUN = function(x){B1_S_all_stat[[x]]$p.value}))
B1_S_RSE <- unlist(lapply(X = c(1:length(B1_S_all)), FUN = function(x){B1_S_all_stat_model[[x]]$sigma}))
B1_S_dfs <- unlist(lapply(X = c(1:length(B1_S_all)), FUN = function(x){B1_S_all_stat_model[[x]]$df[2]}))
B1_S_adj_r_sq <- unlist(lapply(X = c(1:length(B1_S_all)), FUN = function(x){B1_S_all_stat_model[[x]]$adj.r.squared}))



B1S_df_stats <- data.frame(Interval = c("1st", "2nd", "3rd"),
                           R = signif(B1_S_estims, 4),
                           Pval = signif(B1_S_pvalues, 4),
                           Lower = signif(B1_S_lower, 4),
                           Upper = signif(B1_S_upper, 4))

B1S_df_stats1 <- data.frame(Interval = c("1st", "2nd", "3rd"),
                           Adj_R_sq = signif(B1_S_adj_r_sq, 4),
                           RSE = signif(B1_S_RSE, 4),
                           DFs = signif(B1_S_dfs, 4))

B1S_df_stats
B1S_df_stats1

