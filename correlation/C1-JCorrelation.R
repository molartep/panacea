library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
C1_J_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Jim",
                         col_types = c("date", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "numeric", "numeric"), skip = 2)

C1_J_first_hours <- C1_J_hours[1:18, c(1,2,8)]
C1_J_second_hours <- C1_J_hours[20:32, c(1,2,8)]
C1_J_third_hours <- C1_J_hours[34:45, c(1,2,8)]
C1_J_fourth_hours <- C1_J_hours[47:62, c(1,2,8)]
C1_J_fifth_hours <- C1_J_hours[64:75, c(1,2,8)]
C1_J_sixth_hours <- C1_J_hours[77:90, c(1,2,8)]

C1_J_first_hours[,3] <- cumsum(C1_J_first_hours[,3])
C1_J_second_hours[,3] <- cumsum(C1_J_second_hours[,3])
C1_J_third_hours[,3] <- cumsum(C1_J_third_hours[,3])
C1_J_fourth_hours[,3] <- cumsum(C1_J_fourth_hours[,3])
C1_J_fifth_hours[,3] <- cumsum(C1_J_fifth_hours[,3])
C1_J_sixth_hours[,3] <- cumsum(C1_J_sixth_hours[,3])

corr.func <- function(data, method = "pearson"){
  
  cor.test(x = data$`Total therapy duration (Hrs)`, y = data$`Polarity level`, method = method)
  
}

model.func <- function(data){
  
  C1_J_model <- lm(formula = data$`Polarity level` ~ data$`Total therapy duration (Hrs)`)
  
  summary(C1_J_model)
  
}

C1_J_all <- list(C1_J_first_hours, C1_J_second_hours, C1_J_third_hours, C1_J_fourth_hours, C1_J_fifth_hours, C1_J_sixth_hours)

C1_J_all_stat <- lapply(X = C1_J_all, FUN = corr.func)
C1_J_all_stat_model <- lapply(X = C1_J_all, FUN = model.func)

C1_J_estims <- unlist(lapply(X = c(1:length(C1_J_all)), FUN = function(x){C1_J_all_stat[[x]]$estimate}))
C1_J_lower <- unlist(lapply(X = c(1:length(C1_J_all)), FUN = function(x){C1_J_all_stat[[x]]$conf.int[1]}))
C1_J_upper <- unlist(lapply(X = c(1:length(C1_J_all)), FUN = function(x){C1_J_all_stat[[x]]$conf.int[2]}))
C1_J_pvalues <- unlist(lapply(X = c(1:length(C1_J_all)), FUN = function(x){C1_J_all_stat[[x]]$p.value}))
C1_J_RSE <- unlist(lapply(X = c(1:length(C1_J_all)), FUN = function(x){C1_J_all_stat_model[[x]]$sigma}))
C1_J_dfs <- unlist(lapply(X = c(1:length(C1_J_all)), FUN = function(x){C1_J_all_stat_model[[x]]$df[2]}))
C1_J_adj_r_sq <- unlist(lapply(X = c(1:length(C1_J_all)), FUN = function(x){C1_J_all_stat_model[[x]]$adj.r.squared}))



C1J_df_stats <- data.frame(Interval = c("1st", "2nd", "3rd", "4th", "5th", "6th"),
                           R = signif(C1_J_estims, 4),
                           Pval = signif(C1_J_pvalues, 4),
                           Lower = signif(C1_J_lower, 4),
                           Upper = signif(C1_J_upper, 4))
                           
C1J_df_stats1 <- data.frame(Interval = c("1st", "2nd", "3rd", "4th", "5th", "6th"),                       
                           Adj_R_sq = signif(C1_J_adj_r_sq, 4),
                           RSE = signif(C1_J_RSE, 4),
                           DFs = signif(C1_J_dfs, 4))

C1J_df_stats
C1J_df_stats1

