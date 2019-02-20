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

model.func <- function(data){
  
  G1_D_model <- lm(formula = data$`Polarity level` ~ data$`Total therapy duration (Hrs)`)
  
  summary(G1_D_model)
  
}

G1_D_all <- list(G1_D_first_hours, G1_D_second_hours, G1_D_third_hours, G1_D_fourth_hours)

G1_D_all_stat <- lapply(X = G1_D_all, FUN = corr.func)
G1_D_all_stat_model <- lapply(X = G1_D_all, FUN = model.func)

G1_D_estims <- unlist(lapply(X = c(1:length(G1_D_all)), FUN = function(x){G1_D_all_stat[[x]]$estimate}))
G1_D_lower <- unlist(lapply(X = c(1:length(G1_D_all)), FUN = function(x){G1_D_all_stat[[x]]$conf.int[1]}))
G1_D_upper <- unlist(lapply(X = c(1:length(G1_D_all)), FUN = function(x){G1_D_all_stat[[x]]$conf.int[2]}))
G1_D_pvalues <- unlist(lapply(X = c(1:length(G1_D_all)), FUN = function(x){G1_D_all_stat[[x]]$p.value}))
G1_D_RSE <- unlist(lapply(X = c(1:length(G1_D_all)), FUN = function(x){G1_D_all_stat_model[[x]]$sigma}))
G1_D_dfs <- unlist(lapply(X = c(1:length(G1_D_all)), FUN = function(x){G1_D_all_stat_model[[x]]$df[2]}))
G1_D_adj_r_sq <- unlist(lapply(X = c(1:length(G1_D_all)), FUN = function(x){G1_D_all_stat_model[[x]]$adj.r.squared}))



G1D_df_stats <- data.frame(Interval = c("1st", "2nd", "3rd", "4th"),
                           R = signif(G1_D_estims, 4),
                           Pval = signif(G1_D_pvalues, 4),
                           Lower = c(signif(G1_D_lower, 4), NA),
                           Upper = c(signif(G1_D_upper, 4), NA))

G1D_df_stats1 <- data.frame(Interval = c("1st", "2nd", "3rd", "4th"),
                           Adj_R_sq = signif(G1_D_adj_r_sq, 4),
                           RSE = signif(G1_D_RSE, 4),
                           DFs = signif(G1_D_dfs, 4))

G1D_df_stats
G1D_df_stats1

