library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(scales)

B1_S <- read_excel("~/Desktop/Electronegatividad.xlsx", 
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric"), skip = 2)
C1_J <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Jim",
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric"), skip = 2)
G1_D <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Daryl",
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"), skip = 2)
H1_A <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Anna",
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"), skip = 2)
L1_J <- read_excel("~/Desktop/Electronegatividad.xlsx", 
                   sheet = "Jeff", col_types = c("text", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric"), skip = 2)
L2_E <- read_excel("~/Desktop/Electronegatividad.xlsx", 
                   sheet = "Eric", col_types = c("text", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", "numeric"), skip = 2)
L3_K <- read_excel("~/Desktop/Electronegatividad.xlsx", 
                   sheet = "Kevin", col_types = c("text", "numeric", 
                                                  "numeric", "numeric",
                                                  "numeric", "numeric"), skip = 2)

margin_top = theme(plot.margin = unit(c(0.25,1,1,1),"cm"))
margin_bottom = theme(plot.margin = unit(c(0.25,1,1,1),"cm"))
margin = theme(plot.margin = unit(c(1,1,1,1),"cm"))

################################################################################################################

B1_S_first_session <- B1_S[1:17,] %>% mutate(days_after = 0:16)
B1_S_second_session <- B1_S[19:30,] %>% mutate(days_after = 0:11)
B1_S_third_session <- B1_S[32:37,] %>% mutate(days_after = 0:5)

y_maxB1S <- max(B1_S_first_session$`Polarity level`, na.rm = T)

f1 <- max(B1_S_first_session$days_after) + 1
s1 <- max(B1_S_second_session$days_after) + 1
t1 <- max(B1_S_third_session$days_after)

B1_S_first_session[,7] <- cumsum(B1_S_first_session[,7])
B1_S_second_session[,7] <- cumsum(B1_S_second_session[,7])
B1_S_third_session[,7] <- cumsum(B1_S_third_session[,7])

B1_S_first_progress <- B1_S_first_session %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxB1S)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = B1_S_first_session[c(1,f1),c(7,2)],
            label = B1_S_first_session$`Polarity level`[c(1, f1)],
            nudge_y = -15)

B1_S_second_progress <- B1_S_second_session %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxB1S)) +
  labs(subtitle = "Second Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = B1_S_second_session[c(1,s1),c(7,2)],
            label = B1_S_second_session$`Polarity level`[c(1, s1)],
            nudge_y = 15,
            nudge_x = 0.8)

B1_S_third_progress <- B1_S_third_session %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxB1S)) +
  labs(subtitle = "Third Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = B1_S_third_session[c(2,t1),c(7,2)],
            label = B1_S_third_session$`Polarity level`[c(2, t1)],
            nudge_y = -15)

first_grobsB1S <- list(B1_S_first_progress, B1_S_second_progress)
second_grobsB1S <- list(B1_S_third_progress)

B1SFINAL <- grid.arrange(top = "B1-S Polarity", ncol=2,
                         grobs = c(lapply(first_grobsB1S, "+", margin_top),
                                   lapply(second_grobsB1S, "+", margin_bottom)))

###############################################################################################################

first_C1_J <- C1_J[1:18,] %>% mutate(days_after = 0:17)
second_C1_J <- C1_J[20:32,] %>% mutate(days_after = 0:12)
third_C1_J <- C1_J[34:45,] %>% mutate(days_after = 0:11)
fourth_C1_J <- C1_J[47:62,] %>% mutate(days_after = 0:15)

y_maxC1J <- max(first_C1_J$`Polarity level`, na.rm = T)

f1 <- max(first_C1_J$days_after) + 1
s1 <- max(second_C1_J$days_after) + 1
t1 <- max(third_C1_J$days_after) + 1
fo1 <- max(fourth_C1_J[complete.cases(fourth_C1_J[,2]),]$days_after) + 1

first_C1_J[,8] <- cumsum(first_C1_J[,8])
second_C1_J[,8] <- cumsum(second_C1_J[,8])
third_C1_J[,8] <- cumsum(third_C1_J[,8])
fourth_C1_J[,8] <- cumsum(fourth_C1_J[,8])

first_graph_C1_J <- first_C1_J %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxC1J)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = first_C1_J[c(1,f1),c(8,2)],
            label = first_C1_J$`Polarity level`[c(1, f1)],
            nudge_y = -15,
            nudge_x = 1.5,
            size = 3.5)

second_graph_C1_J <- second_C1_J %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxC1J)) +
  labs(subtitle = "Second Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = second_C1_J[c(2,s1),c(8,2)],
            label = second_C1_J$`Polarity level`[c(2, s1)],
            nudge_y = -14,
            size = 3.5)

third_graph_C1_J <- third_C1_J %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxC1J)) +
  labs(subtitle = "Third Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = third_C1_J[c(2,t1),c(8,2)],
            label = third_C1_J$`Polarity level`[c(2, t1)],
            nudge_y = -14,
            nudge_x = -3,
            size = 3.5)

fourth_graph_C1_J <- fourth_C1_J %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxC1J)) +
  labs(subtitle = "Fourth Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = fourth_C1_J[c(4,fo1),c(8,2)],
            label = fourth_C1_J$`Polarity level`[c(4, fo1)],
            nudge_y = 14,
            size = 3.5)

first_grobsC1J <- list(first_graph_C1_J, second_graph_C1_J)
second_grobsC1J <- list(third_graph_C1_J, fourth_graph_C1_J)

C1JFINAL <- grid.arrange(top = "C1-J Polarity", ncol=2,
                         grobs = c(lapply(first_grobsC1J, "+", margin_top),
                                   lapply(second_grobsC1J, "+", margin_bottom)))

################################################################################################################

first <- G1_D[1:4,] %>% mutate(days_after = 0:3)
second <- G1_D[6:9,] %>% mutate(days_after = 0:3)
third <- G1_D[11:14,] %>% mutate(days_after = 0:3)

y_max1 <- max(first$`Polarity level`, na.rm = T)

f1 <- max(first$days_after) + 1
s1 <- max(second$days_after) + 1
t1 <- max(third[complete.cases(third[,2]),]$days_after) + 1

first[,6] <- cumsum(first[,6])
second[,6] <- cumsum(second[,6])
third[,6] <- cumsum(third[,6])

first_graph_G1_D <- first %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = first[c(1,f1),c(6,2)],
            label = first$`Polarity level`[c(1, f1)],
            nudge_y = -0.7)

second_graph_G1_D <- second %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Second Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = second[c(1,s1),c(6,2)],
            label = second$`Polarity level`[c(1, s1)],
            nudge_y = -0.7)

third_graph_G1_D <- third %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Third Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = third[c(1,t1),c(6,2)],
            label = third$`Polarity level`[c(1, t1)],
            nudge_y = -0.7)

first_grobsG1D <- list(first_graph_G1_D, second_graph_G1_D)
second_grobsG1D <- list(third_graph_G1_D)

G1DFINAL <- grid.arrange(top = "G1-D Polarity", ncol=2,
                         grobs = c(lapply(first_grobsG1D, "+", margin_top),
                                   lapply(second_grobsG1D, "+", margin_bottom)))

################################################################################################################

first_H1A <- H1_A[1:9,] %>% mutate(days_after = 0:8)
second_H1A <- H1_A[11:23,] %>% mutate(days_after = 0:12)

y_maxH1A <- max(first_H1A$`Polarity level`, na.rm = T)

f1 <- max(first_H1A$days_after) + 1
s1 <- max(second_H1A[complete.cases(second_H1A),]$days_after) + 1

first_H1A[,6] <- cumsum(first_H1A[,6])
second_H1A[,6] <- cumsum(second_H1A[,6])

first_graph_H1A <- first_H1A %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxH1A)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = first_H1A[c(2,f1),c(6,2)],
            label = first_H1A$`Polarity level`[c(2, f1)],
            nudge_y = -4)

second_graph_H1A <- second_H1A %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxH1A)) +
  labs(subtitle = "Second Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = second_H1A[c(1,s1),c(6,2)],
            label = second_H1A$`Polarity level`[c(1, s1)],
            nudge_y = 3)

grobsH1A <- list(first_graph_H1A, second_graph_H1A)
  
H1AFINAL <- grid.arrange(top = "H1-A Polarity", grobs = lapply(grobsH1A, "+", margin), ncol=2)

################################################################################################################

L1_J_first_session <- L1_J[1:20,] %>% mutate(days_after = 0:19)

y_maxL1J <- max(L1_J_first_session$`Polarity level`, na.rm = T)

f1 <- max(L1_J_first_session$days_after) + 1
#s1 <- max(L1_J_second_session$days_after) + 1

L1_J_first_session[,6] <- cumsum(L1_J_first_session[,6])

L1_J_first_progress <- L1_J_first_session %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxL1J)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = L1_J_first_session[c(1,f1),c(6,2)],
            label = L1_J_first_session$`Polarity level`[c(1, f1)],
            nudge_y = -8)

L1JFINAL <- L1_J_first_progress + labs(title = "L1-J Polarity") + theme(plot.margin=unit(c(1,1,1.2,1.2),"cm"))

###############################################################################################################

first_L2E <- L2_E[1:11,] %>% mutate(days_after = 0:10)
second_L2E <- L2_E[13:25,] %>% mutate(days_after = 0:12)
third_L2E <- L2_E[27:39,] %>% mutate(days_after = 0:12)
fourth_L2E <- L2_E[41:53,] %>% mutate(days_after = 0:12)

y_maxL2E <- max(first_L2E$`Polarity level`, na.rm = T)

f1 <- max(first_L2E$days_after) + 1
s1 <- max(second_L2E$days_after) + 1
t1 <- max(third_L2E$days_after) + 1
fo1 <- max(fourth_L2E$days_after) + 1

first_L2E[,7] <- cumsum(first_L2E[,7])
second_L2E[,7] <- cumsum(second_L2E[,7])
third_L2E[,7] <- cumsum(third_L2E[,7])
fourth_L2E[,7] <- cumsum(fourth_L2E[,7])

first_graph_L2E <- first_L2E %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxL2E)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = first_L2E[c(1,f1),c(7,2)],
            label = first_L2E$`Polarity level`[c(1, f1)],
            nudge_y = -14,
            size = 3.5)

second_graph_L2E <- second_L2E %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxL2E)) +
  labs(subtitle = "Second Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = second_L2E[c(1,s1),c(7,2)],
            label = second_L2E$`Polarity level`[c(1, s1)],
            nudge_y = -14,
            nudge_x = 1,
            size = 3.5)

third_graph_L2E <- third_L2E %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxL2E)) +
  labs(subtitle = "Third Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = third_L2E[c(2,t1),c(7,2)],
            label = third_L2E$`Polarity level`[c(2, t1)],
            nudge_y = 14,
            nudge_x = -1,
            size = 3.5)

fourth_graph_L2E <- fourth_L2E %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxL2E)) +
  labs(subtitle = "Fourth Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = fourth_L2E[c(4,fo1),c(7,2)],
            label = fourth_L2E$`Polarity level`[c(4, fo1)],
            nudge_y = 14,
            size = 3.5)

first_grobsL2E <- list(first_graph_L2E, second_graph_L2E, third_graph_L2E)
second_grobsL2E <- list(fourth_graph_L2E)

L2EFINAL <- grid.arrange(top = "L2-E Polarity", ncol=2,
                         grobs = c(lapply(first_grobsL2E, "+", margin_top),
                                   lapply(second_grobsL2E, "+", margin_bottom)))

################################################################################################################

L3_K_first_session <- L3_K[1:7,] %>% mutate(days_after = 0:6)
L3_K_second_session <- L3_K[9:21,] %>% mutate(days_after = 0:12)

y_maxL3K <- max(L3_K_first_session$`Polarity level`, na.rm = T)

f1 <- max(L3_K_first_session$days_after) + 1
s1 <- max(L3_K_second_session$days_after) + 1

L3_K_first_session[,6] <- cumsum(L3_K_first_session[,6])
L3_K_second_session[,6] <- cumsum(L3_K_second_session[,6])

L3_K_first_progress <- L3_K_first_session %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxL3K)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = L3_K_first_session[c(2,f1),c(6,2)],
            label = L3_K_first_session$`Polarity level`[c(2, f1)],
            nudge_y = -8)

L3_K_second_progress <- L3_K_second_session %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxL3K)) +
  labs(subtitle = "Second Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = L3_K_second_session[c(3,s1),c(6,2)],
            label = L3_K_second_session$`Polarity level`[c(3, s1)],
            nudge_y = -8)

grobsL3K <- list(L3_K_first_progress, L3_K_second_progress)

L3KFINAL <- grid.arrange(top = "L3-K Polarity", grobs = lapply(grobsL3K, "+", margin), ncol=2)

################################################################################################################

totalB1SHours <- c(B1_S_first_session$`Total therapy duration (Hrs)`[nrow(B1_S_first_session)],
                   B1_S_second_session$`Total therapy duration (Hrs)`[nrow(B1_S_second_session)],
                   B1_S_third_session$`Total therapy duration (Hrs)`[nrow(B1_S_third_session)])

totalC1JHours <- c(first_C1_J$`Total therapy duration (Hrs)`[nrow(first_C1_J)],
                   second_C1_J$`Total therapy duration (Hrs)`[nrow(second_C1_J)],
                   third_C1_J$`Total therapy duration (Hrs)`[nrow(third_C1_J)],
                   fourth_C1_J$`Total therapy duration (Hrs)`[nrow(fourth_C1_J)])

totalG1DHours <- c(first_G1D$`Total therapy duration (Hrs)`[nrow(first_G1D)],
                   second_G1D$`Total therapy duration (Hrs)`[nrow(second_G1D)])

totalH1AHours <- c(first_H1A$`Total therapy duration (Hrs)`[nrow(first_H1A)],
                   second_H1A$`Total therapy duration (Hrs)`[nrow(second_H1A)])

totalL1JHours <- L1_J_first_session$`Total therapy duration (Hrs)`[nrow(L1_J_first_session)]

totalL2EHours <- c(first_L2E$`Total therapy duration (Hrs)`[nrow(first_L2E)],
                   second_L2E$`Total therapy duration (Hrs)`[nrow(second_L2E)],
                   third_L2E$`Total therapy duration (Hrs)`[nrow(third_L2E)],
                   fourth_L2E$`Total therapy duration (Hrs)`[nrow(fourth_L2E)])

totalL3KHours <- c(L3_K_first_session$`Total therapy duration (Hrs)`[nrow(L3_K_first_session)],
                   L3_K_second_session$`Total therapy duration (Hrs)`[nrow(L3_K_second_session)])

hours <- list(B1S = totalB1SHours, C1J = totalC1JHours, G1D = totalG1DHours, H1A = totalH1AHours,
           L1J = totalL1JHours, L2E = totalL2EHours, L3K = totalL3KHours)

################################################################################################################

label <- paste("All Patients Therapy Hours ", Sys.Date(), ".pdf", sep = "")
path <- "~/Desktop/PDFs All Patient Therapy Hours"

grid <- list(B1SFINAL, C1JFINAL, G1DFINAL, H1AFINAL, L2EFINAL, L1JFINAL, L3KFINAL)
ggsave(label, marrangeGrob(grid, nrow = 1, ncol = 1, top = NULL), path = path, height = 6, width = 12)
