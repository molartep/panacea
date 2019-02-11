library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(scales)
L2_E <- read_excel("~/Desktop/Electronegatividad.xlsx", 
                   sheet = "Eric", col_types = c("text", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", "numeric"), skip = 2)

first_L2_E <- L2_E[1:11,] %>% mutate(days_after = 0:10)
second_L2_E <- L2_E[13:25,] %>% mutate(days_after = 0:12)
third_L2_E <- L2_E[27:39,] %>% mutate(days_after = 0:12)
fourth_L2_E <- L2_E[41:52,] %>% mutate(days_after = 0:11)
fifth_L2_E <- L2_E[54:65,] %>% mutate(days_after = 0:11)

y_maxL2E <- max(first_L2_E$`Polarity level`, na.rm = T)

f1 <- max(first_L2_E[complete.cases(first_L2_E[,2]),]$days_after) + 1
s1 <- max(second_L2_E[complete.cases(second_L2_E[,2]),]$days_after) + 1
t1 <- max(third_L2_E[complete.cases(third_L2_E[,2]),]$days_after) + 1
fo1 <- max(fourth_L2_E[complete.cases(fourth_L2_E[,2]),]$days_after) + 1
fi1 <- max(fifth_L2_E[complete.cases(fifth_L2_E[,2]),]$days_after) + 1

first_L2_E[,7] <- cumsum(first_L2_E[,7])
second_L2_E[,7] <- cumsum(second_L2_E[,7])
third_L2_E[,7] <- cumsum(third_L2_E[,7])
fourth_L2_E[,7] <- cumsum(fourth_L2_E[,7])
fifth_L2_E[,7] <- cumsum(fifth_L2_E[,7])

first_graph_L2E <- first_L2_E %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxL2E)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = first_L2_E[c(1,f1),c(7,2)],
            label = first_L2_E$`Polarity level`[c(1, f1)],
            nudge_y = -14,
            size = 3.5)

second_graph_L2E <- second_L2_E %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxL2E)) +
  labs(subtitle = "Second Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = second_L2_E[c(1,s1),c(7,2)],
            label = second_L2_E$`Polarity level`[c(1, s1)],
            nudge_y = 14,
            nudge_x = 1,
            size = 3.5)

third_graph_L2E <- third_L2_E %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxL2E)) +
  labs(subtitle = "Third Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = third_L2_E[c(2,t1),c(7,2)],
            label = third_L2_E$`Polarity level`[c(2, t1)],
            nudge_y = 14,
            size = 3.5)

fourth_graph_L2E <- fourth_L2_E %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxL2E)) +
  labs(subtitle = "Fourth Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = fourth_L2_E[c(4,fo1),c(7,2)],
            label = fourth_L2_E$`Polarity level`[c(4, fo1)],
            nudge_y = 14,
            size = 3.5)

grid.arrange(top = "L2-E Polarity", first_graph_L2E, second_graph_L2E, third_graph_L2E, fourth_graph_L2E, ncol=2)
