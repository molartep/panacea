library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
library(scales)

H1_A_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Anna",
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"), skip = 2)

first_H1_A_hours <- H1_A_hours[1:9,] %>% mutate(days_after = 0:8)
second_H1_A_hours <- H1_A_hours[11:23,] %>% mutate(days_after = 0:12)
third_H1_A_hours <- H1_A_hours[25:26,] %>% mutate(days_after = 0:1)
fourth_H1_A_hours <- H1_A_hours[28:39,] %>% mutate(days_after = 0:11)
fifth_H1_A_hours <- H1_A_hours[41:52,] %>% mutate(days_after = 0:11)
sixth_H1_A_hours <- H1_A_hours[54:58,] %>% mutate(days_after = 0:4)

y_max1 <- max(first_H1_A_hours$`Polarity level`, na.rm = T)

f1 <- max(first_H1_A_hours[complete.cases(first_H1_A_hours[,2]),]$days_after) + 1
s1 <- max(second_H1_A_hours[complete.cases(second_H1_A_hours[,2]),]$days_after) + 1
t1 <- max(third_H1_A_hours[complete.cases(third_H1_A_hours[,2]),]$days_after) + 1
fo1 <- max(fourth_H1_A_hours[complete.cases(fourth_H1_A_hours[,2]),]$days_after) + 1
fi1 <- max(fifth_H1_A_hours[complete.cases(fifth_H1_A_hours[,2]),]$days_after) + 1
si1 <- max(sixth_H1_A_hours[complete.cases(sixth_H1_A_hours[,2]),]$days_after) + 1

first_H1_A_hours[,6] <- cumsum(first_H1_A_hours[,6])
second_H1_A_hours[,6] <- cumsum(second_H1_A_hours[,6])
third_H1_A_hours[,6] <- cumsum(third_H1_A_hours[,6])
fourth_H1_A_hours[,6] <- cumsum(fourth_H1_A_hours[,6])
fifth_H1_A_hours[,6] <- cumsum(fifth_H1_A_hours[,6])
sixth_H1_A_hours[,6] <- cumsum(sixth_H1_A_hours[,6])

first_graph_H1A <- first_H1_A_hours %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = first_H1_A_hours[2,c(6,2)],
            label = first_H1_A_hours$`Polarity level`[2],
            nudge_y = -8,
            size = 3.5) +
  geom_text(data = first_H1_A_hours[f1,c(6,2)],
            label = first_H1_A_hours$`Polarity level`[f1],
            nudge_y = 8,
            size = 3.5)

second_graph_H1A <- second_H1_A_hours %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Second Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = second_H1_A_hours[c(1,s1),c(6,2)],
            label = second_H1_A_hours$`Polarity level`[c(1, s1)],
            nudge_y = 8,
            size = 3.5)

third_graph_H1A <- third_H1_A_hours %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Third Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = third_H1_A_hours[c(2,t1),c(6,2)],
            label = third_H1_A_hours$`Polarity level`[c(2, t1)],
            nudge_y = 8,
            size = 3.5)

fourth_graph_H1A <- fourth_H1_A_hours %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Fourth Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = fourth_H1_A_hours[c(7,fo1),c(6,2)],
            label = fourth_H1_A_hours$`Polarity level`[c(7, fo1)],
            nudge_y = 8,
            size = 3.5)

fifth_graph_H1A <- fifth_H1_A_hours %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Fifth Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = fifth_H1_A_hours[c(2,fi1),c(6,2)],
            label = fifth_H1_A_hours$`Polarity level`[c(2, fi1)],
            nudge_y = 8,
            size = 3.5)

sixth_graph_H1A <- sixth_H1_A_hours %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Sixth Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = sixth_H1_A_hours[c(2,si1),c(6,2)],
            label = sixth_H1_A_hours$`Polarity level`[c(2, si1)],
            nudge_y = 8,
            size = 3.5)

grid.arrange(top = "H1-A Polarity", first_graph_H1A, second_graph_H1A, third_graph_H1A, fourth_graph_H1A, fifth_graph_H1A, sixth_graph_H1A, ncol=2)

