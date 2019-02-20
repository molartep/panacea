library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
library(scales)

G1_D <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Daryl",
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"), skip = 2)


first_G1_D <- G1_D_hours[1:4,] %>% mutate(days_after = 0:3)
second_G1_D <- G1_D_hours[6:9,] %>% mutate(days_after = 0:3)
third_G1_D <- G1_D_hours[11:14,] %>% mutate(days_after = 0:3)
fourth_G1_D <- G1_D_hours[16:19,] %>% mutate(days_after = 0:3)

y_max1 <- max(first_G1_D$`Polarity level`, na.rm = T)

f1 <- max(first_G1_D[complete.cases(first_G1_D[,2]),]$days_after) + 1
s1 <- max(second_G1_D[complete.cases(second_G1_D[,2]),]$days_after) + 1
t1 <- max(third_G1_D[complete.cases(third_G1_D[,2]),]$days_after) + 1
fo1 <- max(fourth_G1_D[complete.cases(fourth_G1_D[,2]),]$days_after) + 1

first_G1_D[,6] <- cumsum(first_G1_D[,6])
second_G1_D[,6] <- cumsum(second_G1_D[,6])
third_G1_D[,6] <- cumsum(third_G1_D[,6])
fourth_G1_D[,6] <- cumsum(fourth_G1_D[,6])

first_graph_G1_D <- first_G1_D %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = first_G1_D[c(1,f1),c(6,2)],
            label = first_G1_D$`Polarity level`[c(1, f1)],
            nudge_y = -0.6,
            size = 3.5)

second_graph_G1_D <- second_G1_D %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Second Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = second_G1_D[c(1,s1),c(6,2)],
            label = second_G1_D$`Polarity level`[c(1, s1)],
            nudge_y = -0.6,
            size = 3.5)

third_graph_G1_D <- third_G1_D %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Third Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = third_G1_D[c(1,t1),c(6,2)],
            label = third_G1_D$`Polarity level`[c(1, t1)],
            nudge_y = 0.6,
            size = 3.5)

fourth_graph_G1_D <- fourth_G1_D %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Fourth Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = fourth_G1_D[c(2,fo1),c(6,2)],
            label = fourth_G1_D$`Polarity level`[c(2, fo1)],
            nudge_y = 0.6,
            size = 3.5)

grid.arrange(top = "G1-D Polarity", first_graph_G1_D, second_graph_G1_D, third_graph_G1_D, fourth_graph_G1_D, ncol=2)