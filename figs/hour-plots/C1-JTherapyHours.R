library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
library(scales)

C1_J <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Jim",
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric"), skip = 2)

first_C1_J <- C1_J[1:18,] %>% mutate(days_after = 0:17)
second_C1_J <- C1_J[20:32,] %>% mutate(days_after = 0:12)
third_C1_J <- C1_J[34:45,] %>% mutate(days_after = 0:11)
fourth_C1_J <- C1_J[47:62,] %>% mutate(days_after = 0:15)
fifth_C1_J <- C1_J[64:75,] %>% mutate(days_after = 0:11)
sixth_C1_J <- C1_J[77:90,] %>% mutate(days_after = 0:13)

y_max1 <- max(first_C1_J$`Polarity level`, na.rm = T)

f1 <- max(first_C1_J[complete.cases(first_C1_J[,2]),]$days_after) + 1
s1 <- max(second_C1_J[complete.cases(second_C1_J[,2]),]$days_after) + 1
t1 <- max(third_C1_J[complete.cases(third_C1_J[,2]),]$days_after) + 1
fo1 <- max(fourth_C1_J[complete.cases(fourth_C1_J[,2]),]$days_after) + 1
fi1 <- max(fifth_C1_J[complete.cases(fifth_C1_J[,2]),]$days_after) + 1
si1 <- max(sixth_C1_J[complete.cases(sixth_C1_J[,2]),]$days_after) + 1

first_C1_J[,8] <- cumsum(first_C1_J[,8])
second_C1_J[,8] <- cumsum(second_C1_J[,8])
third_C1_J[,8] <- cumsum(third_C1_J[,8])
fourth_C1_J[,8] <- cumsum(fourth_C1_J[,8])
fifth_C1_J[,8] <- cumsum(fifth_C1_J[,8])
sixth_C1_J[,8] <- cumsum(sixth_C1_J[,8])

first_graph_C1_J <- first_C1_J %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = first_C1_J[c(1,f1),c(8,2)],
            label = first_C1_J$`Polarity level`[c(1, f1)],
            nudge_y = -18,
            size = 3.5)

second_graph_C1_J <- second_C1_J %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Second Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = second_C1_J[c(2,s1),c(8,2)],
            label = second_C1_J$`Polarity level`[c(2, s1)],
            nudge_y = -18,
            size = 3.5)

third_graph_C1_J <- third_C1_J %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Third Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = third_C1_J[c(2,t1),c(8,2)],
            label = third_C1_J$`Polarity level`[c(2, t1)],
            nudge_y = -18,
            size = 3.5)

fourth_graph_C1_J <- fourth_C1_J %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Fourth Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = fourth_C1_J[c(4,fo1),c(8,2)],
            label = fourth_C1_J$`Polarity level`[c(4, fo1)],
            nudge_y = 18,
            size = 3.5)

fifth_graph_C1_J <- fifth_C1_J %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Fifth Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = fifth_C1_J[c(2,fi1),c(8,2)],
            label = fifth_C1_J$`Polarity level`[c(2, fi1)],
            nudge_y = 18,
            size = 3.5)

sixth_graph_C1_J <- sixth_C1_J %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Sixth Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = sixth_C1_J[c(2,si1),c(8,2)],
            label = sixth_C1_J$`Polarity level`[c(2, si1)],
            nudge_y = 18,
            size = 3.5)

grid.arrange(top = "C1-J Polarity", first_graph_C1_J, second_graph_C1_J, third_graph_C1_J, fourth_graph_C1_J, fifth_graph_C1_J,
             sixth_graph_C1_J, ncol=2)
