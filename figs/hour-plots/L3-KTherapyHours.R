library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
library(scales)

L3_K_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", 
                   sheet = "Kevin", col_types = c("date", "numeric", 
                                                  "numeric", "numeric",
                                                  "numeric", "numeric"), skip = 2)

first_L3_K <- L3_K_hours[1:7,] %>% mutate(days_after = 0:6)
second_L3_K <- L3_K_hours[9:20,] %>% mutate(days_after = 0:11)
third_L3_K <- L3_K_hours[22:28,] %>% mutate(days_after = 0:6)
fourth_L3_K <- L3_K_hours[30:34,] %>% mutate(days_after = 0:4)
fifth_L3_K <- L3_K_hours[36:42,] %>% mutate(days_after = 0:6)
sixth_L3_K <- L3_K_hours[44:50,] %>% mutate(days_after = 0:6)

y_max1 <- max(first_L3_K$`Polarity level`, na.rm = T)

f1 <- max(first_L3_K[complete.cases(first_L3_K[,2]),]$days_after) + 1
s1 <- max(second_L3_K[complete.cases(second_L3_K[,2]),]$days_after) + 1
t1 <- max(third_L3_K[complete.cases(third_L3_K[,2]),]$days_after) + 1
fo1 <- max(fourth_L3_K[complete.cases(fourth_L3_K[,2]),]$days_after) + 1
fi1 <- max(fifth_L3_K[complete.cases(fifth_L3_K[,2]),]$days_after) + 1
si1 <- max(sixth_L3_K[complete.cases(sixth_L3_K[,2]),]$days_after) + 1

first_L3_K[,6] <- cumsum(first_L3_K[,6])
second_L3_K[,6] <- cumsum(second_L3_K[,6])
third_L3_K[,6] <- cumsum(third_L3_K[,6])
fourth_L3_K[,6] <- cumsum(fourth_L3_K[,6])
fifth_L3_K[,6] <- cumsum(fifth_L3_K[,6])
sixth_L3_K[,6] <- cumsum(sixth_L3_K[,6])

first_graph_L3_K <- first_L3_K %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = first_L3_K[c(2,f1),c(6,2)],
            label = first_L3_K$`Polarity level`[c(2, f1)],
            nudge_y = -18,
            size = 3.5)

second_graph_L3_K <- second_L3_K %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Second Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = second_L3_K[c(3,s1),c(6,2)],
            label = second_L3_K$`Polarity level`[c(3, s1)],
            nudge_y = -18,
            size = 3.5)

third_graph_L3_K <- third_L3_K %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Third Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = third_L3_K[c(2,t1),c(6,2)],
            label = third_L3_K$`Polarity level`[c(2, t1)],
            nudge_y = -18,
            size = 3.5)

fourth_graph_L3_K <- fourth_L3_K %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Fourth Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = fourth_L3_K[c(2,fo1),c(6,2)],
            label = fourth_L3_K$`Polarity level`[c(2, fo1)],
            nudge_y = 18,
            size = 3.5)

fifth_graph_L3_K <- fifth_L3_K %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Fifth Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = fifth_L3_K[c(2,fi1),c(6,2)],
            label = fifth_L3_K$`Polarity level`[c(2, fi1)],
            nudge_y = 18,
            size = 3.5)

sixth_graph_L3_K <- sixth_L3_K %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Sixth Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = sixth_L3_K[c(1,si1),c(6,2)],
            label = sixth_L3_K$`Polarity level`[c(1, si1)],
            nudge_y = 18,
            size = 3.5)


grid.arrange(top = "L3-K Polarity", first_graph_L3_K, second_graph_L3_K, third_graph_L3_K, fourth_graph_L3_K, fifth_graph_L3_K, sixth_graph_L3_K, ncol=2)
