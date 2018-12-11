library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
G1_D <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Daryl",
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"), skip = 2)

first <- G1_D[1:4,] %>% mutate(days_after = 0:3)
second <- G1_D[6:9,] %>% mutate(days_after = 0:3)
third <- G1_D[11:13,] %>% mutate(days_after = 0:2)

y_max1 <- max(first$`Polarity level`, na.rm = T)

f1 <- max(first$days_after) + 1
s1 <- max(second$days_after) + 1
t1 <- max(third$days_after) + 1

first[,6] <- cumsum(first[,6])
second[,6] <- cumsum(second[,6])
third[,6] <- cumsum(third[,6])

first_graph <- first %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = first[c(1,f1),c(6,2)],
            label = first$`Polarity level`[c(1, f1)],
            nudge_y = -0.7)

second_graph <- second %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Second Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = second[c(1,s1),c(6,2)],
            label = second$`Polarity level`[c(1, s1)],
            nudge_y = -0.7)

third_graph <- third %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Third Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = third[c(1,t1),c(6,2)],
            label = third$`Polarity level`[c(1, t1)],
            nudge_y = -0.7)


grid.arrange(top = "G1-D Polarity", first_graph, second_graph, third_graph, ncol=2)
