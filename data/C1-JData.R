library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
C1_J <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Jim",
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric"), skip = 2)

first_C1_J <- C1_J[1:18,] %>% mutate(days_after = 0:17)
second_C1_J <- C1_J[20:32,] %>% mutate(days_after = 0:12)
third_C1_J <- C1_J[34:46,] %>% mutate(days_after = 0:12)

x_max <- max(c(max(first_C1_J$days_after), max(second_C1_J$days_after)))
y_max1 <- max(first_C1_J$`Polarity level`, na.rm = T)

f1 <- max(first_C1_J$days_after) + 1
s1 <- max(second_C1_J$days_after) + 1
t1 <- max(third_C1_J$days_after) + 1

first_graph_C1_J <- first_C1_J %>% select(days_after, `Polarity level`) %>%
  ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(limits = c(0, x_max), breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Days of Treatment", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = first_C1_J[c(1,f1),c(9,2)],
            label = first_C1_J$`Polarity level`[c(1, f1)],
            nudge_y = -9,
            size = 2.9)

second_graph_C1_J <- second_C1_J %>% select(days_after, `Polarity level`) %>%
  ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(limits = c(0, x_max), breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Second Session Results", x = "Days of Treatment", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = second_C1_J[c(2,s1),c(9,2)],
            label = second_C1_J$`Polarity level`[c(2, s1)],
            nudge_y = -8,
            size = 3)

third_graph_C1_J <- third_C1_J %>% select(days_after, `Polarity level`) %>%
  ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(limits = c(0, x_max), breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Third Session Results", x = "Days of Treatment", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = third_C1_J[c(2,s1),c(9,2)],
            label = third_C1_J$`Polarity level`[c(2, s1)],
            nudge_y = -8,
            size = 3)

grid.arrange(top = "C1-J Polarity", first_graph_C1_J, second_graph_C1_J, third_graph_C1_J, ncol=3)
