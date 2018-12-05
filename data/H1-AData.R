library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
H1_A <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Anna",
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"), skip = 2)

first <- H1_A[1:9,] %>% mutate(days_after = 0:8)
second <- H1_A[11:23,] %>% mutate(days_after = 0:12)

x_max <- max(c(max(first$days_after), max(second$days_after)))
y_max1 <- max(first$`Polarity level`, na.rm = T)

f1 <- max(first$days_after) + 1
s1 <- max(second[complete.cases(second),]$days_after) + 1

first_graph <- first %>% select(days_after, `Polarity level`) %>%
  ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(limits = c(0, x_max), breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Days of Treatment", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = first[c(2,f1),c(7,2)],
            label = first$`Polarity level`[c(2, f1)],
            nudge_y = -4)

second_graph <- second %>% select(days_after, `Polarity level`) %>%
  ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(limits = c(0, x_max), breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Second Session Results", x = "Days of Treatment", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = second[c(1,s1),c(7,2)],
            label = second$`Polarity level`[c(1, s1)],
            nudge_y = 3)

grid.arrange(top = "H1-A Polarity", first_graph, second_graph, ncol=2)

