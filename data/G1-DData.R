library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
G1_D <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Daryl",
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"), skip = 2)

first <- G1_D[1:4,] %>% mutate(days_after = 0:3)
second <- G1_D[6:9,] %>% mutate(days_after = 0:3)

x_max <- max(c(max(first$days_after), max(second$days_after)))
y_max1 <- max(first$`Polarity level`, na.rm = T)

f1 <- max(first$days_after) + 1
s1 <- max(second$days_after) + 1

first_graph <- first %>% select(days_after, `Polarity level`) %>%
  ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(limits = c(0, x_max), breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Days of Treatment", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = first[c(1,f1),c(7,2)],
            label = first$`Polarity level`[c(1, f1)],
            nudge_y = -0.7)

second_graph <- second %>% select(days_after, `Polarity level`) %>%
  ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(limits = c(0, x_max), breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Second Session Results", x = "Days of Treatment", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = second[c(1,s1),c(7,2)],
            label = second$`Polarity level`[c(1, s1)],
            nudge_y = -0.7)


grid.arrange(top = "G1-D Polarity", first_graph, second_graph, ncol=2)
