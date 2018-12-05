library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(scales)
L2_E <- read_excel("~/Desktop/Electronegatividad.xlsx", 
                              sheet = "Eric", col_types = c("text", "numeric", 
                              "numeric", "numeric", "numeric", "numeric"), skip = 2)

first <- L2_E[1:11,] %>% mutate(days_after = 0:10)
second <- L2_E[13:25,] %>% mutate(days_after = 0:12)
third <- L2_E[27:39,] %>% mutate(days_after = 0:12)

x_max <- max(c(max(first$days_after), max(second$days_after), max(third$days_after)))
y_max1 <- max(first$`Polarity level`, na.rm = T)

f1 <- max(first$days_after) + 1
s1 <- max(second$days_after) + 1
t1 <- max(third$days_after) + 1

first_graph <- first %>% select(days_after, `Polarity level`) %>%
  ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(limits = c(0, x_max), breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Days of Treatment", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = first[c(1,f1),c(7,2)],
            label = first$`Polarity level`[c(1, f1)],
            nudge_y = -6)

second_graph <- second %>% select(days_after, `Polarity level`) %>%
  ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(limits = c(0, x_max), breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Second Session Results", x = "Days of Treatment", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = second[c(1,s1),c(7,2)],
            label = second$`Polarity level`[c(1, s1)],
            nudge_y = 6)
  
third_graph <- third %>% select(days_after, `Polarity level`) %>%
  ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(limits = c(0, x_max), breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Third Session Results", x = "Days of Treatment", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = third[c(2,t1),c(7,2)],
            label = third$`Polarity level`[c(2, t1)],
            nudge_y = 6)

grid.arrange(top = "L2-E Polarity", first_graph, second_graph, third_graph, ncol=3)

