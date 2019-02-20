library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
library(scales)

B1_S_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Stephanie",
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric"), skip = 2)

first_B1_S <- B1_S_hours[1:17,] %>% mutate(days_after = 0:16)
second_B1_S <- B1_S_hours[19:30,] %>% mutate(days_after = 0:11)
third_B1_S <- B1_S_hours[32:37,] %>% mutate(days_after = 0:5)

y_max1 <- max(first_B1_S$`Polarity level`, na.rm = T)

f1 <- max(first_B1_S[complete.cases(first_B1_S[,2]),]$days_after) + 1
s1 <- max(second_B1_S[complete.cases(second_B1_S[,2]),]$days_after) + 1
t1 <- max(third_B1_S[complete.cases(third_B1_S[,2]),]$days_after) + 1

first_B1_S[,7] <- cumsum(first_B1_S[,7])
second_B1_S[,7] <- cumsum(second_B1_S[,7])
third_B1_S[,7] <- cumsum(third_B1_S[,7])

B1_S_first_progress <- first_B1_S %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = first_B1_S[c(1,f1),c(7,2)],
            label = first_B1_S$`Polarity level`[c(1, f1)],
            nudge_y = -12)

B1_S_second_progress <- second_B1_S %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Second Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = second_B1_S[c(1,s1),c(7,2)],
            label = second_B1_S$`Polarity level`[c(1, s1)],
            nudge_y = 12,
            nudge_x = 0.8)

B1_S_third_progress <- third_B1_S %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Third Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = third_B1_S[c(2,t1),c(7,2)],
            label = third_B1_S$`Polarity level`[c(2, t1)],
            nudge_y = -12)

grid.arrange(top = "B1-S Polarity", B1_S_first_progress, B1_S_second_progress, B1_S_third_progress, ncol=2)

