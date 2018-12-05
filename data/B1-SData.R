library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
B1_S <- read_excel("~/Desktop/Electronegatividad.xlsx", 
                                col_types = c("text", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric"), skip = 2)

B1_S_first_session <- B1_S[1:17,] %>% mutate(days_after = 0:16)
B1_S_second_session <- B1_S[19:30,] %>% mutate(days_after = 0:11)
B1_S_third_session <- B1_S[32:37,] %>% mutate(days_after = 0:5)

x_max <- max(c(max(B1_S_first_session$days_after), max(B1_S_second_session$days_after)))
y_max1 <- max(B1_S_first_session$`Polarity level`, na.rm = T)

f1 <- max(B1_S_first_session$days_after) + 1
s1 <- max(B1_S_second_session$days_after) + 1
t1 <- max(B1_S_third_session$days_after)

B1_S_first_progress <- B1_S_first_session %>% select(days_after, `Polarity level`) %>%
  ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(limits = c(0, x_max), breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Days of Treatment", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = B1_S_first_session[c(1,f1),c(8,2)],
            label = B1_S_first_session$`Polarity level`[c(1, f1)],
            nudge_y = -6)

B1_S_second_progress <- B1_S_second_session %>% select(days_after, `Polarity level`) %>%
  ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(limits = c(0, x_max), breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Second Session Results", x = "Days of Treatment", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = B1_S_second_session[c(1,s1),c(8,2)],
            label = B1_S_second_session$`Polarity level`[c(1, s1)],
            nudge_y = 5,
            nudge_x = 0.8)

B1_S_third_progress <- B1_S_third_session %>% select(days_after, `Polarity level`) %>%
  ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(limits = c(0, x_max), breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "Third Session Results", x = "Days of Treatment", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = B1_S_third_session[c(2,t1),c(8,2)],
            label = B1_S_third_session$`Polarity level`[c(2, t1)],
            nudge_y = -6)

grid.arrange(top = "B1-S Polarity", B1_S_first_progress, B1_S_second_progress, B1_S_third_progress, ncol=3)

