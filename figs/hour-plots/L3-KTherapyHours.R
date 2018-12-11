library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
L3_K <- read_excel("~/Desktop/Electronegatividad.xlsx", 
                   sheet = "Kevin", col_types = c("text", "numeric", 
                                                  "numeric", "numeric",
                                                  "numeric", "numeric"), skip = 2)

L3_K_first_session <- L3_K[1:7,] %>% mutate(days_after = 0:6)
L3_K_second_session <- L3_K[9:21,] %>% mutate(days_after = 0:12)

y_maxL3K <- max(L3_K_first_session$`Polarity level`, na.rm = T)

f1 <- max(L3_K_first_session$days_after) + 1
s1 <- max(L3_K_second_session$days_after) + 1

L3_K_first_session[,6] <- cumsum(L3_K_first_session[,6])
L3_K_second_session[,6] <- cumsum(L3_K_second_session[,6])

L3_K_first_progress <- L3_K_first_session %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxL3K)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = L3_K_first_session[c(2,f1),c(6,2)],
            label = L3_K_first_session$`Polarity level`[c(2, f1)],
            nudge_y = -8)

L3_K_second_progress <- L3_K_second_session %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_maxL3K)) +
  labs(subtitle = "Second Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = L3_K_second_session[c(3,s1),c(6,2)],
            label = L3_K_second_session$`Polarity level`[c(3, s1)],
            nudge_y = -8)

grobsL3K <- list(L3_K_first_progress, L3_K_second_progress)

grid.arrange(top = "L3-K Polarity", grobs = grobsL3K, ncol=2)