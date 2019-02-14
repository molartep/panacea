library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
L1_J_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", 
                   sheet = "Jeff", col_types = c("text", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric"), skip = 2)

first_L1_J <- L1_J_hours[1:20,] %>% mutate(days_after = 0:19)

y_max1 <- max(first_L1_J$`Polarity level`, na.rm = T)

f1 <- max(first_L1_J[complete.cases(first_L1_J[,2]),]$days_after) + 1

first_L1_J[,6] <- cumsum(first_L1_J[,6])

first_graph_L1_J <- first_L1_J %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(method = "lm", size = 0.5) +
  geom_text(data = first_L1_J[1, c(6,2)],
            label = first_L1_J$`Polarity level`[1],
            nudge_y = -16) +
  geom_text(data = first_L1_J[f1, c(6,2)],
            label = first_L1_J$`Polarity level`[f1],
            nudge_y = 10)

grid.arrange(top = "L1-J Polarity", first_graph_L1_J, ncol=1)
