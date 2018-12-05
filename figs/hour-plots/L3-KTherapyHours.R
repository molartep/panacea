library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
L3_K <- read_excel("~/Desktop/Electronegatividad.xlsx", 
                   sheet = "Kevin", col_types = c("text", "numeric", 
                                                  "numeric", "numeric",
                                                  "numeric", "numeric"), skip = 2)

L3_K_first_session <- L3_K[1:7,] %>% mutate(days_after = 0:6)

y_max1 <- max(L3_K_first_session$`Polarity level`, na.rm = T)

f1 <- max(L3_K_first_session$days_after) + 1

L3_K_first_session[,6] <- cumsum(L3_K_first_session[,6])

L3_K_first_progress <- L3_K_first_session %>% select(`Total therapy duration (Hrs)`, `Polarity level`) %>%
  ggplot(aes(x=`Total therapy duration (Hrs)`, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Total Therapy Duration (Hrs)", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = L3_K_first_session[c(2,f1),c(6,2)],
            label = L3_K_first_session$`Polarity level`[c(2, f1)],
            nudge_y = -8)

L3_K_first_progress + labs(title = "L3-K Polarity")