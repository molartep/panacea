library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
L1_J <- read_excel("~/Desktop/Electronegatividad.xlsx", 
                      sheet = "Jeff", col_types = c("text", "numeric", 
                          "numeric", "numeric", "numeric", "numeric"), skip = 2)

L1_J_first_session <- L1_J[1:20,] %>% mutate(days_after = 0:19)

x_max <- max(L1_J_first_session$days_after)
y_max1 <- max(L1_J_first_session$`Polarity level`, na.rm = T)

f1 <- max(L1_J_first_session$days_after) + 1
#s1 <- max(L1_J_second_session$days_after) + 1

L1_J_first_progress <- L1_J_first_session %>% select(days_after, `Polarity level`) %>%
  ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  scale_x_continuous(limits = c(0, x_max), breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0, y_max1)) +
  labs(subtitle = "First Session Results", x = "Days of Treatment", y = "Polarity Level") +
  geom_smooth(size = 0.5) +
  geom_text(data = L1_J_first_session[c(1,f1),c(7,2)],
            label = L1_J_first_session$`Polarity level`[c(1, f1)],
            nudge_y = -6)

L1_J_first_progress + labs(title = "L1-J Polarity")

#L1_J_second_progress <- L1_J_second_session %>% select(days_after, `Polarity level`) %>%
 # ggplot(aes(x=days_after, y= `Polarity level`)) + geom_point() + 
  #scale_x_continuous(limits = c(0, x_max)) +
  #scale_y_continuous(limits = c(0, y_max1)) +
  #labs(subtitle = "Second Session Results", x = "Days of Treatment", y = "Polarity Level") +
  #geom_smooth(size = 0.5) +
  #geom_text(data = L1_J_first_session[c(1,f1),c(7,2)],
            #label = L1_J_first_session$`Polarity level`[c(1, f1)],
            #nudge_y = -6)


#grid.arrange(top = "L1-J Polarity", L1_J_first_progress, L1_J_second_progress, ncol=2)
