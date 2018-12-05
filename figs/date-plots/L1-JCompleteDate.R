library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(scales)
L1J <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Jeff",
                  col_types = c("date", "numeric", "numeric"))

lims <- as.POSIXct(strptime(c("2018-08-15 01:00","2018-12-15 01:00"), format = "%Y-%m-%d %H:%M"))

L1J_dates <- L1J[,c(1,3)]

L1J_max <- max(L1J$p_level, na.rm = T)

#L1-J DATE GRAPH
L1J_dategraph <- L1J_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, L1J_max)) +
  labs(subtitle = "L1-J", x = "",  y = "") +
  #geom_smooth(size = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


L1J_dategraph