library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(scales)
L3K <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Kevin", 
                  col_types = c("date", "numeric", "numeric"))

lims <- as.POSIXct(strptime(c("2018-08-15 01:00","2018-12-15 01:00"), format = "%Y-%m-%d %H:%M"))

L3K_dates <- L3K[,c(1,3)]

L3K_max <- max(L3K$p_level, na.rm = T)

#L3-K DATE GRAPH
L3K_dategraph <- L3K_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, L3K_max)) +
  labs(subtitle = "L3-K", x = "",  y = "") +
  #geom_smooth(size = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

L3K_segment1 <- list(x1 = L3K_dates$date[7], y1 = L3K_dates$p_level[7],
                     x2 = L3K_dates$date[38], y2 = L3K_dates$p_level[38]) 
L3K_lab1x <- as.POSIXct((as.numeric(L3K_segment1$x1) + as.numeric(L3K_segment1$x2)) / 2, origin = '1970-01-01')
L3K_lab1y <- (L3K_segment1$y1 + L3K_segment1$y2)/2

L3K_dategraph + geom_segment(aes(x = L3K_segment1$x1, y = L3K_segment1$y1,
                                 xend = L3K_segment1$x2, yend = L3K_segment1$y2, color = "30"),
                             linetype = "dashed") +
  geom_text(aes(x = L3K_lab1x, y = L3K_lab1y + 5, color = "30", label = "+ 11.7",
                angle = 5), size = 4.5) + labs(color = "Days Without\nTreatment")