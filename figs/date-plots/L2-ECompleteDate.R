library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(scales)
L2E <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Eric", 
                  col_types = c("date", "numeric", "numeric"))

lims <- as.POSIXct(strptime(c("2018-08-15 01:00","2018-12-01 01:00"), format = "%Y-%m-%d %H:%M"))

L2E_dates <- L2E[,c(1,3)]

L2E_max <- max(L2E$p_level, na.rm = T)

#L2-E DATE GRAPH
L2E_dategraph <- L2E_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, L2E_max)) +
  labs(subtitle = "L2-E", x = "",  y = "") +
  #geom_smooth(size = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

L2E_segment1 <- list(x1 = L2E_dates$date[13], y1 = L2E_dates$p_level[13],
                     x2 = L2E_dates$date[35], y2 = L2E_dates$p_level[35]) 
L2E_lab1x <- as.POSIXct((as.numeric(L2E_segment1$x1) + as.numeric(L2E_segment1$x2)) / 2, origin = '1970-01-01')
L2E_lab1y <- (L2E_segment1$y1 + L2E_segment1$y2)/2

L2E_segment2 <- list(x1 = L2E_dates$date[48], y1 = L2E_dates$p_level[48],
                     x2 = L2E_dates$date[64], y2 = L2E_dates$p_level[64]) 
L2E_lab2x <- as.POSIXct((as.numeric(L2E_segment2$x1) + as.numeric(L2E_segment2$x2)) / 2, origin = '1970-01-01')
L2E_lab2y <- (L2E_segment2$y1 + L2E_segment2$y2)/2

L2E_dategraph + geom_segment(aes(x = L2E_segment1$x1, y = L2E_segment1$y1,
                                 xend = L2E_segment1$x2, yend = L2E_segment1$y2, color = "22"),
                             linetype = "dashed") +
  geom_text(aes(x = L2E_lab1x, y = L2E_lab1y + 5, color = "22", label = "+ 11.9",
                angle = 10)) + 
  geom_segment(aes(x = L2E_segment2$x1, y = L2E_segment2$y1,
                   xend = L2E_segment2$x2, yend = L2E_segment2$y2, color = "15"),
               linetype = "dashed") +
  geom_text(aes(x = L2E_lab2x, y = L2E_lab2y + 5, color = "15", label = "+ 8.6",
                angle = 9)) + labs(color = "Days Without\nTreatment")