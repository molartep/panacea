library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(scales)
B1S <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Stephanie",
                  col_types = c("date", "numeric", "numeric"))

lims <- as.POSIXct(strptime(c("2018-08-15 01:00","2018-12-01 01:00"), format = "%Y-%m-%d %H:%M"))

B1S_dates <- B1S[,c(1,3)]

B1S_max <- max(B1S$p_level, na.rm = T)

#B1-S DATE GRAPH
B1S_dategraph <- B1S_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, B1S_max)) +
  labs(subtitle = "B1-S", x = "",  y = "") +
  #geom_smooth(size = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

B1S_segment1 <- list(x1 = B1S_dates$date[20], y1 = B1S_dates$p_level[20],
                     x2 = B1S_dates$date[35], y2 = B1S_dates$p_level[35]) 
B1S_lab1x <- as.POSIXct((as.numeric(B1S_segment1$x1) + as.numeric(B1S_segment1$x2)) / 2, origin = '1970-01-01')
B1S_lab1y <- (B1S_segment1$y1 + B1S_segment1$y2)/2

B1S_segment2 <- list(x1 = B1S_dates$date[47], y1 = B1S_dates$p_level[47],
                     x2 = B1S_dates$date[86], y2 = B1S_dates$p_level[86]) 
B1S_lab2x <- as.POSIXct((as.numeric(B1S_segment2$x1) + as.numeric(B1S_segment2$x2)) / 2, origin = '1970-01-01')
B1S_lab2y <- (B1S_segment2$y1 + B1S_segment2$y2)/2

B1S_dategraph + geom_segment(aes(x = B1S_segment1$x1, y = B1S_segment1$y1,
                                 xend = B1S_segment1$x2, yend = B1S_segment1$y2, color = "14"),
                             linetype = "dashed") +
  geom_text(aes(x = B1S_lab1x, y = B1S_lab1y + 5, color = "14", label = "+ 16.1",
                angle = 22)) + 
  geom_segment(aes(x = B1S_segment2$x1, y = B1S_segment2$y1,
                   xend = B1S_segment2$x2, yend = B1S_segment2$y2, color = "38"),
               linetype = "dashed") +
  geom_text(aes(x = B1S_lab2x, y = B1S_lab2y + 5, color = "38", label = "+ 56.4",
                angle = 29)) + labs(color = "Days Without\nTreatment")