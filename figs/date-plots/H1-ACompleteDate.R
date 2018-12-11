library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(scales)
H1A <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Anna", 
                  col_types = c("date", "numeric", "numeric"))

lims <- as.POSIXct(strptime(c("2018-08-15 01:00","2018-12-15 01:00"), format = "%Y-%m-%d %H:%M"))

H1A_dates <- H1A[,c(1,3)]

H1A_max <- max(H1A$p_level, na.rm = T)

#H1-A DATE GRAPH
H1A_dategraph <- H1A_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, H1A_max)) +
  labs(subtitle = "H1-A", x = "",  y = "") +
  #geom_smooth(size = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

H1A_segment1 <- list(x1 = H1A_dates$date[11], y1 = H1A_dates$p_level[11],
                     x2 = H1A_dates$date[33], y2 = H1A_dates$p_level[33]) 
H1A_lab1x <- as.POSIXct((as.numeric(H1A_segment1$x1) + as.numeric(H1A_segment1$x2)) / 2, origin = '1970-01-01')
H1A_lab1y <- (H1A_segment1$y1 + H1A_segment1$y2)/2

H1A_segment2 <- list(x1 = H1A_dates$date[46], y1 = H1A_dates$p_level[46],
                     x2 = H1A_dates$date[72], y2 = H1A_dates$p_level[72]) 
H1A_lab2x <- as.POSIXct((as.numeric(H1A_segment2$x1) + as.numeric(H1A_segment2$x2)) / 2, origin = '1970-01-01')
H1A_lab2y <- (H1A_segment2$y1 + H1A_segment2$y2)/2

H1A_segment3 <- list(x1 = H1A_dates$date[72], y1 = H1A_dates$p_level[72],
                     x2 = H1A_dates$date[111], y2 = H1A_dates$p_level[111]) 
H1A_lab3x <- as.POSIXct((as.numeric(H1A_segment3$x1) + as.numeric(H1A_segment3$x2)) / 2, origin = '1970-01-01')
H1A_lab3y <- (H1A_segment3$y1 + H1A_segment3$y2)/2

H1A_dategraph + geom_segment(aes(x = H1A_segment1$x1, y = H1A_segment1$y1,
                                 xend = H1A_segment1$x2, yend = H1A_segment1$y2, color = "22"),
                             linetype = "dashed") +
  geom_text(aes(x = H1A_lab1x, y = H1A_lab1y + 2, color = "22", label = "+ 1.9",
                angle = 4)) + 
  geom_segment(aes(x = H1A_segment2$x1, y = H1A_segment2$y1,
                   xend = H1A_segment2$x2, yend = H1A_segment2$y2, color = "25"),
               linetype = "dashed") +
  geom_text(aes(x = H1A_lab2x, y = H1A_lab2y + 2, color = "25", label = "+ 2",
                angle = 4)) +
  geom_segment(aes(x = H1A_segment3$x1, y = H1A_segment3$y1,
                 xend = H1A_segment3$x2, yend = H1A_segment3$y2, color = "38"),
             linetype = "dashed") +
  geom_text(aes(x = H1A_lab3x, y = H1A_lab3y + 2, color = "38", label = "+ 8.9",
                angle = 4)) + labs(color = "Days Without\nTreatment")