library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(scales)
C1J <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Jim", 
                  col_types = c("date", "numeric", "numeric"))

lims <- as.POSIXct(strptime(c("2018-08-15 01:00","2018-12-15 01:00"), format = "%Y-%m-%d %H:%M"))

C1J_dates <- C1J[,c(1,3)]

C1J_max <- max(C1J$p_level, na.rm = T)

#C1-J DATE GRAPH
C1J_dategraph <- C1J_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, C1J_max)) +
  labs(subtitle = "C1-J", x = "",  y = "") +
  #geom_smooth(size = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

C1J_segment1 <- list(x1 = C1J_dates$date[20], y1 = C1J_dates$p_level[20],
                     x2 = C1J_dates$date[30], y2 = C1J_dates$p_level[30]) 
C1J_lab1x <- as.POSIXct((as.numeric(C1J_segment1$x1) + as.numeric(C1J_segment1$x2)) / 2, origin = '1970-01-01')
C1J_lab1y <- (C1J_segment1$y1 + C1J_segment1$y2)/2

C1J_segment2 <- list(x1 = C1J_dates$date[42], y1 = C1J_dates$p_level[42],
                     x2 = C1J_dates$date[52], y2 = C1J_dates$p_level[52]) 
C1J_lab2x <- as.POSIXct((as.numeric(C1J_segment2$x1) + as.numeric(C1J_segment2$x2)) / 2, origin = '1970-01-01')
C1J_lab2y <- (C1J_segment2$y1 + C1J_segment2$y2)/2

C1J_segment3 <- list(x1 = C1J_dates$date[63], y1 = C1J_dates$p_level[63],
                     x2 = C1J_dates$date[74], y2 = C1J_dates$p_level[74]) 
C1J_lab3x <- as.POSIXct((as.numeric(C1J_segment3$x1) + as.numeric(C1J_segment3$x2)) / 2, origin = '1970-01-01')
C1J_lab3y <- (C1J_segment3$y1 + C1J_segment3$y2)/2

C1J_dategraph + geom_segment(aes(x = C1J_segment1$x1, y = C1J_segment1$y1,
                                 xend = C1J_segment1$x2, yend = C1J_segment1$y2, color = "9"),
                             linetype = "dashed") +
  geom_text(aes(x = C1J_lab1x, y = C1J_lab1y + 5, color = "9", label = "+ 18.1",
                angle = 29)) + 
  geom_segment(aes(x = C1J_segment2$x1, y = C1J_segment2$y1,
                   xend = C1J_segment2$x2, yend = C1J_segment2$y2, color = "9 "),
               linetype = "dashed") +
  geom_text(aes(x = C1J_lab2x, y = C1J_lab2y + 5, color = "9 ", label = "+ 19.1",
                angle = 30)) +
  geom_segment(aes(x = C1J_segment3$x1, y = C1J_segment3$y1,
                   xend = C1J_segment3$x2, yend = C1J_segment3$y2, color = "10"),
               linetype = "dashed") +
  geom_text(aes(x = C1J_lab3x, y = C1J_lab3y + 5, color = "10", label = "+ 1.1",
                angle = 1)) + labs(color = "Days Without\nTreatment")