library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(scales)
C1J <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Jim", 
                  col_types = c("date", "numeric", "numeric"))

lims <- as.POSIXct(strptime(c("2018-09-15 01:00","2019-02-20 01:00"), format = "%Y-%m-%d %H:%M"))

C1J_dates <- C1J[,c(1,3)]

C1J_max <- max(C1J$p_level, na.rm = T)

#C1-J DATE GRAPH 

C1J_dategraph <- C1J_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, C1J_max)) +
  labs(x = "Date",  y = "Polarity")

C1J_segment1 <- list(x1 = first_C1_J$Day[which.min(first_C1_J$`Polarity level`)], 
                     y1 = first_C1_J$`Polarity level`[which.min(first_C1_J$`Polarity level`)],
                     x2 = second_C1_J$Day[which.max(second_C1_J$`Polarity level`)],
                     y2 = second_C1_J$`Polarity level`[which.max(second_C1_J$`Polarity level`)]) 
C1J_lab1x <- as.POSIXct((as.numeric(C1J_segment1$x1) + as.numeric(C1J_segment1$x2)) / 2, origin = '1970-01-01')
C1J_lab1y <- (C1J_segment1$y1 + C1J_segment1$y2)/2

C1J_segment2 <- list(x1 = second_C1_J$Day[which.min(second_C1_J$`Polarity level`)], 
                     y1 = second_C1_J$`Polarity level`[which.min(second_C1_J$`Polarity level`)],
                     x2 = third_C1_J$Day[which.max(third_C1_J$`Polarity level`)],
                     y2 = third_C1_J$`Polarity level`[which.max(third_C1_J$`Polarity level`)]) 
C1J_lab2x <- as.POSIXct((as.numeric(C1J_segment2$x1) + as.numeric(C1J_segment2$x2)) / 2, origin = '1970-01-01')
C1J_lab2y <- (C1J_segment2$y1 + C1J_segment2$y2)/2

C1J_segment3 <- list(x1 = third_C1_J$Day[which.min(third_C1_J$`Polarity level`)], 
                     y1 = third_C1_J$`Polarity level`[which.min(third_C1_J$`Polarity level`)],
                     x2 = fourth_C1_J$Day[which.max(fourth_C1_J$`Polarity level`)],
                     y2 = fourth_C1_J$`Polarity level`[which.max(fourth_C1_J$`Polarity level`)]) 
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