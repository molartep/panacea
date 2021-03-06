library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
library(scales)
C1_J_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Jim", 
                  col_types = c("date", "numeric", "numeric"))
C1_J_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Jim",
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric"), skip = 2)

first_C1_J <- C1_J_hours[1:18,] 
second_C1_J <- C1_J_hours[20:32,]
third_C1_J <- C1_J_hours[34:45,]
fourth_C1_J <- C1_J_hours[47:62,]
fifth_C1_J <- C1_J_hours[64:75,]
sixth_C1_J <- C1_J_hours[77:90,]

first_C1_J_dates <- C1_J_dates[1:20,]
second_C1_J_dates <- C1_J_dates[30:42,]
third_C1_J_dates <- C1_J_dates[52:63,]
fourth_C1_J_dates <- C1_J_dates[74:88,]
fifth_C1_J_dates <- C1_J_dates[135:145,]
sixth_C1_J_dates <- C1_J_dates[170:182,]

lims <- as.POSIXct(strptime(c("2018-09-15 01:00","2019-04-10 01:00"), format = "%Y-%m-%d %H:%M"))

C1J_dates <- C1_J_dates[,c(1,3)]


C1J_max <- max(C1_J_dates$p_level, na.rm = T)

#C1-J DATE GRAPH 

C1J_dategraph <- C1J_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, C1J_max)) +
  labs(x = "Date",  y = "Polarity")

C1J_segment1 <- list(x1 = first_C1_J_dates$date[which.min(first_C1_J_dates$p_level)], 
                     y1 = first_C1_J_dates$p_level[which.min(first_C1_J_dates$p_level)],
                     x2 = second_C1_J_dates$date[which.max(second_C1_J_dates$p_level)],
                     y2 = second_C1_J_dates$p_level[which.max(second_C1_J_dates$p_level)]) 
C1J_lab1x <- as.POSIXct((as.numeric(C1J_segment1$x1) + as.numeric(C1J_segment1$x2)) / 2, origin = '1970-01-01')
C1J_lab1y <- (C1J_segment1$y1 + C1J_segment1$y2)/2

C1J_segment2 <- list(x1 = second_C1_J_dates$date[which.min(second_C1_J_dates$p_level)], 
                     y1 = second_C1_J_dates$p_level[which.min(second_C1_J_dates$p_level)],
                     x2 = third_C1_J_dates$date[which.max(third_C1_J_dates$p_level)],
                     y2 = third_C1_J_dates$p_level[which.max(third_C1_J_dates$p_level)]) 
C1J_lab2x <- as.POSIXct((as.numeric(C1J_segment2$x1) + as.numeric(C1J_segment2$x2)) / 2, origin = '1970-01-01')
C1J_lab2y <- (C1J_segment2$y1 + C1J_segment2$y2)/2

C1J_segment3 <- list(x1 = third_C1_J_dates$date[which.min(third_C1_J_dates$p_level)], 
                     y1 = third_C1_J_dates$p_level[which.min(third_C1_J_dates$p_level)],
                     x2 = fourth_C1_J_dates$date[which.max(fourth_C1_J_dates$p_level)],
                     y2 = fourth_C1_J_dates$p_level[which.max(fourth_C1_J_dates$p_level)]) 
C1J_lab3x <- as.POSIXct((as.numeric(C1J_segment3$x1) + as.numeric(C1J_segment3$x2)) / 2, origin = '1970-01-01')
C1J_lab3y <- (C1J_segment3$y1 + C1J_segment3$y2)/2

C1J_segment4 <- list(x1 = fourth_C1_J_dates$date[which.min(fourth_C1_J_dates$p_level)], 
                     y1 = fourth_C1_J_dates$p_level[which.min(fourth_C1_J_dates$p_level)],
                     x2 = fifth_C1_J_dates$date[which.max(fifth_C1_J_dates$p_level)],
                     y2 = fifth_C1_J_dates$p_level[which.max(fifth_C1_J_dates$p_level)]) 
C1J_lab4x <- as.POSIXct((as.numeric(C1J_segment4$x1) + as.numeric(C1J_segment4$x2)) / 2, origin = '1970-01-01')
C1J_lab4y <- (C1J_segment4$y1 + C1J_segment4$y2)/2

C1J_segment5 <- list(x1 = fifth_C1_J_dates$date[which.min(fifth_C1_J_dates$p_level)], 
                     y1 = fifth_C1_J_dates$p_level[which.min(fifth_C1_J_dates$p_level)],
                     x2 = sixth_C1_J_dates$date[which.max(sixth_C1_J_dates$p_level)],
                     y2 = sixth_C1_J_dates$p_level[which.max(sixth_C1_J_dates$p_level)]) 
C1J_lab5x <- as.POSIXct((as.numeric(C1J_segment5$x1) + as.numeric(C1J_segment5$x2)) / 2, origin = '1970-01-01')
C1J_lab5y <- (C1J_segment5$y1 + C1J_segment5$y2)/2

C1J_first_day <- as.POSIXct(first_C1_J$Day[which(first_C1_J$`Total therapy duration (Hrs)`>0)][1],
           origin = '1970-01-01')
C1J_last_day <- as.POSIXct(tail(sixth_C1_J$Day[which(sixth_C1_J$`Total therapy duration (Hrs)`>0)],1),
           origin = '1970-01-01')

C1J_label_first <- C1J_first_day %>% format(., "%B %d %Y")
C1J_label_last <- C1J_last_day %>% format(., "%B %d %Y")

C1J_final_date_graph <- C1J_dategraph + geom_text(aes(x = C1J_first_day, y = first_C1_J$`Polarity level`[which(first_C1_J$`Polarity level`>0)][1],
                                      label = C1J_label_first), hjust = -0.1, vjust = 0, size = 3.5) +
                         geom_text(aes(x = C1J_last_day, y = tail(sixth_C1_J$`Polarity level`[which(sixth_C1_J$`Polarity level`>0)],1),
                                      label = C1J_label_last), hjust = -0.1, vjust = 0, size = 3.5) +
  geom_segment(aes(x = C1J_segment1$x1, y = C1J_segment1$y1,
                                 xend = C1J_segment1$x2, yend = C1J_segment1$y2, color = "9"),
                             linetype = "dashed") +
  geom_text(aes(x = C1J_lab1x, y = C1J_lab1y + 5, color = "9", label = "+ 18.1",
                angle = 36)) + 
  geom_segment(aes(x = C1J_segment2$x1, y = C1J_segment2$y1,
                   xend = C1J_segment2$x2, yend = C1J_segment2$y2, color = "9 "),
               linetype = "dashed") +
  geom_text(aes(x = C1J_lab2x, y = C1J_lab2y + 5, color = "9 ", label = "+ 19.1",
                angle = 39)) +
  geom_segment(aes(x = C1J_segment3$x1, y = C1J_segment3$y1,
                   xend = C1J_segment3$x2, yend = C1J_segment3$y2, color = "10"),
               linetype = "dashed") +
  geom_text(aes(x = C1J_lab3x, y = C1J_lab3y + 5, color = "10", label = "+ 1.1",
                angle = 1)) + 
  geom_segment(aes(x = C1J_segment4$x1, y = C1J_segment4$y1,
                   xend = C1J_segment4$x2, yend = C1J_segment4$y2, color = "46"),
               linetype = "dashed") +
  geom_text(aes(x = C1J_lab4x, y = C1J_lab4y + 5, color = "46", label = "+ 26.7",
                angle = 14)) +
  geom_segment(aes(x = C1J_segment5$x1, y = C1J_segment5$y1,
                 xend = C1J_segment5$x2, yend = C1J_segment5$y2, color = "24"),
             linetype = "dashed") +
  geom_text(aes(x = C1J_lab5x, y = C1J_lab5y + 5, color = "24", label = "+ 27.5",
                angle = 30)) + labs(color = "Days Without\nTreatment")

C1J_final_date_graph