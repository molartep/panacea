library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
library(scales)
H1_A_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Anna", 
                  col_types = c("date", "numeric", "numeric"))
H1_A_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Anna",
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"), skip = 2)

first_H1_A_hours <- H1_A_hours[1:9,]
second_H1_A_hours <- H1_A_hours[11:23,] 
third_H1_A_hours <- H1_A_hours[25:26,] 
fourth_H1_A_hours <- H1_A_hours[28:39,]
fifth_H1_A_hours <- H1_A_hours[41:52,]

first_H1_A_dates <- H1_A_dates[1:11,]
second_H1_A_dates <- H1_A_dates[33:46,]
third_H1_A_dates <- H1_A_dates[72:73,]
fourth_H1_A_dates <- H1_A_dates[111:115,]
fifth_H1_A_dates <- H1_A_dates[140:151,]

lims <- as.POSIXct(strptime(c("2018-08-15 01:00","2019-02-20 01:00"), format = "%Y-%m-%d %H:%M"))

H1A_dates <- H1_A_dates[,c(1,3)]

H1A_max <- max(H1_A_dates$p_level, na.rm = T)

#H1-A DATE GRAPH
H1A_dategraph <- H1A_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, H1A_max)) +
  labs(x = "Date",  y = "Polarity")

H1A_segment1 <- list(x1 = first_H1_A_dates$date[which.min(first_H1_A_dates$p_level)], 
                     y1 = first_H1_A_dates$p_level[which.min(first_H1_A_dates$p_level)],
                     x2 = second_H1_A_dates$date[which.max(second_H1_A_dates$p_level)],
                     y2 = second_H1_A_dates$p_level[which.max(second_H1_A_dates$p_level)]) 
H1A_lab1x <- as.POSIXct((as.numeric(H1A_segment1$x1) + as.numeric(H1A_segment1$x2)) / 2, origin = '1970-01-01')
H1A_lab1y <- (H1A_segment1$y1 + H1A_segment1$y2)/2

H1A_segment2 <- list(x1 = second_H1_A_dates$date[which.min(second_H1_A_dates$p_level)], 
                     y1 = second_H1_A_dates$p_level[which.min(second_H1_A_dates$p_level)],
                     x2 = third_H1_A_dates$date[which.max(third_H1_A_dates$p_level)],
                     y2 = third_H1_A_dates$p_level[which.max(third_H1_A_dates$p_level)]) 
H1A_lab2x <- as.POSIXct((as.numeric(H1A_segment2$x1) + as.numeric(H1A_segment2$x2)) / 2, origin = '1970-01-01')
H1A_lab2y <- (H1A_segment2$y1 + H1A_segment2$y2)/2

H1A_segment3 <- list(x1 = third_H1_A_dates$date[which.min(third_H1_A_dates$p_level)], 
                     y1 = third_H1_A_dates$p_level[which.min(third_H1_A_dates$p_level)],
                     x2 = fourth_H1_A_dates$date[which.max(fourth_H1_A_dates$p_level)],
                     y2 = fourth_H1_A_dates$p_level[which.max(fourth_H1_A_dates$p_level)]) 
H1A_lab3x <- as.POSIXct((as.numeric(H1A_segment3$x1) + as.numeric(H1A_segment3$x2)) / 2, origin = '1970-01-01')
H1A_lab3y <- (H1A_segment3$y1 + H1A_segment3$y2)/2

H1A_segment4 <- list(x1 = fourth_H1_A_dates$date[which.min(fourth_H1_A_dates$p_level)], 
                     y1 = fourth_H1_A_dates$p_level[which.min(fourth_H1_A_dates$p_level)],
                     x2 = fifth_H1_A_dates$date[which.max(fifth_H1_A_dates$p_level)],
                     y2 = fifth_H1_A_dates$p_level[which.max(fifth_H1_A_dates$p_level)]) 
H1A_lab4x <- as.POSIXct((as.numeric(H1A_segment4$x1) + as.numeric(H1A_segment4$x2)) / 2, origin = '1970-01-01')
H1A_lab4y <- (H1A_segment4$y1 + H1A_segment4$y2)/2

H1A_first_day <- as.POSIXct(first_H1_A_hours$Day[which(first_H1_A_hours$`Total therapy duration (Hrs)`>0)][1],
                            origin = '1970-01-01')
H1A_last_day <- as.POSIXct(tail(fifth_H1_A_hours$Day[which(fifth_H1_A_hours$`Total therapy duration (Hrs)`>0)],1),
                           origin = '1970-01-01')

H1A_label_first <- H1A_first_day %>% format(., "%B %d %Y")
H1A_label_last <- H1A_last_day %>% format(., "%B %d %Y")

H1A_final_date_graph <- H1A_dategraph + geom_text(aes(x = H1A_first_day, y = first_H1_A_hours$`Polarity level`[which(first_H1_A_hours$`Polarity level`>0)][1],
                                      label = H1A_label_first), hjust = -0.1, vjust = 0, size = 3.5) +
                        geom_text(aes(x = H1A_last_day, y = tail(fifth_H1_A_hours$`Polarity level`[which(fifth_H1_A_hours$`Polarity level`>0)],1),
                                      label = H1A_label_last), hjust = -0.1, vjust = 1, size = 3.5) +
  geom_segment(aes(x = H1A_segment1$x1, y = H1A_segment1$y1,
                   xend = H1A_segment1$x2, yend = H1A_segment1$y2, color = "23"),
               linetype = "dashed") +
  geom_text(aes(x = H1A_lab1x, y = H1A_lab1y + 2, color = "23", label = "+ 1.9",
                angle = 5)) + 
  geom_segment(aes(x = H1A_segment2$x1, y = H1A_segment2$y1,
                   xend = H1A_segment2$x2, yend = H1A_segment2$y2, color = "26"),
               linetype = "dashed") +
  geom_text(aes(x = H1A_lab2x, y = H1A_lab2y + 2, color = "26", label = "+ 2.0",
                angle = 5)) + 
  geom_segment(aes(x = H1A_segment3$x1, y = H1A_segment3$y1,
                   xend = H1A_segment3$x2, yend = H1A_segment3$y2, color = "32"),
               linetype = "dashed") +
  geom_text(aes(x = H1A_lab3x, y = H1A_lab3y + 2, color = "32", label = "+ 8.9",
                angle = 20)) + 
  geom_segment(aes(x = H1A_segment4$x1, y = H1A_segment4$y1,
                   xend = H1A_segment4$x2, yend = H1A_segment4$y2, color = "25"),
               linetype = "dashed") +
  geom_text(aes(x = H1A_lab4x, y = H1A_lab4y + 2, color = "25", label = "+ 1.7",
                angle = 5)) + labs(color = "Days Without\nTreatment")

H1A_final_date_graph
