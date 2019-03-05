library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(scales)
L2_E_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Eric", 
                  col_types = c("date", "numeric", "numeric"))
L2_E_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Eric",
                         col_types = c("date", "numeric", "numeric", "numeric",
                                       "numeric", "numeric", "numeric"), skip = 2)

lims <- as.POSIXct(strptime(c("2018-08-15 01:00","2019-03-20 01:00"), format = "%Y-%m-%d %H:%M"))

first_L2_E <- L2_E_hours[1:11,] 
second_L2_E <- L2_E_hours[13:25,]
third_L2_E <- L2_E_hours[27:39,]
fourth_L2_E <- L2_E_hours[41:52,]
fifth_L2_E <- L2_E_hours[54:65,]

first_L2_E_dates <- L2_E_dates[1:13,]
second_L2_E_dates <- L2_E_dates[35:48,]
third_L2_E_dates <- L2_E_dates[64:76,]
fourth_L2_E_dates <- L2_E_dates[108:117,]
fifth_L2_E_dates <- L2_E_dates[142:153,]

L2E_dates <- L2_E_dates[,c(1,3)]

L2E_max <- max(L2_E_dates$p_level, na.rm = T)

#L2-E DATE GRAPH
L2E_dategraph <- L2E_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, L2E_max)) +
  labs(x = "Date",  y = "Polarity")

L2E_segment1 <- list(x1 = first_L2_E_dates$date[which.min(first_L2_E_dates$p_level)], 
                     y1 = first_L2_E_dates$p_level[which.min(first_L2_E_dates$p_level)],
                     x2 = second_L2_E_dates$date[which.max(second_L2_E_dates$p_level)],
                     y2 = second_L2_E_dates$p_level[which.max(second_L2_E_dates$p_level)]) 
L2E_lab1x <- as.POSIXct((as.numeric(L2E_segment1$x1) + as.numeric(L2E_segment1$x2)) / 2, origin = '1970-01-01')
L2E_lab1y <- (L2E_segment1$y1 + L2E_segment1$y2)/2

L2E_segment2 <- list(x1 = second_L2_E_dates$date[which.min(second_L2_E_dates$p_level)], 
                     y1 = second_L2_E_dates$p_level[which.min(second_L2_E_dates$p_level)],
                     x2 = third_L2_E_dates$date[which.max(third_L2_E_dates$p_level)],
                     y2 = third_L2_E_dates$p_level[which.max(third_L2_E_dates$p_level)]) 
L2E_lab2x <- as.POSIXct((as.numeric(L2E_segment2$x1) + as.numeric(L2E_segment2$x2)) / 2, origin = '1970-01-01')
L2E_lab2y <- (L2E_segment2$y1 + L2E_segment2$y2)/2

L2E_segment3 <- list(x1 = third_L2_E_dates$date[which.min(third_L2_E_dates$p_level)], 
                     y1 = third_L2_E_dates$p_level[which.min(third_L2_E_dates$p_level)],
                     x2 = fourth_L2_E_dates$date[which.max(fourth_L2_E_dates$p_level)],
                     y2 = fourth_L2_E_dates$p_level[which.max(fourth_L2_E_dates$p_level)]) 
L2E_lab3x <- as.POSIXct((as.numeric(L2E_segment3$x1) + as.numeric(L2E_segment3$x2)) / 2, origin = '1970-01-01')
L2E_lab3y <- (L2E_segment3$y1 + L2E_segment3$y2)/2

L2E_segment4 <- list(x1 = fourth_L2_E_dates$date[which.min(fourth_L2_E_dates$p_level)], 
                     y1 = fourth_L2_E_dates$p_level[which.min(fourth_L2_E_dates$p_level)],
                     x2 = fifth_L2_E_dates$date[which.max(fifth_L2_E_dates$p_level)],
                     y2 = fifth_L2_E_dates$p_level[which.max(fifth_L2_E_dates$p_level)]) 
L2E_lab4x <- as.POSIXct((as.numeric(L2E_segment4$x1) + as.numeric(L2E_segment4$x2)) / 2, origin = '1970-01-01')
L2E_lab4y <- (L2E_segment4$y1 + L2E_segment4$y2)/2

L2E_first_day <- as.POSIXct(first_L2_E$Day[which(first_L2_E$`Total therapy duration (Hrs)`>0)][1],
                            origin = '1970-01-01')
L2E_last_day <- as.POSIXct(tail(fifth_L2_E$Day[which(fifth_L2_E$`Total therapy duration (Hrs)`>0)],1),
                           origin = '1970-01-01')

L2E_label_first <- L2E_first_day %>% format(., "%B %d %Y")
L2E_label_last <- L2E_last_day %>% format(., "%B %d %Y")

L2E_final_date_graph <- L2E_dategraph + geom_text(aes(x = L2E_first_day, y = first_L2_E$`Polarity level`[which(first_L2_E$`Polarity level`>0)][1],
                                                      label = L2E_label_first), hjust = -0.1, vjust = 0, size = 3.5) +
  geom_text(aes(x = L2E_last_day, y = tail(fifth_L2_E$`Polarity level`[which(fifth_L2_E$`Polarity level`>0)],1),
                label = L2E_label_last), hjust = -0.1, vjust = 0, size = 3.5) +
  geom_segment(aes(x = L2E_segment1$x1, y = L2E_segment1$y1,
                   xend = L2E_segment1$x2, yend = L2E_segment1$y2, color = "23"),
               linetype = "dashed") +
  geom_text(aes(x = L2E_lab1x, y = L2E_lab1y + 5, color = "23", label = "+ 11.9",
                angle = 23)) + 
  geom_segment(aes(x = L2E_segment2$x1, y = L2E_segment2$y1,
                   xend = L2E_segment2$x2, yend = L2E_segment2$y2, color = "16"),
               linetype = "dashed") +
  geom_text(aes(x = L2E_lab2x, y = L2E_lab2y + 5, color = "16", label = "+ 8.6",
                angle = 25)) +
  geom_segment(aes(x = L2E_segment3$x1, y = L2E_segment3$y1,
                   xend = L2E_segment3$x2, yend = L2E_segment3$y2, color = "30"),
               linetype = "dashed") +
  geom_text(aes(x = L2E_lab3x, y = L2E_lab3y + 5, color = "30", label = "+ 5.2",
                angle = 13)) + 
  geom_segment(aes(x = L2E_segment4$x1, y = L2E_segment4$y1,
                   xend = L2E_segment4$x2, yend = L2E_segment4$y2, color = "24"),
               linetype = "dashed") +
  geom_text(aes(x = L2E_lab4x, y = L2E_lab4y + 5, color = "24", label = "+ 2.4",
                angle = 7)) + labs(color = "Days Without\nTreatment")

L2E_final_date_graph