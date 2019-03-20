library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
library(scales)
L3_K_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Kevin",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

L3_K_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Kevin",
                         col_types = c("date", "numeric", "numeric"))

first_L3_K <- L3_K_hours[1:7,] 
second_L3_K <- L3_K_hours[9:20,]
third_L3_K <- L3_K_hours[22:28,]
fourth_L3_K <- L3_K_hours[30:34,]
fifth_L3_K <- L3_K_hours[36:42,]

first_L3_K_dates <- L3_K_dates[2:7,]
second_L3_K_dates <- L3_K_dates[38:48,]
third_L3_K_dates <- L3_K_dates[79:84,]
fourth_L3_K_dates <- L3_K_dates[107:110,]
fifth_L3_K_dates <- L3_K_dates[135:140,]

lims <- as.POSIXct(strptime(c("2018-10-25 01:00","2019-04-20 01:00"), format = "%Y-%m-%d %H:%M"))

L3K_dates <- L3_K_dates[,c(1,3)]


L3K_max <- max(L3_K_dates$p_level, na.rm = T)

#L3-K DATE GRAPH 

L3K_dategraph <- L3K_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, L3K_max)) +
  labs(x = "Date",  y = "Polarity")

L3K_segment1 <- list(x1 = first_L3_K_dates$date[which.min(first_L3_K_dates$p_level)], 
                     y1 = first_L3_K_dates$p_level[which.min(first_L3_K_dates$p_level)],
                     x2 = second_L3_K_dates$date[which.max(second_L3_K_dates$p_level)],
                     y2 = second_L3_K_dates$p_level[which.max(second_L3_K_dates$p_level)]) 
L3K_lab1x <- as.POSIXct((as.numeric(L3K_segment1$x1) + as.numeric(L3K_segment1$x2)) / 2, origin = '1970-01-01')
L3K_lab1y <- (L3K_segment1$y1 + L3K_segment1$y2)/2

L3K_segment2 <- list(x1 = second_L3_K_dates$date[which.min(second_L3_K_dates$p_level)], 
                     y1 = second_L3_K_dates$p_level[which.min(second_L3_K_dates$p_level)],
                     x2 = third_L3_K_dates$date[which.max(third_L3_K_dates$p_level)],
                     y2 = third_L3_K_dates$p_level[which.max(third_L3_K_dates$p_level)]) 
L3K_lab2x <- as.POSIXct((as.numeric(L3K_segment2$x1) + as.numeric(L3K_segment2$x2)) / 2, origin = '1970-01-01')
L3K_lab2y <- (L3K_segment2$y1 + L3K_segment2$y2)/2

L3K_segment3 <- list(x1 = third_L3_K_dates$date[which.min(third_L3_K_dates$p_level)], 
                     y1 = third_L3_K_dates$p_level[which.min(third_L3_K_dates$p_level)],
                     x2 = fourth_L3_K_dates$date[which.max(fourth_L3_K_dates$p_level)],
                     y2 = fourth_L3_K_dates$p_level[which.max(fourth_L3_K_dates$p_level)]) 
L3K_lab3x <- as.POSIXct((as.numeric(L3K_segment3$x1) + as.numeric(L3K_segment3$x2)) / 2, origin = '1970-01-01')
L3K_lab3y <- (L3K_segment3$y1 + L3K_segment3$y2)/2

L3K_segment4 <- list(x1 = fourth_L3_K_dates$date[which.min(fourth_L3_K_dates$p_level)], 
                     y1 = fourth_L3_K_dates$p_level[which.min(fourth_L3_K_dates$p_level)],
                     x2 = fifth_L3_K_dates$date[which.max(fifth_L3_K_dates$p_level)],
                     y2 = fifth_L3_K_dates$p_level[which.max(fifth_L3_K_dates$p_level)]) 
L3K_lab4x <- as.POSIXct((as.numeric(L3K_segment4$x1) + as.numeric(L3K_segment4$x2)) / 2, origin = '1970-01-01')
L3K_lab4y <- (L3K_segment4$y1 + L3K_segment4$y2)/2

L3K_first_day <- as.POSIXct(first_L3_K$Day[which(first_L3_K$`Total therapy duration (Hrs)`>0)][1],
                            origin = '1970-01-01')
L3K_last_day <- as.POSIXct(tail(fifth_L3_K$Day[which(fifth_L3_K$`Total therapy duration (Hrs)`>0)],1),
                           origin = '1970-01-01')

L3K_label_first <- L3K_first_day %>% format(., "%B %d %Y")
L3K_label_last <- L3K_last_day %>% format(., "%B %d %Y")

L3K_final_date_graph <- L3K_dategraph + geom_text(aes(x = L3K_first_day, y = first_L3_K$`Polarity level`[which(first_L3_K$`Polarity level`>0)][1],
                                                      label = L3K_label_first), hjust = -0.1, vjust = 0, size = 3.5) +
  geom_text(aes(x = L3K_last_day, y = tail(fifth_L3_K$`Polarity level`[which(fifth_L3_K$`Polarity level`>0)],1),
                label = L3K_label_last), hjust = -0.1, vjust = 0, size = 3.5) +
  geom_segment(aes(x = L3K_segment1$x1, y = L3K_segment1$y1,
                   xend = L3K_segment1$x2, yend = L3K_segment1$y2, color = "30"),
               linetype = "dashed") +
  geom_text(aes(x = L3K_lab1x, y = L3K_lab1y + 5, color = "30", label = "+ 11.7",
                angle = 7)) + 
  geom_segment(aes(x = L3K_segment2$x1, y = L3K_segment2$y1,
                   xend = L3K_segment2$x2, yend = L3K_segment2$y2, color = "30 "),
               linetype = "dashed") +
  geom_text(aes(x = L3K_lab2x, y = L3K_lab2y + 5, color = "30 ", label = "+ 13.2",
                angle = 8)) +
  geom_segment(aes(x = L3K_segment3$x1, y = L3K_segment3$y1,
                   xend = L3K_segment3$x2, yend = L3K_segment3$y2, color = "23"),
               linetype = "dashed") +
  geom_text(aes(x = L3K_lab3x, y = L3K_lab3y + 5, color = "23", label = "+ 10.6",
                angle = 8)) +
  geom_segment(aes(x = L3K_segment4$x1, y = L3K_segment4$y1,
                 xend = L3K_segment4$x2, yend = L3K_segment4$y2, color = "25"),
             linetype = "dashed") +
  geom_text(aes(x = L3K_lab4x, y = L3K_lab4y + 5, color = "25", label = "+ 43.6",
                angle = 45)) + labs(color = "Days Without\nTreatment")

L3K_final_date_graph