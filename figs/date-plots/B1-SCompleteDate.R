library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(scales)
B1_S_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Stephanie",
                  col_types = c("date", "numeric", "numeric"))
B1_S_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Stephanie",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", "numeric"), skip = 2)
first_B1_S <- B1_S_hours[1:17,]
second_B1_S <- B1_S_hours[19:30,]
third_B1_S <- B1_S_hours[32:37,]

first_B1_S_dates <- B1_S_dates[1:20,]
second_B1_S_dates <- B1_S_dates[35:47,]
third_B1_S_dates <- B1_S_dates[86:89,]


lims <- as.POSIXct(strptime(c("2018-08-15 01:00","2018-12-01 01:00"), format = "%Y-%m-%d %H:%M"))

B1S_dates <- B1_S_dates[,c(1,3)]

B1S_max <- max(B1_S_dates$p_level, na.rm = T)

#B1-S DATE GRAPH
B1S_dategraph <- B1S_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, B1S_max)) +
  labs(x = "Date",  y = "Polarity")

B1S_segment1 <- list(x1 = first_B1_S_dates$date[which.min(first_B1_S_dates$p_level)], 
                     y1 = first_B1_S_dates$p_level[which.min(first_B1_S_dates$p_level)],
                     x2 = second_B1_S_dates$date[which.max(second_B1_S_dates$p_level)],
                     y2 = second_B1_S_dates$p_level[which.max(second_B1_S_dates$p_level)]) 
B1S_lab1x <- as.POSIXct((as.numeric(B1S_segment1$x1) + as.numeric(B1S_segment1$x2)) / 2, origin = '1970-01-01')
B1S_lab1y <- (B1S_segment1$y1 + B1S_segment1$y2)/2

B1S_segment2 <- list(x1 = second_B1_S_dates$date[which.min(second_B1_S_dates$p_level)], 
                     y1 = second_B1_S_dates$p_level[which.min(second_B1_S_dates$p_level)],
                     x2 = third_B1_S_dates$date[which.max(third_B1_S_dates$p_level)],
                     y2 = third_B1_S_dates$p_level[which.max(third_B1_S_dates$p_level)]) 
B1S_lab2x <- as.POSIXct((as.numeric(B1S_segment2$x1) + as.numeric(B1S_segment2$x2)) / 2, origin = '1970-01-01')
B1S_lab2y <- (B1S_segment2$y1 + B1S_segment2$y2)/2

B1S_first_day <- as.POSIXct(first_B1_S$Day[which(first_B1_S$`Total therapy duration (Hrs)`>0)][1],
                            origin = '1970-01-01')
B1S_last_day <- as.POSIXct(tail(third_B1_S$Day[which(third_B1_S$`Total therapy duration (Hrs)`>0)],1),
                           origin = '1970-01-01')

B1S_label_first <- B1S_first_day %>% format(., "%B %d %Y")
B1S_label_last <- B1S_last_day %>% format(., "%B %d %Y")

B1S_final_date_graph <- B1S_dategraph + geom_text(aes(x = B1S_first_day, y = first_B1_S$`Polarity level`[which(first_B1_S$`Polarity level`>0)][1],
                              label = B1S_label_first), hjust = -0.1, vjust = 0, size = 3.5) +
                geom_text(aes(x = B1S_last_day, y = tail(third_B1_S$`Polarity level`[which(third_B1_S$`Polarity level`>0)],1),
                              label = B1S_label_last), hjust = -0.1, vjust = 1, size = 3.5) +
  geom_segment(aes(x = B1S_segment1$x1, y = B1S_segment1$y1,
                                 xend = B1S_segment1$x2, yend = B1S_segment1$y2, color = "14"),
                             linetype = "dashed") +
  geom_text(aes(x = B1S_lab1x, y = B1S_lab1y + 5, color = "14", label = "+ 16.1",
                angle = 22)) + 
  geom_segment(aes(x = B1S_segment2$x1, y = B1S_segment2$y1,
                   xend = B1S_segment2$x2, yend = B1S_segment2$y2, color = "38"),
               linetype = "dashed") +
  geom_text(aes(x = B1S_lab2x, y = B1S_lab2y + 5, color = "38", label = "+ 56.4",
                angle = 29)) + labs(color = "Days Without\nTreatment")

B1S_final_date_graph