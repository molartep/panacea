library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
library(scales)
G1_D_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Daryl",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)

G1_D_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Daryl",
                         col_types = c("date", "numeric", "numeric"))

first_G1_D <- G1_D_hours[1:4,] 
second_G1_D <- G1_D_hours[6:9,]
third_G1_D <- G1_D_hours[11:14,]
fourth_G1_D <- G1_D_hours[16:19,]

first_G1_D_dates <- G1_D_dates[1:4,]
second_G1_D_dates <- G1_D_dates[39:42,]
third_G1_D_dates <- G1_D_dates[79:82,]
fourth_G1_D_dates <- G1_D_dates[115:117,]

lims <- as.POSIXct(strptime(c("2018-09-15 01:00","2019-03-10 01:00"), format = "%Y-%m-%d %H:%M"))

G1D_dates <- G1_D_dates[,c(1,3)]


G1D_max <- max(G1_D_dates$p_level, na.rm = T)

#G1-D DATE GRAPH 

G1D_dategraph <- G1D_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, G1D_max)) +
  labs(x = "Date",  y = "Polarity")

G1D_segment1 <- list(x1 = first_G1_D_dates$date[which.min(first_G1_D_dates$p_level)], 
                     y1 = first_G1_D_dates$p_level[which.min(first_G1_D_dates$p_level)],
                     x2 = second_G1_D_dates$date[which.max(second_G1_D_dates$p_level)],
                     y2 = second_G1_D_dates$p_level[which.max(second_G1_D_dates$p_level)]) 
G1D_lab1x <- as.POSIXct((as.numeric(G1D_segment1$x1) + as.numeric(G1D_segment1$x2)) / 2, origin = '1970-01-01')
G1D_lab1y <- (G1D_segment1$y1 + G1D_segment1$y2)/2

G1D_segment2 <- list(x1 = second_G1_D_dates$date[which.min(second_G1_D_dates$p_level)], 
                     y1 = second_G1_D_dates$p_level[which.min(second_G1_D_dates$p_level)],
                     x2 = third_G1_D_dates$date[which.max(third_G1_D_dates$p_level)],
                     y2 = third_G1_D_dates$p_level[which.max(third_G1_D_dates$p_level)]) 
G1D_lab2x <- as.POSIXct((as.numeric(G1D_segment2$x1) + as.numeric(G1D_segment2$x2)) / 2, origin = '1970-01-01')
G1D_lab2y <- (G1D_segment2$y1 + G1D_segment2$y2)/2

G1D_segment3 <- list(x1 = third_G1_D_dates$date[which.min(third_G1_D_dates$p_level)], 
                     y1 = third_G1_D_dates$p_level[which.min(third_G1_D_dates$p_level)],
                     x2 = fourth_G1_D_dates$date[which.max(fourth_G1_D_dates$p_level)],
                     y2 = fourth_G1_D_dates$p_level[which.max(fourth_G1_D_dates$p_level)]) 
G1D_lab3x <- as.POSIXct((as.numeric(G1D_segment3$x1) + as.numeric(G1D_segment3$x2)) / 2, origin = '1970-01-01')
G1D_lab3y <- (G1D_segment3$y1 + G1D_segment3$y2)/2

G1D_first_day <- as.POSIXct(first_G1_D$Day[which(first_G1_D$`Total therapy duration (Hrs)`>0)][1],
                            origin = '1970-01-01')
G1D_last_day <- as.POSIXct(tail(fourth_G1_D$Day[which(fourth_G1_D$`Total therapy duration (Hrs)`>0)],1),
                           origin = '1970-01-01')

G1D_label_first <- G1D_first_day %>% format(., "%B %d %Y")
G1D_label_last <- G1D_last_day %>% format(., "%B %d %Y")

G1D_final_date_graph <- G1D_dategraph + geom_text(aes(x = G1D_first_day, y = first_G1_D$`Polarity level`[which(first_G1_D$`Polarity level`>0)][1],
                                                      label = G1D_label_first), hjust = -0.1, vjust = 0, size = 3.5) +
                                        geom_text(aes(x = G1D_last_day, y = tail(fourth_G1_D$`Polarity level`[which(fourth_G1_D$`Polarity level`>0)],1),
                                                      label = G1D_label_last), hjust = -0.1, vjust = 1, size = 3.5) +
  geom_segment(aes(x = G1D_segment1$x1, y = G1D_segment1$y1,
                   xend = G1D_segment1$x2, yend = G1D_segment1$y2, color = "36"),
               linetype = "dashed") +
  geom_text(aes(x = G1D_lab1x, y = G1D_lab1y + 0.4, color = "36", label = "+ 2.4",
                angle = 36)) + 
  geom_segment(aes(x = G1D_segment2$x1, y = G1D_segment2$y1,
                   xend = G1D_segment2$x2, yend = G1D_segment2$y2, color = "38"),
               linetype = "dashed") +
  geom_text(aes(x = G1D_lab2x, y = G1D_lab2y + 0.4, color = "38", label = "+ 1.1",
                angle = 18)) +
  geom_segment(aes(x = G1D_segment3$x1, y = G1D_segment3$y1,
                   xend = G1D_segment3$x2, yend = G1D_segment3$y2, color = "33"),
               linetype = "dashed") +
  geom_text(aes(x = G1D_lab3x, y = G1D_lab3y + 0.4, color = "33", label = "+ 1.5",
                angle = 27)) + labs(color = "Days Without\nTreatment")

G1D_final_date_graph