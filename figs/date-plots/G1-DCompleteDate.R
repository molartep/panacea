library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(scales)
G1D <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Daryl", 
                  col_types = c("date", "numeric", "numeric"))

lims <- as.POSIXct(strptime(c("2018-08-15 01:00","2018-12-01 01:00"), format = "%Y-%m-%d %H:%M"))

G1D_dates <- G1D[,c(1,3)]

G1D_max <- max(G1D$p_level, na.rm = T)

#L2-E DATE GRAPH
G1D_dategraph <- G1D_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, G1D_max)) +
  labs(subtitle = "L2-E", x = "",  y = "") +
  #geom_smooth(size = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

G1D_segment1 <- list(x1 = G1D_dates$date[4], y1 = G1D_dates$p_level[4],
                     x2 = G1D_dates$date[39], y2 = G1D_dates$p_level[39]) 
G1D_lab1x <- as.POSIXct((as.numeric(G1D_segment1$x1) + as.numeric(G1D_segment1$x2)) / 2, origin = '1970-01-01')
G1D_lab1y <- (G1D_segment1$y1 + G1D_segment1$y2)/2

G1D_dategraph + geom_segment(aes(x = G1D_segment1$x1, y = G1D_segment1$y1,
                                 xend = G1D_segment1$x2, yend = G1D_segment1$y2, color = "35"),
                             linetype = "dashed") +
  geom_text(aes(x = G1D_lab1x, y = G1D_lab1y + 0.5, color = "35", label = "+ 2.4",
                angle = 25)) + labs(color = "Days Without\nTreatment")