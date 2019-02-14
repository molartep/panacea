library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)
library(scales)
L1_J_hours <- read_excel("~/Desktop/Electronegatividad.xlsx", sheet = "Jeff",
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"), skip = 2)
L1_J_dates <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Jeff",
                  col_types = c("date", "numeric", "numeric"))

first_L1_J <- L1_J_hours[1:20,] 

first_L1_J_dates <- L1_J_dates[1:27,]

lims <- as.POSIXct(strptime(c("2018-08-10 01:00","2018-09-17 01:00"), format = "%Y-%m-%d %H:%M"))

L1J_dates <- L1_J_dates[,c(1,3)]

L1J_max <- max(L1_J_dates$p_level, na.rm = T)

#L1-J DATE GRAPH
L1J_dategraph <- L1J_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 week", date_labels =  "%b %d %Y", limits = lims) +
  scale_y_continuous(limits = c(0, L1J_max)) +
  labs(x = "Date",  y = "Polarity")

L1J_first_day <- as.POSIXct(first_L1_J$Day[which(first_L1_J$`Total therapy duration (Hrs)`>0)][1],
                            origin = '1970-01-01')
L1J_last_day <- as.POSIXct(tail(first_L1_J$Day[which(first_L1_J$`Total therapy duration (Hrs)`>0)],1),
                           origin = '1970-01-01')

L1J_label_first <- L1J_first_day %>% format(., "%B %d %Y")
L1J_label_last <- L1J_last_day %>% format(., "%B %d %Y")


L1J_final_date_graph <- L1J_dategraph + geom_text(aes(x = L1J_first_day, y = first_L1_J$`Polarity level`[which(first_L1_J$`Polarity level`>0)][1],
                                                      label = L1J_label_first), hjust = -0.1, vjust = 0, size = 3.5) +
                                        geom_text(aes(x = L1J_last_day, y = tail(first_L1_J$`Polarity level`[which(first_L1_J$`Polarity level`>0)],1),
                                                      label = L1J_label_last), hjust = -0.1, vjust = 1, size = 3.5)


L1J_final_date_graph