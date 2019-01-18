library("dplyr")
library("ggplot2")
library(readxl)
library(gridExtra)
library(scales)
B1S <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Stephanie",
                   col_types = c("date", "numeric", "numeric"))
L2E <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Eric", 
                   col_types = c("date", "numeric", "numeric"))
L1J <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Jeff", 
                  col_types = c("date", "numeric", "numeric"))
H1A <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Anna", 
                   col_types = c("date", "numeric", "numeric"))
C1J <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Jim", 
                   col_types = c("date", "numeric", "numeric"))
G1D <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Daryl", 
                   col_types = c("date", "numeric", "numeric"))
L3K <- read_excel("~/Desktop/PATIENT_DATA.xlsx", sheet = "Kevin", 
                   col_types = c("date", "numeric", "numeric"))

lims <- as.POSIXct(strptime(c("2018-08-15 01:00","2019-01-19 01:00"), format = "%Y-%m-%d %H:%M"))

B1S_dates <- B1S[,c(1,3)]
L2E_dates <- L2E[,c(1,3)]
L1J_dates <- L1J[,c(1,3)]
H1A_dates <- H1A[,c(1,3)]
C1J_dates <- C1J[,c(1,3)]
G1D_dates <- G1D[,c(1,3)]
L3K_dates <- L3K[,c(1,3)]

B1S_max <- max(B1S$p_level, na.rm = T)
L2E_max <- max(L2E$p_level, na.rm = T)
L1J_max <- max(L1J$p_level, na.rm = T)
H1A_max <- max(H1A$p_level, na.rm = T)
C1J_max <- max(C1J$p_level, na.rm = T)
G1D_max <- max(G1D$p_level, na.rm = T)
L3K_max <- max(L3K$p_level, na.rm = T)

#B1-S DATE GRAPH
B1S_dategraph <- B1S_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point(size = 1) + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, B1S_max)) +
  labs(subtitle = "B1-S", x = "",  y = "") +
  #geom_smooth(size = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#L2-E DATE GRAPH
L2E_dategraph <- L2E_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point(size = 1) + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, L2E_max)) +
  labs(subtitle = "L2-E", x = "",  y = "") +
  #geom_smooth(size = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#L1-J DATE GRAPH
L1J_dategraph <- L1J_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point(size = 1) + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, L1J_max)) +
  labs(subtitle = "L1-J", x = "",  y = "") +
  #geom_smooth(size = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#H1-A DATE GRAPH
H1A_dategraph <- H1A_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point(size = 1) + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, H1A_max)) +
  labs(subtitle = "H1-A", x = "",  y = "") +
  #geom_smooth(size = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#C1-J DATE GRAPH
C1J_dategraph <- C1J_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point(size = 1) + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, C1J_max)) +
  labs(subtitle = "C1-J", x = "",  y = "") +
  #geom_smooth(size = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#G1-D DATE GRAPH
G1D_dategraph <- G1D_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point(size = 1) + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, G1D_max)) +
  labs(subtitle = "G1-D", x = "",  y = "") +
  #geom_smooth(size = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#L3-K DATE GRAPH
L3K_dategraph <- L3K_dates %>%
  ggplot(aes(x=date, y= p_level)) + geom_point(size = 1) + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  scale_y_continuous(limits = c(0, L3K_max)) +
  labs(subtitle = "L3-K", x = "",  y = "") +
  #geom_smooth(size = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grobs <- list(B1S_dategraph, L2E_dategraph,H1A_dategraph, C1J_dategraph, G1D_dategraph, L3K_dategraph, L1J_dategraph)
graph <- grid.arrange(top = "Polarity Values with Dates", grobs = grobs, ncol=4, scales = "", left = "Polarity Level",
             bottom = "Month")

######################################################################################################################

#Save plot in Desktop folder
label <- paste("All Patients Individual Plots ", Sys.Date(), ".pdf", sep = "")
path <- "~/Desktop/PDFs All Patients Individual Plots/Single Page"

ggsave(label, graph, path = path, height = 6, width = 12)
