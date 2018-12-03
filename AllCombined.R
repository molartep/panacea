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

lims <- as.POSIXct(strptime(c("2018-08-15 01:00","2018-12-01 01:00"), format = "%Y-%m-%d %H:%M"))

B1S_dates <- B1S[,c(1,3)] %>% mutate(Patient= "B1S")
L2E_dates <- L2E[,c(1,3)] %>% mutate(Patient= "L2E")
L1J_dates <- L1J[,c(1,3)] %>% mutate(Patient= "L1J")
H1A_dates <- H1A[,c(1,3)] %>% mutate(Patient= "H1A")
C1J_dates <- C1J[,c(1,3)] %>% mutate(Patient= "C1J")
G1D_dates <- G1D[,c(1,3)] %>% mutate(Patient= "G1D")
L3K_dates <- L3K[,c(1,3)] %>% mutate(Patient= "L3K")

B1S_segment1 <- list(x1 = B1S_dates$date[20], y1 = B1S_dates$p_level[20],
                     x2 = B1S_dates$date[35], y2 = B1S_dates$p_level[35]) 
B1S_lab1x <- as.POSIXct((as.numeric(B1S_segment1$x1) + as.numeric(B1S_segment1$x2)) / 2, origin = '1970-01-01')
B1S_lab1y <- (B1S_segment1$y1 + B1S_segment1$y2)/2

B1S_segment2 <- list(x1 = B1S_dates$date[47], y1 = B1S_dates$p_level[47],
                     x2 = B1S_dates$date[86], y2 = B1S_dates$p_level[86]) 
B1S_lab2x <- as.POSIXct((as.numeric(B1S_segment2$x1) + as.numeric(B1S_segment2$x2)) / 2, origin = '1970-01-01')
B1S_lab2y <- (B1S_segment2$y1 + B1S_segment2$y2)/2

L2E_segment1 <- list(x1 = L2E_dates$date[13], y1 = L2E_dates$p_level[13],
                     x2 = L2E_dates$date[35], y2 = L2E_dates$p_level[35]) 
L2E_lab1x <- as.POSIXct((as.numeric(L2E_segment1$x1) + as.numeric(L2E_segment1$x2)) / 2, origin = '1970-01-01')
L2E_lab1y <- (L2E_segment1$y1 + L2E_segment1$y2)/2

L2E_segment2 <- list(x1 = L2E_dates$date[48], y1 = L2E_dates$p_level[48],
                     x2 = L2E_dates$date[64], y2 = L2E_dates$p_level[64]) 
L2E_lab2x <- as.POSIXct((as.numeric(L2E_segment2$x1) + as.numeric(L2E_segment2$x2)) / 2, origin = '1970-01-01')
L2E_lab2y <- (L2E_segment2$y1 + L2E_segment2$y2)/2

H1A_segment1 <- list(x1 = H1A_dates$date[11], y1 = H1A_dates$p_level[11],
                     x2 = H1A_dates$date[33], y2 = H1A_dates$p_level[33]) 
H1A_lab1x <- as.POSIXct((as.numeric(H1A_segment1$x1) + as.numeric(H1A_segment1$x2)) / 2, origin = '1970-01-01')
H1A_lab1y <- (H1A_segment1$y1 + H1A_segment1$y2)/2

H1A_segment2 <- list(x1 = H1A_dates$date[46], y1 = H1A_dates$p_level[46],
                     x2 = H1A_dates$date[72], y2 = H1A_dates$p_level[72]) 
H1A_lab2x <- as.POSIXct((as.numeric(H1A_segment2$x1) + as.numeric(H1A_segment2$x2)) / 2, origin = '1970-01-01')
H1A_lab2y <- (H1A_segment2$y1 + H1A_segment2$y2)/2

C1J_segment1 <- list(x1 = C1J_dates$date[20], y1 = C1J_dates$p_level[20],
                     x2 = C1J_dates$date[30], y2 = C1J_dates$p_level[30]) 
C1J_lab1x <- as.POSIXct((as.numeric(C1J_segment1$x1) + as.numeric(C1J_segment1$x2)) / 2, origin = '1970-01-01')
C1J_lab1y <- (C1J_segment1$y1 + C1J_segment1$y2)/2

C1J_segment2 <- list(x1 = C1J_dates$date[42], y1 = C1J_dates$p_level[42],
                     x2 = C1J_dates$date[52], y2 = C1J_dates$p_level[52]) 
C1J_lab2x <- as.POSIXct((as.numeric(C1J_segment2$x1) + as.numeric(C1J_segment2$x2)) / 2, origin = '1970-01-01')
C1J_lab2y <- (C1J_segment2$y1 + C1J_segment2$y2)/2

G1D_segment1 <- list(x1 = G1D_dates$date[4], y1 = G1D_dates$p_level[4],
                     x2 = G1D_dates$date[39], y2 = G1D_dates$p_level[39]) 
G1D_lab1x <- as.POSIXct((as.numeric(G1D_segment1$x1) + as.numeric(G1D_segment1$x2)) / 2, origin = '1970-01-01')
G1D_lab1y <- (G1D_segment1$y1 + G1D_segment1$y2)/2

patients <- rbind(B1S_dates, L2E_dates, L1J_dates, H1A_dates, C1J_dates, G1D_dates, L3K_dates)

patients %>% group_by(Patient) %>%
  ggplot(aes(x=date, y= p_level, color = Patient)) + geom_point() + 
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y", limits = lims) +
  
  #B1S Segments
  
  geom_segment(aes(x = B1S_segment1$x1, y = B1S_segment1$y1,
                     xend = B1S_segment1$x2, yend = B1S_segment1$y2, color = "B1S"),
                 linetype = "dashed") +
  geom_text(aes(x = B1S_lab1x, y = B1S_lab1y + 5, label = "+ 16.1 in 15 days", color = "B1S",
                angle = 16), size = 3) + 
  geom_segment(aes(x = B1S_segment2$x1, y = B1S_segment2$y1,
                   xend = B1S_segment2$x2, yend = B1S_segment2$y2, color = "B1S"),
               linetype = "dashed") +
  geom_text(aes(x = B1S_lab2x, y = B1S_lab2y + 5, label = "+ 56.4 in 38 days", color = "B1S",
                angle = 23), size = 3) + 
  
  #L2E Segments
  
  geom_segment(aes(x = L2E_segment1$x1, y = L2E_segment1$y1,
                     xend = L2E_segment1$x2, yend = L2E_segment1$y2, color = "L2E"),
                 linetype = "dashed") +
  geom_text(aes(x = L2E_lab1x, y = L2E_lab1y + 5, color = "L2E", label = "+ 11.9 in 22 days",
                angle = 9), size = 3) + 
  geom_segment(aes(x = L2E_segment2$x1, y = L2E_segment2$y1,
                   xend = L2E_segment2$x2, yend = L2E_segment2$y2, color = "L2E"),
               linetype = "dashed") +
  geom_text(aes(x = L2E_lab2x, y = L2E_lab2y + 5, color = "L2E", label = "+ 8.6 in 15 days",
                angle = 8), size = 3) +
  
  #H1A Segments
  
  geom_segment(aes(x = H1A_segment1$x1, y = H1A_segment1$y1,
                     xend = H1A_segment1$x2, yend = H1A_segment1$y2, color = "H1A"),
                 linetype = "dashed") +
  geom_text(aes(x = H1A_lab1x, y = H1A_lab1y - 5, color = "H1A", label = "+ 1.9 in 22 days",
                angle = 2), size = 3) + 
  geom_segment(aes(x = H1A_segment2$x1, y = H1A_segment2$y1,
                   xend = H1A_segment2$x2, yend = H1A_segment2$y2, color = "H1A"),
               linetype = "dashed") +
  geom_text(aes(x = H1A_lab2x, y = H1A_lab2y - 5, color = "H1A", label = "+ 2 in 25 days",
                angle = 2), size = 3) +
  
  #C1J Segments
  geom_segment(aes(x = C1J_segment1$x1, y = C1J_segment1$y1,
                     xend = C1J_segment1$x2, yend = C1J_segment1$y2, color = "C1J"),
                 linetype = "dashed") +
  geom_text(aes(x = C1J_lab1x, y = C1J_lab1y + 5, color = "C1J", label = "+ 18.1 / 9 days",
                angle = 27), size = 3) + 
  geom_segment(aes(x = C1J_segment2$x1, y = C1J_segment2$y1,
                   xend = C1J_segment2$x2, yend = C1J_segment2$y2, color = "C1J"),
               linetype = "dashed") +
  geom_text(aes(x = C1J_lab2x, y = C1J_lab2y + 5, color = "C1J", label = "+ 19.1 / 9 days",
                angle = 29), size = 3) +
  
  #G1D Segments
  
  geom_segment(aes(x = G1D_segment1$x1, y = G1D_segment1$y1,
                     xend = G1D_segment1$x2, yend = G1D_segment1$y2, color = "G1D"),
                 linetype = "dashed") +
  geom_text(aes(x = G1D_lab1x, y = G1D_lab1y + 5, color = "G1D", label = "+ 2.4 in 35 days",
                angle = 1), size = 3) +
  
  #L3K Segments
  
  labs(title = "Polarity Levels", x = "Months",  y = "Polarity Levels")

