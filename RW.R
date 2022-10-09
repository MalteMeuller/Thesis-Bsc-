#random ralk

library(tidyverse)
library(readr)
library(readxl)
library(forecast)
library(Metrics)

RW <- read_excel("C:/Users/malte/OneDrive/Desktop/Tradingeconomics.xlsx")

RW <- RW %>% slice(1:242)

RW$Month <- seq(as.Date("2000-1-1"), as.Date("2020-02-1"), by = "month")

RW$Inflation <-as.ts(RW$Inflation)

RW<-RW%>% subset(select= c(23,61))

#Random Walk

RW$RW1 <- RW$Inflation %>% lag(1)
RW$RW3 <- RW$Inflation %>% lag(3)  
RW$RW6 <- RW$Inflation %>% lag(6)
RW$RW12 <- RW$Inflation %>% lag(12)

#long format, 2016 fram till COVID. 
RW_long <- RW%>% slice(193:242)

RW_long2 <- RW_long %>%
  pivot_longer(cols = c("Inflation", "RW1", "RW3", "RW6", "RW12"), names_to = "Horison", values_to = "Values")
  
RW_long2 %>% ggplot(aes(x=Month, y=Values, colour=Horison))+
  geom_line()

RW_long2 <- RW_long %>%
  pivot_longer(cols = c("Inflation", "RW1", "RW3", "RW6", "RW12"), names_to = "Horison", values_to = "values")

#MAE
mae(RW_long$Inflation, RW_long$RW1)
mae(RW_long$Inflation, RW_long$RW3)
mae(RW_long$Inflation, RW_long$RW6)
mae(RW_long$Inflation, RW_long$RW12)

RW_final <- RW_long

