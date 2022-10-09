

library(readr)
library(readxl)
library(tidyverse)
library(forecast)
library(Metrics)

ARIMA <- read_excel("C:/Users/malte/OneDrive/Desktop/Tradingeconomics.xlsx")

ARIMA <- ARIMA %>% slice(1:242)

ARIMA$Month <- seq(as.Date("2000-1-1"), as.Date("2020-02-1"), by = "month")

ARIMA<-ARIMA%>% subset(select= c(23,61))

#repeat
ARIMA2 <- ARIMA %>% slice(1:241)

Inflation <- ts(ARIMA2$Inflation, start=2002-01-01, frequency = 12)

model = auto.arima(Inflation, seasonal = TRUE)
summary(model)

f<-as.data.frame(forecast(model, h=12))
f[1,]
f[3,]
f[6,]
f[12,]

#ladda ner excell-fil
auto<- read_excel("C:/Users/malte/OneDrive/Desktop/ARIMA-RÄTT.xlsx")
auto$Month <- seq(as.Date("2016-1-1"), as.Date("2020-02-1"), by = "month")
Inflation_comp <- ARIMA%>% slice(193:242)

MAE <- full_join(auto, Inflation_comp, by="Month")

MAE <- MAE%>% subset(select=c(-1,-2))

mae(MAE$Inflation, MAE$`1`)
mae(MAE$Inflation, MAE$`3`)
mae(MAE$Inflation, MAE$`6`)
mae(MAE$Inflation, MAE$`12`)

MAE2 <- MAE%>% subset(select=c(-5))

MAE2 <- MAE2 %>% 
  pivot_longer(cols = c("1", "3", "6", "12", "Inflation"), values_to = "values", names_to = "horison")

MAE2 %>% ggplot(aes(x=Month, y=values, colour=horison))+
  geom_line()

ARIMA_final <- auto %>% rename(
  "ARIMA1"="1",
  "ARIMA3"="3",
  "ARIMA6"="6",
  "ARIMA12"="12"
) 

ARIMA_final <- ARIMA_final %>% subset(select=c(-1,-2,-7))
