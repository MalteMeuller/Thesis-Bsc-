library(readxl)
library(patchwork)
library(tidyverse)

install.packages('tinytex')
library(tinytex)

#RF Expanding Window
RF_EW <- read_excel("C:/Users/malte/OneDrive/Desktop/EW.xlsx")

RF_EW$Date <- seq(as.Date("2016-1-1"), as.Date("2020-02-1"), by = "month")

RF_EW <- RF_EW %>% rename(
  "1 Month, RFEW"="1_MonthEW",
  "3 Month, RFEW"="3_MonthEW",
 "6 Month, RFEW"= "6_MonthEW",
  "12 Month, RFEW"="12_MonthEW"
)



#ARIMA
ARIMA<- read_excel("C:/Users/malte/OneDrive/Desktop/ARIMA-RÄTT.xlsx")
ARIMA<-ARIMA%>% subset(select=c(3,4,5,6))
ARIMA<- ARIMA%>% rename(
  "1 Month, Arima" = "1",
  "3 Month, Arima" = "3",
  "6 Month, Arima" = "6",
  "12 Month, Arima" = "12"
)
ARIMA$Date <- seq(as.Date("2016-01-1"), as.Date("2020-02-1"), by = "month")

#Random Walk
RW <- read_excel("C:/Users/malte/OneDrive/Desktop/RW.xlsx")
RW$Date <- seq(as.Date("2016-01-1"), as.Date("2020-02-1"), by = "month")

A <- full_join(RW, ARIMA, by="Date")
B <- RF_EW

Final <- full_join(A,B, by ="Date")
 
mae(Final$Inflation, Final$`1 Month, RW` )
mae(Final$Inflation, Final$`3 Month, RW` )
mae(Final$Inflation, Final$`6 Month, RW` )
mae(Final$Inflation, Final$`12 Month, RW` )

mae(Final$Inflation, Final$`1 Month, Arima` )
mae(Final$Inflation, Final$`3 Month, Arima` )
mae(Final$Inflation, Final$`6 Month, Arima` )
mae(Final$Inflation, Final$`12 Month, Arima` )

mae(Final$Inflation, Final$`1 Month, RFEW` )
mae(Final$Inflation, Final$`3 Month, RFEW` )
mae(Final$Inflation, Final$`6 Month, RFEW` )
mae(Final$Inflation, Final$`12 Month, RFEW` )

Final <- Final %>%slice (1:49)

Final1 <- Final %>% subset(select=c(1,2,6,7,11))
Final3 <- Final %>% subset(select=c(1,3,6,8,12))
Final6 <- Final %>% subset(select=c(1,4,6,9,13))
Final12 <- Final %>% subset(select=c(1,5,6,10,14))


# 1 month
p1_RW<-ggplot(data=Final1, aes(Date))+
  geom_line(aes(y=Inflation), col="Black")+
  geom_line(aes(y=`1 Month, RW`), col="Red")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=12))+
   labs(title='Random Walk')

p1_ARIMA<-ggplot(data=Final1, aes(Date))+
  geom_line(aes(y=Inflation), col="Black")+
  geom_line(aes(y=`1 Month, Arima`), col="Red")+
  theme(axis.title.x = element_blank(),
        axis.title.y  = element_text(size = 16, face = "bold"),
        plot.title = element_text(size=12))+
  labs(title='ARIMA')

p1_RF<-ggplot(data=Final1, aes(Date))+
  geom_line(aes(y=Inflation), col="Black")+
  geom_line(aes(y=`1 Month, RFEW`), col="Red")+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        plot.title = element_text(size=12))+
  labs(title='Random Forest')

p1tot<- p1_RW+ p1_ARIMA+ p1_RF + plot_layout(ncol=1)
p1tot <- p1tot + plot_annotation( title = "           1 Months forecast")
p1tot


# 3 month
p3_RW<-ggplot(data=Final3, aes(Date))+
  geom_line(aes(y=Inflation), col="Black")+
  geom_line(aes(y=`3 Month, RW`), col="Red")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=12))+
  labs(title='Random Walk')

p3_ARIMA<-ggplot(data=Final3, aes(Date))+
  geom_line(aes(y=Inflation), col="Black")+
  geom_line(aes(y=`3 Month, Arima`), col="Red")+
  theme(axis.title.x = element_blank(),
        axis.title.y  = element_text(size = 16, face = "bold"),
        plot.title = element_text(size=12))+
  labs(title='ARIMA')

p3_RF<-ggplot(data=Final3, aes(Date))+
  geom_line(aes(y=Inflation), col="Black")+
  geom_line(aes(y=`3 Month, RFEW`), col="Red")+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        plot.title = element_text(size=12))+
  labs(title='Random Forest')

p3<-p3_RW+ p3_ARIMA+ p3_RF + plot_layout(ncol=1)
p3tot <- p3 + plot_annotation( title = "          3 Months forecast")
p3tot

#6 month
p6_RW<-ggplot(data=Final6, aes(Date))+
  geom_line(aes(y=Inflation), col="Black")+
  geom_line(aes(y=`6 Month, RW`), col="Red")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=12))+
  labs(title='Random Walk')

p6_ARIMA<-ggplot(data=Final6, aes(Date))+
  geom_line(aes(y=Inflation), col="Black")+
  geom_line(aes(y=`6 Month, Arima`), col="Red")+
  theme(axis.title.x = element_blank(),
        axis.title.y  = element_text(size = 13, face = "bold"),
        plot.title = element_text(size=12))+
  labs(title='ARIMA')

p6_RF<-ggplot(data=Final6, aes(Date))+
  geom_line(aes(y=Inflation), col="Black")+
  geom_line(aes(y=`6 Month, RFEW`), col="Red")+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        plot.title = element_text(size=12))+
  labs(title='Random Forest')

p6<-p6_RW+ p6_ARIMA+ p6_RF + plot_layout(ncol=1)
p6tot<-p6 + plot_annotation( title = "          6 Months forecast")
p6tot



p36tot <- (p3tot | p6tot) + plot_annotation( title = "               3 Months forecast                                                       6 Months forecast")
p36tot

# 12 month
p12_RW<-ggplot(data=Final12, aes(Date))+
  geom_line(aes(y=Inflation), col="Black")+
  geom_line(aes(y=`12 Month, RW`), col="Red")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=12))+
  labs(title='Random Walk')

p12_ARIMA<-ggplot(data=Final12, aes(Date))+
  geom_line(aes(y=Inflation), col="Black")+
  geom_line(aes(y=`12 Month, Arima`), col="Red")+
  theme(axis.title.x = element_blank(),
        axis.title.y  = element_text(size = 13, face = "bold"),
        plot.title = element_text(size=12))+
  labs(title='ARIMA')

p12_RF<-ggplot(data=Final12, aes(Date))+
  geom_line(aes(y=Inflation), col="Black")+
  geom_line(aes(y=`12 Month, RFEW`), col="Red")+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        plot.title = element_text(size=12))+
  labs(title='Random Forest')

p12tot <- p12_RW+ p12_ARIMA+ p12_RF + plot_layout(ncol=1) + plot_annotation( title = "           12 Months forecast")
p12tot








#varimptot
install.packages("ggcharts")
library(dplyr)
library(ggplot2)
library(ggcharts)

imp <- read_excel("C:/Users/malte/OneDrive/Desktop/imp.xlsx")
imptot <- read_excel("C:/Users/malte/OneDrive/Desktop/imp.xlsx")
imptot<-imptot %>% subset(select=c(9,10))
imp<-imp%>% slice(1:10)


imp$Variable1 <- factor(imp$Variable1,                                    
                  levels = imp$Variable1[order(imp$Importance1, decreasing = TRUE)])
imp[2,7] <- "CPIF[t-13]"
imp[3,7] <- "CPIF[t-12]"

#1

chart1 <- imp %>%
  lollipop_chart(Variable1, Importance1) %>%
  print()

chart1<-chart1 +
  geom_text(aes(label = Importance1, hjust = -0.5))+
  labs(
    x = NULL,
    y = "Importance, %",
    title = " Variable Importance,1 Months forecast")

chart1 <- chart1 + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
 theme(plot.background = element_rect(fill = 'white', colour = 'white'))+
  theme( axis.text = element_text(color = "black"))


chart1<-chart1 + theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    axis.text = element_text(color = "black")
  )
chart1


#3
chart3 <- imp %>%
  lollipop_chart(Variable3, Importance3) %>%
  print()

chart3<-chart3 +
  geom_text(aes(label = Importance3, hjust = -0.5))+
  labs(
    x = NULL,
    y = "Importance, %",
    title = " Variable Importance, 3 Months forecast")

chart3 <- chart3 + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))+
  theme( axis.text = element_text(color = "black"))


chart3<-chart3 + theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    axis.text = element_text(color = "black", size=14))
  
chart3

#

chart6 <- imp %>%
  lollipop_chart(Variable6, Importance6) %>%
  print()

chart6<-chart6 +
  geom_text(aes(label = Importance6, hjust = -0.5))+
  labs(
    x = NULL,
    y = "Importance, %",
    title = " Variable Importance, 6 Months forecast")

chart6 <- chart6 + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))+
  theme( axis.text = element_text(color = "black"))


chart6<-chart6 + theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    axis.text = element_text(color = "black", size=14),
    
    
  )
chart6


chart3+chart6




#12
chart12 <- imp %>%
  lollipop_chart(Variable12, Importance12) %>%
  print()

chart12<-chart12 +
  geom_text(aes(label = Importance12, hjust = -0.2))+
  labs(
    x = NULL,
    y = "Importance, %",
    title = " Variable Importance, 12 Months forecast")

chart12 <- chart12 + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))+
  theme( axis.text = element_text(color = "black"))


chart12<-chart12 + theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    axis.text = element_text(color = "black")
  )
chart12


#tot
chartt <- imptot %>%
  lollipop_chart(Total, importanceTotal) 
chartt

chartt<-chartt +
  geom_text(aes(label = importanceTotal, hjust = -0.2))+
  labs(
    x = NULL,
    y = "Importance, %",
    title = " Variable Importance, Total")
chartt

chartt <- chartt + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))+
  theme( axis.text = element_text(color = "black"))


chartt<-chartt + theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    axis.text = element_text(color = "black")
  )
chartt



#tot
charttot <- imptot %>%
  bar_chart(Total, importanceTotal) %>%
  print()

charttot<-charttot +
  geom_text(aes(label = importanceTotal, hjust = "left"))+
  labs(
    x = NULL,
    y = "Importance",
    title = "Variable Importance Plot, Total")
charttot


