
library(readr)
library(readxl)
library(tidyverse)
library(randomForest)
library(Metrics)


diff1 <- read_excel("C:/Users/malte/OneDrive/Desktop/diff.xlsx")

diff1 <- diff1 %>% subset(select=c(-245,-247,-248,-249))

diff1 <- diff1 %>% slice(1:238)

diff1$Date <- seq(as.Date("2000-5-1"), as.Date("2020-02-1"), by = "month")

diff1$Order <- 1:238

diff3 <- read_excel("C:/Users/malte/OneDrive/Desktop/diff.xlsx")

diff3 <- diff3 %>% subset(select=c(-245,-246,-248,-249))

diff3 <- diff3 %>% slice(1:238)

diff3$Date <- seq(as.Date("2000-5-1"), as.Date("2020-02-1"), by = "month")

diff3$Order <- 1:238

diff6 <- read_excel("C:/Users/malte/OneDrive/Desktop/diff.xlsx")

diff6 <- diff6 %>% subset(select=c(-245,-246,-247,-249))

diff6 <- diff6 %>% slice(1:238)

diff6$Date <- seq(as.Date("2000-5-1"), as.Date("2020-02-1"), by = "month")

diff6$Order <- 1:238

diff12 <- read_excel("C:/Users/malte/OneDrive/Desktop/diff.xlsx")

diff12 <- diff12 %>% subset(select=c(-245,-246,-247,-248))

diff12 <- diff12 %>% slice(1:238)

diff12$Date <- seq(as.Date("2000-5-1"), as.Date("2020-02-1"), by = "month")

diff12$Order <- 1:238

#PR1
diff1$Class <- ifelse (diff1$Order > (187), "Test" , "Train" )
traindiff1 = diff1[diff1$Class == 'Train',]
testdiff1 = diff1[diff1$Class == 'Test',]
rfdiff1 = randomForest(PR1 ~ Inflation + Inflation1+Date+Order+Interest_Rate3+Inflation2+Interest_Rate2+ KIX_index1 +KIX_index3+KIX_index2 + Year+ Interest_Rate+ M1_supply3+ Lending_Housegolds_Growth3+ M1_supply2+ Lending_Housegolds_Growth2+Lending_Housegolds_Growth+ Inflation3+ Government_Debt2+Wage_growth2,mtry=15, ntree=600, nodesize=10, importance=TRUE, data = traindiff1)
predictionsdiff1 = predict(rfdiff1, newdata = testdiff1)
pred1 <- as.data.frame(predictionsdiff1)
varImpPlot(rfdiff1)

#PR3
diff3$Class <- ifelse (diff3$Order > (185), "Test" , "Train" )
traindiff3 = diff3[diff3$Class == 'Train',]
testdiff3 = diff3[diff3$Class == 'Test',]
rfdiff3 = randomForest(PR3 ~ Interest_Rate2+Interest_Rate+ Interest_Rate3+Inflation+Interest_Rate1+M1_supply2+Lending_Housegolds_Growth2+KIX_index1+Order+Date+M1_supply3+Inflation1+KIX_index, mtry=13, ntree=600, nodesize=10, importance=TRUE, data = traindiff3)
predictionsdiff3 = predict(rfdiff3, newdata = testdiff3)
pred3 <- as.data.frame(predictionsdiff3)
varImpPlot(rfdiff3, n.var = 10)
#PR6
diff6$Class <- ifelse (diff6$Order > (182), "Test" , "Train" )
traindiff6 = diff6[diff6$Class == 'Train',]
testdiff6 = diff6[diff6$Class == 'Test',]
rfdiff6 = randomForest(PR6 ~ Interest_Rate1+Interest_Rate+Interest_Rate3+M1_supply2+Order+ M1_supply3+Interest_Rate2+M1_supply1+M1_supply+Date+Producer_Price+ KIX_index3+ Lending_Housegolds_Growth3, mtry=13, ntree=600, nodesize=10, importance=TRUE, data = traindiff6)
predictionsdiff6 = predict(rfdiff6, newdata = testdiff6)
pred6 <- as.data.frame(predictionsdiff6)
varImpPlot(rfdiff6, n.var = 10)
#PR12
diff12$Class <- ifelse (diff12$Order > (176), "Test" , "Train" )
traindiff12 = diff12[diff12$Class == 'Train',]
testdiff12 = diff12[diff12$Class == 'Test',]
rfdiff12 = randomForest(PR12 ~ M1_supply2+M1_supply+M1_supply1+Date+M1_supply3+Order+KIX_index1+Interest_Rate+ Lending_Housegolds_Growth+KIX_index3+Lending_Housegolds_Growth2+KPIF+Lending_Housegolds_Growth1+Year+CB_Balance_Sheet+KIX_index+Interest_Rate1+ Foreign_Exchange_Reserv2+Interest_Rate2+KPIF1+KIX_index2+CB_Balance_Sheet1+Unemployment3+ Interest_Rate3+Lending_Housegolds_Growth3+Unemployment+Construction_buildings3,mtry=13, ntree=600, nodesize=10, importance=TRUE, data = traindiff12)
predictionsdiff12 = predict(rfdiff12, newdata = testdiff12)
pred12 <- as.data.frame(predictionsdiff12)
varImpPlot(rfdiff12, n.var = 10)
pred1[1,]
pred3[1,]
pred6[1,]
pred12[1,]


#sammanställd
RF_EW <- read_excel("C:/Users/malte/OneDrive/Desktop/EW.xlsx")

RF_EW$Date <- seq(as.Date("2016-01-1"), as.Date("2020-02-1"), by = "month")

compRF_EW <- full_join(RF_EW, Inflation1, by="Date") #se till att ha inflation1 redo från tidigare

compRF_EW <- compRF_EW %>% slice(1:49)

mae(compRF_EW$PR1, compRF_EW$`1_MonthEW`)
mae(compRF_EW$PR1, compRF_EW$`3_MonthEW`)
mae(compRF_EW$PR1, compRF_EW$`6_MonthEW`)
mae(compRF_EW$PR1, compRF_EW$`12_MonthEW`)

compdiff6 <- pivot_longer(compdiff6, cols=c("predictionsdiff6", "PR1"), names_to = "Type", values_to="Value")
