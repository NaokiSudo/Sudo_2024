library(tidyverse)

data1 <- read.csv("EVS_panel2.csv")
data1 <- select(data1, -cl1, -cl2)

data2 <- read.csv("dataC_integrated_wvs.csv")

data3 <- bind_rows(data1, data2)
data3$id <- data3$countryN*10+data3$wave

data4 <- read.csv("class_integrated_new.csv")
data4$id <- data4$country*10+data4$wave
data4 <- select(data4, id, cl1, cl2)

data5 <- left_join(data3, data4, by="id")

data5$LDI <- data5$v2x_libdem
data5$GDP <- log(data5$GDP)

data6 <- select(data5, countryN, wave, year, ls, free, GDP, LDI, cl2)
data6 <- na.omit(data6)
data6 <- subset(data6, countryN!=347 & countryN!=352 & countryN!=359 & countryN!=373)

data0 <- read.csv("countrycode.csv")

data7 <- select(data6, countryN, wave, year, ls, free, GDP, LDI, cl2)
data7 <- na.omit(data7)
data20 <- filter(data7, countryN!=347 & countryN!=352 & countryN!=359 & countryN!=373)

data20 <- group_by(data20, countryN, wave)
data20 <- data.frame(summarise(data20, mean(year)))

data21 <- left_join(data20, data0, by="countryN")

data22 <- spread(data21, wave, mean.year.)
colnames(data22) <- c("Country ID","Name", "Wave1", "Wave2", "Wave3", "Wave4", "Wave5", "WVS")
data22 <- data22[order(data22$Name),]
data22


