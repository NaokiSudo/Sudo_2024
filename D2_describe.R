library(dplyr); library(psych)

data1 <- read.csv("EVS_panel2.csv")
data1 <- select(data1, -cl1, -cl2)

data2 <- read.csv("dataC_integrated_wvs.csv")

data3 <- bind_rows(data1, data2)
data3$id <- data3$countryN*10+data3$wave

data4 <- read.csv("class_integrated_new.csv")
data4$id <- data4$country*10+data4$wave
data4 <- select(data4, id, cl1, cl2)

data5 <- left_join(data3, data4, by="id")
data6 <- select(data5, ls, free, v2x_libdem, GDP, cl2)

describe(data6, skew = F)

cord <- corr.test(data6)
round(cord$r,3)
round(cord$p,3)

