library(ggplot2); library(dplyr); library(plm); library(lmtest)
library(modelsummary); library(olsrr)

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
data5$GDP_old <- data5$GDP
data5$GDP <- log(data5$GDP)


data6 <- dplyr::select(data5, countryN, wave, ls, free, GDP, LDI, cl2, GDP_old)
data6 <- na.omit(data6)
data6 <- subset(data6, countryN!=347 & countryN!=352 & countryN!=359 & countryN!=373)

data7 <- pdata.frame(data6, index = c("countryN", "wave"))

p1 <- plm(ls ~ cl2,
          data7, model = "within", effects="twoways")
summary(p1)


p2 <- plm(ls ~ cl2+GDP+LDI,
          data7, model = "within", effects="twoways")
summary(p2)


p3 <- plm(ls ~ cl2+GDP+LDI+cl2:GDP+cl2:LDI+GDP:LDI,
          data7, model = "within", effects="twoways")
summary(p3)


p4 <- plm(ls ~ cl2+GDP+LDI+GDP:LDI,
          data7, model = "within", effects="twoways")
summary(p4)


modelsummary(list(p1, p2, p3, p4),fmt3=3, stars = T, gof_omit = 'DF|Deviance|R2')

ols_test_normality(p1$residuals)
ols_test_normality(p2$residuals)
ols_test_normality(p3$residuals)
ols_test_normality(p4$residuals)

eff1 <- marginaleffects::slopes(p3)                              
summary(eff1)

f1 <- marginaleffects::plot_slopes(p3, variables = "LDI", condition = "GDP")+
  xlab("GDP [log]")+ylab("Marginal Effect of Changes in LDI")+
 geom_hline(yintercept=0, linetype="dashed", color = "red")
f1

ggsave("f1.jpeg", dpi=320)

f2 <- marginaleffects::plot_slopes(p3, variables = "GDP", condition = "LDI")+
  xlab("LDI")+ylab("Marginal Effect of Changes in GDP")+
  geom_hline(yintercept=0, linetype="dashed", color = "red")
f2

ggsave("f2.jpeg", dpi=320)

ols_test_normality(data6$GDP_old)
ols_test_normality(data6$GDP)
