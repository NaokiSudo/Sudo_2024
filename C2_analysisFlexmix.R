set.seed(20220818)

library(dplyr); library(ggplot2); library(flexmix); library(psych)

## read the original dataset ##
data01 <- read.csv("newdata2.csv")
data02 <- read.csv("newdata4.csv")
data1 <- bind_rows(data01, data02)

# check distribution of life satisfaction
  g1 <- ggplot(data1, aes(ls))+
       geom_histogram(binwidth = 1.0, color="black", fill="blue", alpha=0.6)+  
       xlab("Life Satisfaction")+ylab("Count")+
       scale_x_continuous(breaks = seq(0.0, 11.0, by = 1.0))
  g1 

data1$GDP <- data1$GDP/1000
data1$age <- data1$age/100

data1 <- na.omit(data1)
  
# finite mixture of regression models (including control variables and independent variables)
M1 <- flexmix(ls~age+female+widowed+unmarried+
                parttime+selfemployed+nopaid+unemployed+
                income+free+GDP+LDI,
              data=data1, k = 1, 
              control = list(iter.max = 20000, verbose = 200))
result1 <- flexmix::refit(M1)
summary(result1) 

M2 <- flexmix(ls~age+female+widowed+unmarried+
                parttime+selfemployed+nopaid+unemployed+
                income+free+GDP+LDI,
              data=data1, k = 2, 
              control = list(iter.max = 20000, verbose = 200))
result2 <- flexmix::refit(M2)
summary(result2) 

  summary(M1)
  summary(M2)

  BIC(M1, M2)
  AIC(M1, M2)
  
  pred <- predict(M2, data1)
  data2 <-data.frame(data1, pred)

  data2$cl1 <- 0
  data2$cl2 <- 0
  
  data2$cl1[M2@cluster==1] <- 1
  data2$cl2[M2@cluster==2] <- 1
  
   
  describe(data2)[,c("mean", "sd", "min", "max")]

 #descriptive statistics (by each latent class)
  data3 <-subset(data2, M2@cluster==1) 
  data4 <-subset(data2, M2@cluster==2)

  describe(data3)[,c("mean", "sd", "min", "max")]
  describe(data4)[,c("mean", "sd", "min", "max")]
  nrow(data3)
  nrow(data4)

   #Weltch's test
    t.test(data3$ls,data4$ls,var.equal=F,paired=F)
    t.test(data3$free,data4$free,var.equal=F,paired=F)
    t.test(data3$age,data4$age,var.equal=F,paired=F)
    t.test(data3$female,data4$female,var.equal=F,paired=F)
    t.test(data3$married,data4$married,var.equal=F,paired=F)
    t.test(data3$widowed,data4$widowed,var.equal=F,paired=F)
    t.test(data3$unmarried,data4$unmarrie,var.equal=F,paired=F)
    t.test(data3$fulltime,data4$fulltime,var.equal=F,paired=F)
    t.test(data3$parttime,data4$parttime,var.equal=F,paired=F)
    t.test(data3$selfemployed,data4$selfemployed,var.equal=F,paired=F)
    t.test(data3$nopaid,data4$nopaid,var.equal=F,paired=F)
    t.test(data3$unemployed,data4$unemployed,var.equal=F,paired=F)
    t.test(data3$income,data4$income,var.equal=F,paired=F)
    t.test(data3$GDP,data4$GDP,var.equal=F,paired=F)
    t.test(data3$LDI,data4$LDI,var.equal=F,paired=F)

data5 <- group_by(data2, country, wave)
data6 <- data.frame(summarize(data5, cl1=mean(cl1), cl2=mean(cl2)))

write.csv(data6, "class_integrated_new.csv")



