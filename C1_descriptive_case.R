library(tidyverse)
library(psych)

# read data
data1 <- read.csv("newdata2.csv")
data2 <- read.csv("newdata4.csv")
data3 <- bind_rows(data1, data2)

descriptive <- select(data3, ls, free,  
                      age, female, married, widowed, unmarried,       
                       fulltime, parttime, selfemployed, 
                       nopaid, unemployed, income)

#descriptive statistics (overall)
describe(descriptive)[,c("mean","sd","median","min","max")]
nrow(descriptive)

