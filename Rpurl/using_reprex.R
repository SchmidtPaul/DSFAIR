knitr::opts_chunk$set(echo = TRUE,
                      warning = FALSE,
                      message = FALSE)

## library(dplyr)
## 
## PlantGrowth %>%
##   group_by(group) %>%
##   summarise(mean = mean(weight))
## 
## model <- lm(weight = group, data = PlantGrowth)
## 
## anova(model)

library(dplyr)

mydata <- PlantGrowth

mydata %>% 
  group_by(group) %>% 
  summarise(mean = mean(weight))

model <- lm(weight = group, data = mydata)

anova(model)

mydata <- PlantGrowth

model <- lm(weight = group, data = mydata)
