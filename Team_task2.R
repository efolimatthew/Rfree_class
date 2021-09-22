#Load required packages
library(tidyverse)

#Import data
data <- read.csv("Benefits.csv", header = T)
head(data)
View(data)
data <- data [ ,-1] #take out first column
View(data) 
str(data) #View data structure
dim(data)

#change data structure of some variables
glimpse(data)

#Change specific columns by their name to factor
data2 <- data %>% mutate(across(c(joblost, nwhite, school12, sex, 
                                  bluecol, smsa, married, dkids, 
                                  dykids, head, ui), factor))

class(data$joblost)
data$joblost <- as.factor(data$joblost)
class(data$joblost)

str(data)
#Exploratory_data_analysis using tidyverse