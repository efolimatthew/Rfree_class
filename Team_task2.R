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

anyNA(data)#no missing data

#change data structure of some variables
glimpse(data)

#Check the sixe of the dataset
object.size(data)
print(object.size(data), units = "Mb")

#The Mutate verb

#Change specific columns by their name to factor
data2 <- data %>% mutate(across(c(state, joblost, nwhite, school12, sex, 
                                  bluecol, smsa, married, dkids, 
                                  dykids, yrdispl, head, ui), factor))

levels(data2$state)
str(data2)

## On studying the data column 'bluecol', it seems to be redundant as all the workers are 'blue collar-ed'
data2 <- data2 %>% select(-(bluecol))
str(data2)
dim(data2) #reduced by 1

summary(as.integer(data2$yrdispl))
levels(as.factor(data2$tenure))
summary(data2$rr)
summary(data2$statemb)
sapply (data2, levels)

# Visualization
data2 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

data2 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

data2 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

data2 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")

#Exploratory_data_analysis using tidyverse
#It assumed that most states in the US determine 
#if UI benefits' applicants get their benefits or not despite federal law.
#Although, factors such as unemployment rate, 
#Is there any difference in the unemployment rate by state_code?
# filter the data by state code

## State with State_code 11
stat_code11 <- data2 %>% filter(state == 11) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code11 <- stat_code11 %>% select(-(state))# take out the state column as it is redundant
View(stat_code11) #the stateur varies with the year displacement yrdisp
dim(stat_code11)
stat_code11 %>% sapply(levels)
#What pattern determines if an individual get the UI benefits?

stat_code11 %>% count(sex, stat_code11$ui) #females were not denied
#Although that can not be proved for all categories of job lost as there were absent in the category "seasonal job ended"

#What gender mostly get denied the UI benefits?
stat_code11_sex <- stat_code11 %>% count(stat_code11$yrdispl,sex, ui)

#what was the marital status of the males denied of UI benefits?
stat_code11_married <- stat_code11 %>% count(stat_code11$yrdispl,sex, married, ui)

View(stat_code11_married)

# Visualization
stat_code11 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code11 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code11 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code11 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")

stat_code11$statemb
levels(data2$state)
levels(as.factor(data2$stateur))

#What were the state unemployment rates for 10 years in state_code11?
levels(as.factor(stat_code11$stateur))
#It was steadily rising. Possibly there are other factors determining the denial of this applicant's benefits.

#what was the state maximum benefits for each year? 
levels(as.factor(stat_code11$statemb))

## State with State_code 12
stat_code12 <- data2 %>% filter(state == 12) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code12 <- stat_code12 %>% select(-(state))# take out the state column as it is redundant

dim(stat_code12)
#What pattern determines if an individual get the UI benefits?

stat_code12 %>% count(sex, stat_code12$ui) #females were denied

# Visualization
stat_code12 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code12 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code12 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code12 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")


levels(data2$state)

## State with State_code 13
stat_code13 <- data2 %>% filter(state == 13) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code13 <- stat_code13%>% select(-(state))# take out the state column as it is redundant

dim(stat_code13)
#What pattern determines if an individual get the UI benefits?

stat_code13 %>% count(sex, stat_code13$ui) #females were denied

# Visualization
stat_code13 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code13 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code13 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code13 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")


## State with State_code 14
stat_code14 <- data2 %>% filter(state == 14) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code14 <- stat_code14%>% select(-(state))# take out the state column as it is redundant

dim(stat_code14)
#What pattern determines if an individual get the UI benefits?

stat_code14 %>% count(sex, stat_code14$ui) #females were denied

# Visualization
stat_code14 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code14 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code14 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code14 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")

## State with State_code 15
stat_code15 <- data2 %>% filter(state == 15) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code15 <- stat_code15%>% select(-(state))# take out the state column as it is redundant

dim(stat_code15)
#What pattern determines if an individual get the UI benefits?

stat_code15 %>% count(sex, stat_code15$ui) #females were denied

# Visualization
stat_code15 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code15 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code15 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code15 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")
levels(data2$state)

## State with State_code 16
stat_code16 <- data2 %>% filter(state == 16) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code16 <- stat_code16%>% select(-(state))# take out the state column as it is redundant

dim(stat_code16)
#What pattern determines if an individual get the UI benefits?

stat_code16 %>% count(sex, stat_code16$ui) #females were denied

# Visualization
stat_code16 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code16 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code16 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code16 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")

## State with State_code 21
stat_code21 <- data2 %>% filter(state == 21) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code21 <- stat_code15%>% select(-(state))# take out the state column as it is redundant

dim(stat_code21)
#What pattern determines if an individual get the UI benefits?

stat_code21 %>% count(sex, stat_code21$ui) #females were denied

# Visualization
stat_code21 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code21 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code21 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code21 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")

## State with State_code 15
stat_code22 <- data2 %>% filter(state == 22) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code22 <- stat_code22%>% select(-(state))# take out the state column as it is redundant

dim(stat_code22)
#What pattern determines if an individual get the UI benefits?

stat_code22 %>% count(sex, stat_code22$ui) #females were denied

# Visualization
stat_code22 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code22 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code22 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code22 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")


## State with State_code 23
stat_code23 <- data2 %>% filter(state == 23) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code23 <- stat_code23%>% select(-(state))# take out the state column as it is redundant

dim(stat_code23)
#What pattern determines if an individual get the UI benefits?

stat_code23 %>% count(sex, stat_code23$ui) #females were denied

# Visualization
stat_code23 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code23 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code23 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code23 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")


## State with State_code 31
stat_code31 <- data2 %>% filter(state == 31) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code31 <- stat_code31%>% select(-(state))# take out the state column as it is redundant

dim(stat_code31)
#What pattern determines if an individual get the UI benefits?

stat_code31 %>% count(sex, stat_code31$ui) #females were denied

# Visualization
stat_code31 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code31 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code31 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code31 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")


## State with State_code 32
stat_code32 <- data2 %>% filter(state == 32) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code32 <- stat_code32%>% select(-(state))# take out the state column as it is redundant

dim(stat_code32)
#What pattern determines if an individual get the UI benefits?

stat_code32 %>% count(sex, stat_code32$ui) #females were denied

# Visualization
stat_code32 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code32 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code32 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code32 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")

## State with State_code 15
stat_code33 <- data2 %>% filter(state == 33) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code33 <- stat_code33%>% select(-(state))# take out the state column as it is redundant

dim(stat_code33)
#What pattern determines if an individual get the UI benefits?

stat_code33 %>% count(sex, stat_code33$ui) #females were denied

# Visualization
stat_code33 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code33 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code33 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code33 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")

## State with State_code 34
stat_code34 <- data2 %>% filter(state == 34) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code34 <- stat_code34%>% select(-(state))# take out the state column as it is redundant

dim(stat_code34)
#What pattern determines if an individual get the UI benefits?

stat_code34 %>% count(sex, stat_code34$ui) #females were denied

# Visualization
stat_code34 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code34 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code34 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code34 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")


## State with State_code 15
stat_code35 <- data2 %>% filter(state == 35) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code15 <- stat_code15%>% select(-(state))# take out the state column as it is redundant

dim(stat_code35)
#What pattern determines if an individual get the UI benefits?

stat_code35 %>% count(sex, stat_code35$ui) #females were denied

# Visualization
stat_code35 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code35 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code35 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code35 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")


## State with State_code 41
stat_code41 <- data2 %>% filter(state == 41) %>%
  arrange(stateur) %>% group_by(joblost) 

stat_code41 <- stat_code41%>% select(-(state))# take out the state column as it is redundant

dim(stat_code41)
#What pattern determines if an individual get the UI benefits?

stat_code41 %>% count(sex, stat_code41$ui) #females were denied

# Visualization
stat_code41 %>% ggplot(mapping = aes(x = joblost, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on 'joblost' category")

stat_code41 %>% ggplot(mapping = aes(x = sex, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on Gender")

stat_code41 %>% ggplot(mapping = aes(x = married, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on marital status")

stat_code41 %>% ggplot(mapping = aes(x = yrdispl, fill = ui)) + 
  geom_bar(stat = "count") + ggtitle("UI applicants' status based on year of displacement")
##Having done an EDA to estimates the possible reason for the grant of UI benefits' application 
##A statistical modeling will be carried out to determine the effectiveness of the UI application system

# Select the numeric data
num_data <- data2 %>% select(stateur, statemb, state, age, tenure, rr)
str(num_data)
data2 %>% summary
num_data %>% summary

##Reference
#https://www.jstor.org/stable/1392373
#https://www.bls.gov/respondents/mwr/electronic-data-interchange/appendix-d-usps-state-abbreviations-and-fips-codes.htm