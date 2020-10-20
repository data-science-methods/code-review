library(tidyverse)
library(ggplot2)
library(gvlma)
library(skimr)

#load in data
dataf <- read_csv(file.path('data', 'CrowdstormingDataJuly1st.csv'))

skim(data)

#omit missing data
#create average rating of player skin color
dataf2 <- dataf %>% 
  na.omit %>% 
  mutate(rater_average = ((rater1 + rater2) / 2))

skim(dataf2)

#predicting yellow cards based on skin color
#plotting
ggplot(dataf2, aes(rater_average, yellowCards)) +
  geom_point(position = 'jitter')

#creating the model
m <- lm(yellowCards ~ rater_average, dataf2)
gvlma(m) %>% summary
m %>% summary

