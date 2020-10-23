## Loading Necessary Libraries
library(tidyverse)
library(skimr)
library(visdat)
library(ggplot2)


## Load data
data = read_csv("CrowdstormingDataJuly1st.csv")

## Skim the data
skim(data)

## Visualization of the data
vis_miss(data, warn_large_data = FALSE)


smallData <- data %>%  
  select(club, leagueCountry, goals, victories, defeats, redCards, yellowCards, ties) 

smallData %>% 
  select(goals, victories, defeats, redCards, yellowCards, ties) %>% 
  vis_cor()

smallData %>% 
  group_by(leagueCountry) %>% 
  summarize(n = n(),
            across(.cols = redCards,
                   .fns = lst(mean, sd),
                   aov() ))



hist(data$rater1)
hist(data$rater2)




