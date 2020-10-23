#Question: Does skin color influence the rate at which players are given red cards? 

#load libraries
library(tidyverse)
library(ggplot2)
library(vroom) 
library(skimr) 
library(visdat)

#load data
data_dir = 'data'
target_file = file.path(data_dir, 'CrowdstormingDataJuly1st.csv')

#store raw data
dataf_raw = vroom(target_file)

#checking data
skim(dataf_raw)

#check for missing values
set.seed(2020-09-10)
dataf_smol = sample_n(dataf_raw, 1000)
vis_miss(dataf_smol, cluster = TRUE)

#set cleaned dataframe value, remove missing values, and add rater_mean column
dataf = dataf_raw %>% 
  filter(!is.na(photoID),!is.na(rater1),!is.na(rater2)) %>% 
  mutate(rater_mean = rowMeans(select(.,rater1, rater2)))

#check for clean data
set.seed(2020-09-10)
dataf_smol = sample_n(dataf, 1000)
vis_miss(dataf_smol, cluster = TRUE)
  
#check if raters are correlated (they are, r = 0.92)
with(dataf, cor(rater1, rater2))
  
#check if red cards and race correlated (at this level, does not appear so; r = 0.008)
with(dataf, cor(rater_mean, redCards))

#visualizing 

#number of individuals per rater_mean skin color score
ggplot(dataf) + 
  geom_bar(aes(x = rater_mean))

#table of skin color categories and corresponding red_card_ratios 
summaryf = dataf %>% 
  group_by(rater_mean) %>% 
  summarize(
    red_card_ratio = mean(redCards),
    yellow_card_ratio = mean(yellowCards)) %>% 
  ungroup()

#visualizing skin color vs. red_card_ratio
summaryf %>% 
  ggplot(aes(rater_mean, red_card_ratio)) + 
  geom_point()

#visualizing skin color vs. yellow_card_ratio
summaryf %>% 
  ggplot(aes(rater_mean, yellow_card_ratio)) + 
  geom_point() 
  
#correlation skin color to red_card_ratio
with(summaryf, cor(rater_mean, red_card_ratio))

#correlation skin color to yellow_card_ratio
with(summaryf, cor(rater_mean, yellow_card_ratio))

#Answer: We observed a weak correlation (r = 0.25) between the rate of red cards given and the skin color of players (as measured by the average of two independent rater's scores)

