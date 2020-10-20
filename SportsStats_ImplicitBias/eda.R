library(tidyverse)
library(ggplot2)
library(vroom) 
library(skimr) 
library(visdat)

#load data
data_dir = 'data'
target_file = file.path(data_dir, 'CrowdstormingDataJuly1st.csv')

dataf_raw = vroom(target_file)
skim(dataf_raw)

#check for missing values
set.seed(2020-09-10)
dataf_smol = sample_n(dataf_raw, 1000)
vis_miss(dataf_smol, cluster = TRUE)

#set cleaned dataframe value
#remove missing values
#add rater_mean column
dataf = dataf_raw %>% 
  filter(!is.na(photoID),!is.na(rater1),!is.na(rater2)) %>% 
  mutate(rater_mean = rowMeans(select(.,rater1, rater2)))

#check for clean data
set.seed(2020-09-10)
dataf_smol = sample_n(dataf, 1000)
vis_miss(dataf_smol, cluster = TRUE)
  
#rater correlation
with(dataf, cor(rater1, rater2))
  
#red cards and race
with(dataf, cor(rater_mean, redCards))

#visualizing

#skin colors
ggplot(dataf) + 
  geom_bar(aes(x = rater_mean))

#
ggplot(dataf) + 
  geom_bar(aes(x = redCards))
ggplot(dataf) + 
  geom_bar(aes(x = yellowCards))
