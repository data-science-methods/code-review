library(tidyverse)
library(ggplot2)
library(vroom) 
library(skimr) 
library(visdat)
library(tictoc) 

#load data
data_dir = 'data'
target_file = file.path(data_dir, 'CrowdstormingDataJuly1st.csv')
tic()
read.csv(target_file)
toc()
dataf_raw = vroom(target_file)
skim(dataf_raw)

#checking for missing values
set.seed(2020-09-10)
dataf_smol = sample_n(dataf_raw, 1000)
vis_miss(dataf_smol, cluster = TRUE)

#remove missing values
dataf = dataf_raw %>% 
  filter(!is.na(photoID),!is.na(rater1),!is.na(rater2)) %>% 
  mutate(rater_mean = rowMeans(dataf[,c('rater1', 'rater2')]))

#check for clean data
  set.seed(2020-09-10)
  dataf_smol = sample_n(dataf, 1000)
  vis_miss(dataf_smol, cluster = TRUE)
  
#rater correlation
  with(dataf, cor(rater1, rater2))
  
#red cards and race
  with(dataf, cor(rater_mean, redCards))
  ggplot(dataf, aes(rater_mean, redCards)) +
    geom_point() 
