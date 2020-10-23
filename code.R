#  load libraries; plyr must be loaded before tidyverse because plyr masks the count function from dplyr
library(plyr)
library(tidyverse)
library(ggplot2)
library(skimr)

#  trying to use the Peng & Matsui EDA checklist, minus the steps that are not applicable

## 1 formulate question ----
## we are examining whether player skin color predicts the frequency of getting yellow carded

## 2 load in data ----
dataf <- read_csv(file.path('data', 'CrowdstormingDataJuly1st.csv'))

## 4 look at top & bottom of data ----
skim(dataf)
head(dataf)

#  omit missing data
#  create average rating of player skin color
dataf2 <- dataf %>% 
  na.omit %>% 
  mutate(rater_average = ((rater1 + rater2) / 2)) 

skim(dataf2)
count(dataf2, playerShort)
#  1419 unique players
count(dataf2, rater_average)
#  very uneven distribution of skin colors in foosball (it's the devil!)

#  creating a new df that combines rows based on player name 
#  (so some players aren't overrepresented in the analysis based on how long they've been playing),
#  that player's mean number of yellow cards per game, 
#  and that player's rated skin color
dataf3 <- ddply(dataf2, .(playerShort), summarise, mean_YC = mean(yellowCards))
dataf4 <- dataf2 %>% distinct(playerShort, rater_average)
dataf5 <- merge(dataf3, dataf4, by = c("playerShort"))
#  probably a way to do all this in one step, but we don't know how

count(dataf5, rater_average)
#still uneven distribution of skin colors, but more manageable numbers now

#creating a new df that averages across skin color rating
dataf6 <- ddply(dataf2, .(rater_average), summarise, mean_YC = mean(yellowCards))

## 7 make a plot ----

#  plotting the raw number of yellow cards against the player skin color rating, just to see what it looks like
ggplot(dataf2, aes(rater_average, yellowCards)) +
  geom_point(position = 'jitter')
#  looks like thanos snapped his hand and all the data is disintegrating
#  also illustrates the problem noted earlier with count(dataf2, rater_average) and the proportion of skin colors across the players


#  plotting the average number of YCs for each skin color rating average
ggplot(dataf6, aes(rater_average, mean_YC)) +
  geom_point()
#  well, this makes it look like nothing interesting is going on


## 8 try the easy solution first ----
#  chi squared test to see if the average yellow cards differs across the skin color rating categories
chisq.test(dataf6)
#  sure enough, non-significant finding; we cannot reject the assumption that these two variables are independent
#  maybe some other stuff is going on with variables like leagueCountry or refCountry, but as a first pass this suggests no relationship




