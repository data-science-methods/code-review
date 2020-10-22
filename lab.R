#### Chaohong + Jung ####

## We perform an EDA to check whether darker skin players are more likely to receive red cards

#### Load packages & data ####
library(tidyverse)
library(skimr)
dat <- read.csv("CrowdstormingDataJuly1st.csv")

#### Skim the data ####
skim(dat)
## photoID, rater1, rater 2 have the same amount of missing values, which means we only have 1585 out of 2053 players being rated on their skin color.

### Omit missing values ####
dat_nona <- dat %>% 
  filter(!is.na(photoID),
         !is.na(rater1),
         !is.na(rater2))

#### Check agreement between raters ####
cor(dat_nona$rater1,dat_nona$rater2)
## Correlation coefficient is quite high, which means agreement between both raters is decent and the two variables are quite similar to each other. 

#### Average both ratings ####
dat_nona_rating <- dat_nona %>% 
  mutate(rating=(rater1+rater2)/2)

#### Visualize relationship between average rating and red cards
dat_nona_rating %>% 
  ggplot(aes(rating,redCards))+
  geom_bar(stat='identity')

#### Visualization across different countries ####
dat_nona_rating %>% 
  ggplot(aes(rating,redCards))+
  geom_bar(stat='identity')+
  facet_wrap(~leagueCountry)