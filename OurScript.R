## Loading Necessary Libraries ----
library(tidyverse)
library(skimr)
library(visdat)
library(ggplot2)

## Path to data (can be changed by user) 
data_dir <- "data"
target_file <- file.path(data_dir,"CrowdstormingDataJuly1st.csv")

## Load data
soccer_data <- read_csv(target_file)

## Skim the data ----
skim(soccer_data)

#' The data shows that we there is 28 variable, but the README file only mention 27. I appears that
#' "ALPHA_3" is the additional variable on the dataset. It is a character variable with 160 unique abservations
#' in it. 


## Visualization of missing data
vis_miss(soccer_data, warn_large_data = FALSE)

## Visualization of smaller dataset
set.seed(2020-10-22)
soc_small <- sample_n(soccer_data, 15000)

vis_miss(soc_small,
         cluster = T,
         sort_miss = T)

#' From this we can see that observation for variables photo ID, rater1 and rater2 are missing together. This makes sense 
#' as the raters need the players' pictures to rate their skin tones.

## Plots ----
plt_viol <- ggplot(data = soccer_data, 
                   aes(leagueCountry, 
                       meanIAT, 
                       fill = leagueCountry)) +
  geom_violin() +
  labs(title = "Mean Implicit Bias Score by Country League", 
       subtitle = "IAT scores for referrees' country of origin",
       y = "Mean IAT Score",
       x = "European League Country")

plt_viol + theme(axis.text = element_text(size = 12), 
            axis.title = element_text(size = 14, 
                                      face="italic"))

#' The plot above seems to show that Spain has a higher tendency than the other countries in the league 
#' to associate white with good and black with bad. However this visualization might not be the best.
#' Let's looks at histograms per Country

plt_hist <- ggplot(data = soccer_data, 
                   aes(x = meanIAT, 
                       color= leagueCountry, 
                       fill = leagueCountry)) + 
  geom_histogram() +
  labs(title = "Mean Implicit Bias Score by Country League", 
       subtitle = "IAT scores for referrees' country of origin",
       x = "Mean IAT Score")

plt_hist + theme(axis.text = element_text(size = 12), 
            axis.title = element_text(size = 14, 
                                      face="italic"))
  
plt_hist + facet_wrap(vars(leagueCountry)) 

#' This view is better. Although Spain has the higher IAT mean, it is also the country with the less observations
#' Let's look at the relationship between implicit and explicit bias 
                 
plt_point <- ggplot(data = soccer_data, 
                    aes(meanIAT, 
                        meanExp, 
                        color = leagueCountry)) +
  geom_point() + labs(title = "Mean Implicit Bias Score Vs Mean Explicit Bias Score", 
                      subtitle = "IAT and racial thermometer scores for referrees' country of origin",
                      y = "Mean Racial Thermometer Score",
                      x = "Mean IAT Score")

plt_point + theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14, 
                                      face="italic"))    

#' There seem to be a strong positive correlation between implicit and explicit bias in the league.
#' We can look at that association per Country:

plt_point + facet_wrap(vars(leagueCountry)) +
  geom_smooth(method = "lm", 
              col = "firebrick")

#' The plots look very similar between countries, almost identical (except for a few observations)...




soccer_data %>% 
  select(meanIAT,meanExp) %>% 
  vis_cor()


soccer_data %>% 
  filter(rater1 > .5 & rater2 > .5) %>% 
  select(meanIAT,meanExp, redCards, yellowCards) %>% 
  vis_cor()
  
  count(leagueCountry) 





smallData <- data %>%  
  select(club, leagueCountry, goals, victories, defeats, redCards, yellowCards, ties) 

smallData %>% 
  select(goals, victories, defeats, redCards, yellowCards, ties) %>% 
  vis_cor()

smallData %>% 
  group_by(leagueCountry) %>% 
  summarize(n = n(),
            across(.cols = redCards,
                   .fns = lst(mean, sd)))

## EDA 








