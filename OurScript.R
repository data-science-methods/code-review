library(tidyverse)
library(skimr)
library(visdat)

data = read_csv("1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")


skim(data)

hist(data$rater1)
hist(data$rater2)

vis_miss(data, warn_large_data = FALSE)


