library(tidyverse)   # for working with the data
library(lubridate)   # for working with datetime data
library(vroom)       # for quickly reading large CSV files
library(skimr)       # generate a text-based overview of the data
library(visdat)      # generate plots visualizing data types and missingness
library(tictoc)      # for timing how long R takes to do the thing
library(ggplot2)

# Load the data 
data <- read.csv("CrowdstormingDataJuly1st.csv")

# Inspect the data 
skim(data)

# Inspect the redCards
table(data$redCards)

# Correclation between two skin tone raters 
cor(data$rater1, data$rater2, use = "pairwise.complete.obs")

# Correlation between red-cards and skin tone raters
cor(data$rater1, data$redCards, use = "pairwise.complete.obs")
cor(data$rater2, data$redCards, use = "pairwise.complete.obs")

ggplot(data, aes(rater1, redCards))+ geom_point()
