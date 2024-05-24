#This script takes the microporella images that have been segmented and 
#landmarked to filter them and add in metadat
#it also extracts linear trait measurements

#### ENVIRONMENT ----
source("Scripts/env.R")

#### LOAD DATA ---
filter.df <- read.csv("Data/image.filter.csv",
                      header = TRUE)