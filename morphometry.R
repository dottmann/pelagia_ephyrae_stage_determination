
#   Ephyrae morphometry

# Author: Daniel Ottmann
# Created: April 2021
# Last update: April 2021


###########################################
#       Readme

# This script evaluates the morpohmetry of live and preserved ephyrae and metaephyrae collected around the Balearic Islands in 2019


#######################################################################################################################


##################################################################
# Set the working directory
setwd("C:/Users/danie/Google Drive/_phd/Projects/ephyrae/pelagia_ephyrae_stage_determination/")


#############################
# Load packages:
library(tidyverse)


#############################################################
# Load the data

data <- read.delim('in/data_morphometrics.txt', sep = '\t', header = T, stringsAsFactors = F, dec = ".")


# Edit data frame and get table of min, max, mean and SD for each measurement and stage:
data %>%
  group_by(measurement, stage) %>%
  summarise(n = n(),
            mean_tbd = mean(size_mm),
            sd_tbd = sd(size_mm),
            min_tbd = min(size_mm),
            max_tbd = max(size_mm))




  
  
  