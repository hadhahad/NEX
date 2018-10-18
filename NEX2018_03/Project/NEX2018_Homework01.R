# Clear the environment
rm(list = ls())

# Loading libraries
packages <- c("tidyverse", "data.table", "car")
lapply(packages, require, character.only = TRUE)
rm(packages)

# Setting the working directory
# setwd(".../NEX/NEX2018_03/Project")


# Loading source files
source("DataPreparation.R")

# Loading data
hit_data <- loadExperimentData("./data/experiment_data.csv", 
                               rename_blocks = TRUE, 
                               shuffle_rows = FALSE)

# Basic statistics
summary(hit_data)
