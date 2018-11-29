# Clear the environment
rm(list = ls())

# Set up the working directory
setwd(paste0(getwd(), '/NEX2018_08/Project/R'))

# Load source files 
source('functions/DataPreparation.R')
source('functions/CreateMultiBoxPlot.R')
source('functions/MapColNames.R')


################################################
##########          LOAD DATA         ##########
################################################

# Load the dataset
data_all <- loadData(FILE = '../Data/experimental_data.csv')

# Create a data frame with original values

# Separate center points
MAIN_IDX    <- seq(1, 64)
data        <- data_all[MAIN_IDX, ]
data_center <- data_all[-MAIN_IDX, ]

# Remove unnecessary data and variables from the environment
rm(list = setdiff(ls(), c('data', 'data_center', lsf.str())))


################################################
##########    BASIC VISUAL ANALYSIS   ##########
################################################

boxplot_all_vars <- createMultiBoxPlot(
  df = data,
  OUT_PATH = "figures/",
  PLOT_NAME = "boxplot_all_vars",
  PRINT_PLOT = FALSE
)
