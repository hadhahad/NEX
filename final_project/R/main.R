# Clear the environment
rm(list = ls())
if (is.null(dev.list()) == F) { dev.off() }

# Define paths
PATH_GENERAL <- 'final_project/'
PATH_FUNCTIONS <- 'R/functions/'
PATH_DATA <- 'Data/'
PATH_OUTPUT <- 'Output/'

# Load sources
FUNCTIONS_LIST <- lapply(
  c('DataPreparation.R', 'CreateMultiBoxPlot.R', 'MapValues.R',
    'PlotAllInteractions.R'), 
  function(x) paste0(PATH_GENERAL, PATH_FUNCTIONS, x)
)
lapply(FUNCTIONS_LIST, source)
rm(FUNCTIONS_LIST)


################################################################################
###############                   Data Loading                   ###############
################################################################################

FILE_NAME <- 'design_df_3'
df <- loadData(paste0(PATH_GENERAL, PATH_DATA, FILE_NAME, '.csv'), 'accuracy')
df_mapped <- mapValues(df)

################################################################################
###############                Visual Analysis                   ###############
################################################################################

if (FALSE) {
  createMultiBoxPlot(df = df, DEP_VAR = 'accuracy', 
                     OUT_PATH = paste0(PATH_GENERAL, PATH_OUTPUT),
                     PLOT_NAME = paste0("box_plot_all", FILE_NAME),
                     PRINT_PLOT = FALSE)
}


################################################################################
###############              Interaction Analysis                ###############
################################################################################

if (FALSE) {
  plotAllInteractions(df, RESPONSE_NAME = 'accuracy', 
                      OUT_PATH = paste0(PATH_GENERAL, PATH_OUTPUT))
}
