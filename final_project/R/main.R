# Clear the environment
rm(list = ls())
if (is.null(dev.list()) == F) { dev.off() }

# Load packages
PACKAGES <- c("FrF2", "agricolae")
NEW_PACKAGES <- PACKAGES[!(PACKAGES %in% installed.packages()[,"Package"])]
if(length(NEW_PACKAGES)) install.packages(NEW_PACKAGES)
lapply(PACKAGES, require, character.only = TRUE)
rm(list = c("PACKAGES", "NEW_PACKAGES"))

# Define paths
PATH_GENERAL <- 'final_project/'
PATH_FUNCTIONS <- 'R/functions/'
PATH_DATA <- 'Data/'
PATH_OUTPUT <- 'Output/'

# Load sources
FUNCTIONS_LIST <- lapply(
  c('DataPreparation.R', 'CreateMultiBoxPlot.R', 'MapValues.R',
    'PlotAllInteractions.R', 'Pareto.R'), 
  function(x) paste0(PATH_GENERAL, PATH_FUNCTIONS, x)
)
lapply(FUNCTIONS_LIST, source)
rm(FUNCTIONS_LIST)


################################################################################
###############                   Data Loading                   ###############
################################################################################

FILE_NAME <- 'design_df_3'
DEP_VAR <- 'accuracy'
df <- loadData(FILE = paste0(PATH_GENERAL, PATH_DATA, FILE_NAME, '.csv'), 
               DEPENDENT_VARIABLE = DEP_VAR)
df_mapped <- mapValues(df)
VARS <- setdiff(colnames(df), DEP_VAR)


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
###############                Effects Analysis                  ###############
################################################################################

# Simple ANOVA
aov_main.df_mapped <- aov(accuracy ~ ., data = df_mapped)
summary(aov_main.df_mapped)

# Tukey Honest Significant Differences
tukey <- TukeyHSD(aov_main.df_mapped)
tukey

# Main effects
main_effects <- MEPlot(aov_main.df_mapped)


################################################################################
###############              Interaction Analysis                ###############
################################################################################

# Interaction plot
if (FALSE) {
  plotAllInteractions(df, RESPONSE_NAME = 'accuracy', 
                      OUT_PATH = paste0(PATH_GENERAL, PATH_OUTPUT))
}

# Daniel plot for all interactions
aov_allint.df_mapped <- aov(
  accuracy ~ n_estimators * min_samples_split *
    max_features * max_depth * criterion * bootstrap,
  data = df_mapped
)
summary(aov_allint.df_mapped)
dp_allint <- DanielPlot(aov_allint.df_mapped)
qqplot(dp_allint$x, dp_allint$y) 
qqline(dp_allint$y)

# Pareto plot for all interactions
pareto(dp_allint$x, names = dp_allint$effect)

# Daniel plot for double interactions
aov_doubleint.df_mapped <- aov(accuracy ~ (.)^2, data = df_mapped)
summary(aov_doubleint.df_mapped)
dp_doubleint <- DanielPlot(aov_doubleint.df_mapped)
qqplot(dp_doubleint$x, dp_doubleint$y) 
qqline(dp_doubleint$y)

# Pareto plot for double interactions
pareto(dp_doubleint$x, names = dp_doubleint$effect)


################################################################################
###############                      ANOVA                       ###############
################################################################################

# Final ANOVA according to previous interaction analysis
aov_final.df_mapped <- aov(
  accuracy ~ max_depth + n_estimators + min_samples_split:bootstrap + 
    min_samples_split + n_estimators:bootstrap + criterion + 
    min_samples_split:max_depth + n_estimators:min_samples_split + 
    max_depth:criterion,
  data = df_mapped
)
summary(aov_final.df_mapped)


################################################################################
###############                  Center Points                   ###############
################################################################################
