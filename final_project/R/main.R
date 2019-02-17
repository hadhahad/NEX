# Clear the environment
rm(list = ls())
if (is.null(dev.list()) == F) {
  dev.off()
}

# Load packages
PACKAGES <- c("FrF2", "agricolae", "nortest", "lmtest", "caret", "plotly")
NEW_PACKAGES <- PACKAGES[!(PACKAGES %in% installed.packages()[, "Package"])]
if (length(NEW_PACKAGES)) install.packages(NEW_PACKAGES)
lapply(PACKAGES, require, character.only = TRUE)
rm(list = c("PACKAGES", "NEW_PACKAGES"))

# Define paths with an assumption that the working directory
# is one step above
PATH_GENERAL <- "final_project/"
PATH_FUNCTIONS <- "R/functions/"
PATH_DATA <- "Data/"
PATH_OUTPUT <- "Output/"

# Load sources
FUNCTIONS_LIST <- lapply(
  c(
    "DataPreparation.R", "CreateMultiBoxPlot.R", "MapValues.R",
    "PlotAllInteractions.R", "Pareto.R", "ToFactor.R"
  ),
  function(x) paste0(PATH_GENERAL, PATH_FUNCTIONS, x)
)
lapply(FUNCTIONS_LIST, source)
rm(FUNCTIONS_LIST)


################################################################################
###############                   Data Loading                   ###############
################################################################################

FILE_NAME <- "design_df_3"
DEP_VAR <- "accuracy"

df <- loadData(
  FILE = paste0(PATH_GENERAL, PATH_DATA, FILE_NAME, ".csv"),
  DEPENDENT_VARIABLE = DEP_VAR
)
df_center <- loadData(
  FILE = paste0(PATH_GENERAL, PATH_DATA, FILE_NAME, "_center", ".csv"),
  DEPENDENT_VARIABLE = DEP_VAR
)
df_all <- rbind(df, df_center)

df_mapped <- toFactor(mapValues(df), DEP_VAR)
df_mapped_center <- toFactor(mapValues(df_center), DEP_VAR)
df_mapped_all <- toFactor(rbind(df_mapped, df_mapped_center), DEP_VAR)


################################################################################
###############                Visual Analysis                   ###############
################################################################################

if (FALSE) {
  createMultiBoxPlot(
    df = df_mapped, DEP_VAR = DEP_VAR,
    OUT_PATH = paste0(PATH_GENERAL, PATH_OUTPUT),
    PLOT_NAME = paste0("box_plot_all_", FILE_NAME)
  )
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
  plotAllInteractions(df_mapped,
    RESPONSE_NAME = DEP_VAR,
    OUT_PATH = paste0(PATH_GENERAL, PATH_OUTPUT)
  )
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

# Box plot visualization of center points
if (FALSE) {
  createMultiBoxPlot(
    df = df_mapped_all[, c(
      "n_estimators",
      "min_samples_split",
      "max_depth",
      DEP_VAR
    )],
    DEP_VAR = DEP_VAR,
    OUT_PATH = paste0(PATH_GENERAL, PATH_OUTPUT),
    PLOT_NAME = paste0("box_plot_center_", FILE_NAME)
  )
}

# Create a design table with center points
# df_design <- FrF2(64, 6,
#                   factor.names = rev(setdiff(colnames(df_mapped), DEP_VAR)),
#                   randomize = FALSE)
# df_design <- add.center(df_design, 8)
# df_design <- add.response(df_design, df_all$accuracy)
# names(df_design)[length(colnames(df_design))] <- DEP_VAR

lm.center <- lm(accuracy ~ I(n_estimators^1) +
  I(min_samples_split^1) +
  I(max_depth^1), data = df_all)
summary(lm.center)
op <- par(mfrow = c(2, 2))
plot(lm.center)
par(op)


################################################################################
###############               Linear Regression                  ###############
################################################################################

# New data frame just not to mess with the main one
df_fit <- df_all

# Fitting the linear model
lm.numeric_empirical <-
  lm(log(accuracy) ~ I((n_estimators - median(n_estimators))^2) +
    I((max_depth - median(max_depth))^3), data = df_fit)
summary(lm.numeric_empirical)
lm.numeric <-
  lm(accuracy ~ poly(n_estimators, 1) +
    poly(max_depth, 2), data = df_fit)
summary(lm.numeric)

# Plot of the linear model
op <- par(mfrow = c(2, 2))
plot(lm.numeric)
par(op)

# Residual tests to test normality of residuals
lillie.test(residuals(lm.numeric))
shapiro.test(residuals(lm.numeric))

# Heteriscedasticity analysis
bptest(lm.numeric)

# Function to evaluate the fit function on the grid
vals_xy <- function(x, y, model = lm.numeric) {
  new_data <- data.frame("n_estimators" = x, "max_depth" = y)
  return(predict(model, new_data))
}

# Set up the grid
x <- seq(5, 505, length.out = 150)
y <- seq(5, 45, length.out = 150)
z <- outer(X = x, Y = y, FUN = vals_xy)

# Plot via plotly
p <- plot_ly(x = x, y = y, z = z) %>%
  add_surface() %>%
  add_markers(
    x = df_fit$n_estimators,
    y = df_fit$max_depth,
    z = df_fit$accuracy, marker =
      list(
        color = df_fit$accuracy,
        colorscale = c("#FFE1A1", "#683531"), showscale = FALSE
      )
  )
chart_link <- api_create(p, filename = "surface-2")
chart_link
