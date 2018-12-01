# Clear the environment
rm(list = ls())

# Load libraries
PACKAGES <- c('FrF2', 'lattice', 'pid', 'tidyverse', 'nortest')
lapply(PACKAGES, require, character.only = TRUE)
rm(PACKAGES)

# Set up the working directory
setwd(paste0(getwd(), '/NEX2018_08/Project/R'))

# Load source files 
source('functions/DataPreparation.R')
source('functions/CreateMultiBoxPlot.R')
source('functions/MapColNames.R')
source('functions/PlotAllInteractions.R')


################################################
##########          LOAD DATA         ##########
################################################

# Load the dataset
data_all <- loadData(FILE = '../Data/experimental_data.csv')

# Create a data frame with original values

# Separate center points
MAIN_IDX     <- seq(1, 64)
data_noncent <- data_all[MAIN_IDX, ]
data_center  <- data_all[-MAIN_IDX, ]

# Remove unnecessary data and variables from the environment
rm(list = setdiff(ls(), c('data_noncent', 'data_center', 'data_all', lsf.str())))


################################################
##########    BASIC VISUAL ANALYSIS   ##########
################################################

boxplot_all_vars <- createMultiBoxPlot(
  df = data,
  OUT_PATH = "figures/",
  PLOT_NAME = "boxplot_all_vars",
  PRINT_PLOT = FALSE
)

################################################
###############    EFFECTS   ###################
################################################

data_noncent.aov_simple <- aov(measurement ~ ., data = data_noncent)
summary(data_noncent.aov_simple)
MEPlot(data_noncent.aov_simple)


################################################
#############    INTERACTIONS   ################
################################################

plotAllInteractions(df = data_noncent, 
                    RESPONSE_NAME = 'measurement', 
                    OUT_PATH = 'figures/')

data_design <- data2design(data_noncent[, seq(1:6)], quantitative = c(F, F, F, F, F, F))
data_design <- add.response(data_design, data_noncent$measurement)
class(data_design)

# Daniel plot
data_noncent.aov_allint <- aov(measurement ~ mass*distance*filling*hand*vision*stance, 
                               data = data_noncent)
summary(data_noncent.aov_allint)
qqplot(DanielPlot(data_noncent.aov_allint)$x, 
       DanielPlot(data_noncent.aov_allint)$y) 
qqline(DanielPlot(data_noncent.aov_allint)$y)

data_noncent.aov_doubleint <- aov(measurement ~ (mass + distance + filling + hand + vision + stance)^2, 
                                  data = data_noncent)
summary(data_noncent.aov_doubleint)
qqplot(DanielPlot(data_noncent.aov_doubleint)$x, 
       DanielPlot(data_noncent.aov_doubleint)$y) 
qqline(DanielPlot(data_noncent.aov_doubleint)$y)

# Pareto chart
paretoPlot(data_noncent.aov_allint)


################################################
################    ANOVA   ####################
################################################

prefinal.aov <- aov(lm(measurement ~ distance + mass:distance:filling:stance + 
                         mass:distance:hand:stance + filling:hand:vision:stance + 
                         mass:distance:filling:vision:stance + distance:hand:stance + 
                         mass:distance:stance + distance:hand:vision:stance + 
                         distance:filling + mass:hand + distance:filling:vision + 
                         hand:stance + vision:stance + distance:vision:stance + 
                         distance:filling:stance - 1, data = data_noncent))
summary(prefinal.aov)

final.aov <- aov(lm.default(formula = measurement ~ distance + 
                              stance:vision - 1, data = data_noncent))
summary(final.aov)


final.aov <- aov(lm(measurement ~ distance + distance:filling:stance - 1, data = data_noncent))
summary(final.aov)

################################################
#############    CENTER POINTS   ###############
################################################

b1 <- createSingleBoxPlot(mapColNames(data_all, "mass"), 1, 7, "Mass, [g]", "Measurement, [mm]",
  "Mass with Center Points",
  PRINT_PLOT = TRUE
)

b2 <- createSingleBoxPlot(mapColNames(data_all, "distance"), 2, 7, "Distance, [m]", "Measurement, [mm]",
  "Distance with Center Points",
  PRINT_PLOT = TRUE
)
plot_final <- grid.arrange(b1, b2, ncol = 2)
ggsave(
  filename = "figures/boxplot_center_points.png",
  plot = plot_final,
  width = 170, height = 115, units = "mm"
)

center.lm <- lm(measurement ~ mass + distance - 1, data = data_all)
summary(center.lm)
center.aov <- aov(center.lm)
summary(center.aov)


################################################
##########    LINEAR REGRESSION   ##############
################################################

data_all_num <- mapColNames(data_all, c('mass', 'distance'))
data_all_num <- data_all_num %>% 
  mutate(
    mass = as.numeric(as.character(mass)),
    distance = as.numeric(as.character(distance))
  )

final.lm_num <- lm(measurement ~ mass + distance - 1, data = data_all_num)
summary(final.lm_num)

lillie.test(residuals(final.lm_num))
shapiro.test(residuals(final.lm_num))

par(mfrow = c(2, 2))
plot(final.lm_num)


################################################
############    CONTOUR PLOT    ################
################################################

# contourPlot(final.lm_num, N = 25)
new_data <- data.frame(
  'mass'     = seq(40, 110, length.out = 200),
  'distance' = seq(2, 6, length.out = 200)
)
new_data <- expand.grid(new_data)
predictions <- predict(final.lm_num, new_data)
new_data$measurement <- predictions

contour_plot <- ggplot(new_data, aes(mass, distance, z = measurement)) + 
  geom_raster(aes(fill = measurement)) +
  geom_contour(colour = "white", binwidth = 20) +
  labs(title = 'Contour Plot for Mass and Distance',
       fill = 'Measurement') +
  xlab('Mass') +
  ylab('Distance')
