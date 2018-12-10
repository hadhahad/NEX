# Clear the environment
rm(list = ls())

# Load libraries
PACKAGES <- c('FrF2', 'lattice', 'pid', 
              'tidyverse', 'nortest', 'lmtest', 'caret')
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

#Creating of boxplots
boxplot_all_vars <- createMultiBoxPlot(
  df = data,
  OUT_PATH = "figures/",
  PLOT_NAME = "boxplot_all_vars",
  PRINT_PLOT = FALSE
)

################################################
###############    EFFECTS   ###################
################################################

#one-way ANOVA test that tests if variables from the data_noncent 
#have same mean values or not. Output values of the test are
#significant variable for this dataset

data_noncent.aov_simple <- aov(measurement ~ ., data = data_noncent)

#summary of the test
summary(data_noncent.aov_simple)

#Mean excess plots. These plots show dependency between mean  
#values of different variables and measurements

MEPlot(data_noncent.aov_simple)


################################################
#############    INTERACTIONS   ################
################################################

#Function for plotting and saving all interaction 
#with respect to measurements

plotAllInteractions(df = data_noncent, 
                    RESPONSE_NAME = 'measurement', 
                    OUT_PATH = 'figures/')

#Converting to another format for applying Daniel plot function
data_design <- data2design(data_noncent[, seq(1:6)], quantitative = c(F, F, F, F, F, F))
data_design <- add.response(data_design, data_noncent$measurement)
class(data_design)

#Creating Daniel plot that represents significance of some interactions
#Labeled interactions are significant

data_noncent.aov_allint <- aov(measurement ~ mass*distance*filling*hand*vision*stance, 
                               data = data_noncent)
summary(data_noncent.aov_allint)
qqplot(DanielPlot(data_noncent.aov_allint)$x, 
       DanielPlot(data_noncent.aov_allint)$y) 
qqline(DanielPlot(data_noncent.aov_allint)$y)

#Creating Daniel plot with respect to single variables and double interactions
data_noncent.aov_doubleint <- aov(measurement ~ (mass + distance + filling + hand + vision + stance)^2, 
                                  data = data_noncent)
summary(data_noncent.aov_doubleint)
qqplot(DanielPlot(data_noncent.aov_doubleint)$x, 
       DanielPlot(data_noncent.aov_doubleint)$y) 
qqline(DanielPlot(data_noncent.aov_doubleint)$y)

#Plotting Pareto chart that also if specific interactions are significant 
#or not. The higher the values, the more significant interaction is

paretoPlot(data_noncent.aov_allint)


################################################
################    ANOVA   ####################
################################################

#Creating and investigating model with different interaction that were 
#chosen with respect to Daniel and Pareto plots

prefinal.aov <- aov(lm(measurement ~ distance + mass:distance:filling:stance + 
                         mass:distance:hand:stance + filling:hand:vision:stance + 
                         mass:distance:filling:vision:stance + distance:hand:stance + 
                         mass:distance:stance + distance:hand:vision:stance + 
                         distance:filling + mass:hand + distance:filling:vision + 
                         hand:stance + vision:stance + distance:vision:stance + 
                         distance:filling:stance - 1, data = data_noncent))
summary(prefinal.aov)
#Final model
final.aov <- aov(lm.default(formula = measurement ~ distance + 
                              distance:stance:vision - 1, data = data_noncent))
summary(final.aov)

# not Final model
#final.aov <- aov(lm(measurement ~ distance + distance:filling:stance - 1, data = data_noncent))
#summary(final.aov)

################################################
#############    CENTER POINTS   ###############
################################################

#Boxplots of variables with center points

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

#Linear model of measurement that depends on mass and distance
#without intercept

center.lm <- lm(measurement ~ mass + distance - 1, data = data_all)
summary(center.lm)
center.aov <- aov(center.lm)
summary(center.aov)


################################################
##########    LINEAR REGRESSION   ##############
################################################

#Linear regression with mapping to numeric values
data_all_num <- mapColNames(data_all, c('mass', 'distance'))
data_all_num <- data_all_num %>% 
  mutate(
    mass = as.numeric(as.character(mass)),
    distance = as.numeric(as.character(distance))
  )

#The final linear model
final.lm_num <- lm(measurement ~ mass + distance - 1, data = data_all_num)
summary(final.lm_num)

#Residual tests to test normality of residuals
lillie.test(residuals(final.lm_num))
shapiro.test(residuals(final.lm_num))

#Heteriscedasticity analysis
bptest(final.lm_num)

# Box-Cox transformation
bc_transf <- BoxCoxTrans(data_all_num$measurement)
data_all_num$measurement_bc <- predict(bc_transf, data_all_num$measurement)

#Plotting summary of the linear regression model
par(mfrow = c(2, 2))
plot(final.lm_num)

#The FINAL final linear model with Box-Cox transformation
final_bc.lm_num <- lm(measurement_bc ~ mass + distance - 1, data = data_all_num)
summary(final_bc.lm_num)

# Once again normality tests of residuals
lillie.test(residuals(final_bc.lm_num))
shapiro.test(residuals(final_bc.lm_num))

#Once again eteriscedasticity analysis
bptest(final_bc.lm_num)

#Plotting summary of the linear regression model
par(mfrow = c(2, 2))
plot(final_bc.lm_num)


################################################
############    CONTOUR PLOT    ################
################################################

#Creating contour plot to show the predictions with repsect 
#to mass and distance values

# contourPlot(final.lm_num, N = 25)
new_data <- data.frame(
  'mass'     = seq(40, 110, length.out = 200),
  'distance' = seq(2, 6, length.out = 200)
)

#Creating data grid for predictions
new_data <- expand.grid(new_data)
predictions <- predict(final.lm_num, new_data)
new_data$measurement <- predictions

#Plotting contour plot
contour_plot <- ggplot(new_data, aes(mass, distance, z = measurement)) + 
  geom_raster(aes(fill = measurement)) +
  geom_contour(colour = "white", binwidth = 20) +
  labs(title = 'Contour Plot for Mass and Distance',
       fill = 'Measurement') +
  xlab('Mass') +
  ylab('Distance')
