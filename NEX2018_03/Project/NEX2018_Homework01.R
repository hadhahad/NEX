# Clear the environment
rm(list = ls())

# Loading libraries
packages <- c("tidyverse", "data.table", "car", "ggplot2", "agricolae", "nortest", "pwr")
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

# Mean values for each block
means_block <- tapply(hit_data$HITS_SUM, hit_data$BLOCK, mean)
means_hand <- tapply(hit_data$HITS_SUM, hit_data$HAND, mean)
means_diameter <- tapply(hit_data$HITS_SUM, hit_data$DIAMETER, mean)
# Mean values for each of the blocks are slightly different. However, the 4th block 
# shows an outstanding behavior. As a result, further investigation is needed. 
# Regarding circle diameters, the data shows, that with bogger diameter value the number
# of hits increases. 
# Turning to mean values with respect to the hand, as expected, the number of hits 
# made by the dominant hand is significantly larger than that of 
# the non-dominant and both hands.

# Variance
var_block <- tapply(hit_data$HITS_SUM, hit_data$BLOCK, var)
var_hand <- tapply(hit_data$HITS_SUM, hit_data$HAND, var)
var_diameter <- tapply(hit_data$HITS_SUM, hit_data$DIAMETER, var)
# The variance shows the same behavior as that of mean values. Once again, the 4th 
# block displays outlying behavior performance.

# Boxplots
ggplot(data = hit_data) +
  geom_boxplot(mapping = aes(x = BLOCK, y = HITS_SUM), color = "royalblue3") +
  labs(title = "Boxplot with Respect to Blocks") + 
  xlab("Block, [factor]") +
  ylab("Hit Count")

ggplot(data = hit_data) +
  geom_boxplot(mapping = aes(x = HAND, y = HITS_SUM), color = "maroon3") +
  labs(title = "Boxplot with Respect to Hand") + 
  xlab("Hand, [factor]") +
  ylab("Hit Count")

ggplot(data = hit_data) +
  geom_boxplot(mapping = aes(x = DIAMETER, y = HITS_SUM), color = "turquoise3") +
  labs(title = "Boxplot with Respect to Circle Diameter") + 
  xlab("Dimater, [cm]") +
  ylab("Hit Count")

# According to the boxplot visualization we can speculate, that 
# mean values are significantly different for the 'DIAMETER' variable. 

# Interaction plot
ggplot(data = hit_data) +
  aes(x = BLOCK, color = DIAMETER, group = DIAMETER, y = HITS_SUM) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  labs(title = "Interaction Plot of Hits with Respect to Blocks and Circle Diameter",
       color = "Circle Diameter") +
  xlab("Block, [factor]") +
  ylab("Hits Count")

ggplot(data = hit_data) +
  aes(x = HAND, color = DIAMETER, group = DIAMETER, y = HITS_SUM) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  labs(title = "Interaction Plot of Hits with Respect to Hand and Circle Diameter",
       color = "Circle Diameter") +
  xlab("Hand, [factor]") +
  ylab("Hits Count")

ggplot(data = hit_data) +
  aes(x = HAND, color = BLOCK, group = BLOCK, y = HITS_SUM) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  labs(title = "Interaction Plot of Hits with Respect to Hand and Blocks",
       color = "Block") +
  xlab("Hand, [factor]") +
  ylab("Hits Count")

# Interaction plots 1 and 3 display, that the 4th block (operator) is different from 
# the rest. Others show similar circle hits count. This can possibly be caused by 
# the effect of noise.
# Interection plot 2 displays the dependence of the circle hits count on the 'HAND' 
# and 'DIAMETER' variables, e.g. hits count to the circle of diameter 5 cm for 
# the dominant hand is the largest.


# ANOVA -------------------------------------------------------------------

hits_aov_all <- aov(HITS_SUM ~ BLOCK + HAND + DIAMETER, data = hit_data)
summary(hits_aov_all)
# ANOVA has shown, that all variables are significant on the 95% significance level.

hits_aov <- aov(HITS_SUM ~ HAND + DIAMETER, data = hit_data)
summary(hits_aov)
# Variables 'HAND' and 'DIAMETER' are still significant even without dependence of
# the circle hits on the blocks (operators). 
# That enables us to reject the hypothesis about the equality of mean values.

# ANOVA power
pwr_blocks <- power.anova.test(groups = 4, n = 9, between.var = (7.3)^2 / (2 * 4), 
                               within.var = 11.1,
                               sig.level = 0.05)
pwr_blocks$power

pwr_hand <- power.anova.test(groups = 3, n = 12, between.var = (6.416667)^2 / (2 * 3), 
                               within.var = 11.1,
                               sig.level = 0.05)
pwr_hand$power

pwr_diameter <- power.anova.test(groups = 3, n = 12, between.var = (12.25)^2 / (2 * 3), 
                               within.var = 11.1,
                               sig.level = 0.05)
pwr_diameter$power

# For each of the variables the ANOVA test power is above 90% which is remarkable.

n <- seq(2, 8)
a <- 3
b <- 4
D <- 5
sigma <- 4

pwr_blocks_specific <- power.anova.test(groups = 4, n = n, between.var = n*(5)^2 / (n-1), 
                                        within.var = 16,
                                        sig.level = 0.05)
rbind(n, pwr_blocks_specific$power)
# At least three replications have to be made for each block 
# to achieve the test power of 90%.

# Residuals Analysis ------------------------------------------------------

# Residuals
residuals_aov_all <- residuals(hits_aov_all)
residuals_aov <- residuals(hits_aov)

# Q-Q plot for residuals
params_all <- as.list(MASS::fitdistr(residuals_aov_all, "normal")$estimate)
ggplot(as.data.frame(residuals_aov_all), aes(sample = residuals_aov_all)) +
  stat_qq(distribution = qnorm, dparams = params) +
  stat_qq_line(distribution = qnorm, dparams = params) + 
  labs(title = "Residuals Normal Q-Q Plot for All Variables") +
  xlab("Theoretical Quantiles") + 
  ylab("Sample Quantiles")

params <- as.list(MASS::fitdistr(residuals_aov, "normal")$estimate)
ggplot(as.data.frame(residuals_aov), aes(sample = residuals_aov)) +
  stat_qq(distribution = qnorm, dparams = params) +
  stat_qq_line(distribution = qnorm, dparams = params) + 
  labs(title = "Residuals Normal Q-Q Plot") +
  xlab("Theoretical Quantiles") + 
  ylab("Sample Quantiles")

# Q-Q plots lines fit the data in an acceptable way. However, a few values display 
# outlying behavior. Normality tests must be carries out.


# Normality tests
shapiro.test(residuals_aov_all)
shapiro.test(residuals_aov)
# As p-values from the Shapiro-Wilk test are close to the set significance level (5%),
# we will also perform the Lilliefors test of normality.
lillie.test(residuals_aov_all)
lillie.test(residuals_aov)
# As a result, we cannot reject the residuals normality hypothesis for both models. 
# Thus, we are enabled to use ANOVA in spite of the fact, that outlying behavior 
# of a few data points was spotted.

# It would be also interesting to observe dependence of residuals with respect to measurements
# order. However, such data are not available within our experiment. 


# Differences -------------------------------------------------------------

# Fisher's LSD-test
lsd_all_hand <- LSD.test(hit_data$HITS_SUM, hit_data$HAND, DFerror = 28, 11.1)
lsd_all_hand

lsd_all_block <- LSD.test(hit_data$HITS_SUM, hit_data$BLOCK, DFerror = 28, 11.1)
lsd_all_block

lsd_all_diameter <- LSD.test(hit_data$HITS_SUM, hit_data$DIAMETER, DFerror = 28, 11.1)
lsd_all_diameter

# Tukey's HSD-test
hsd_all_hand <- TukeyHSD(hits_aov_all)
hsd_all_hand

# Once again we observe significant difference between the 4th block (operator)
# and 3 other blocks. An interesting observation is that the 3rd block is 
# on the edge of being significantly similar to the 4th one.

# Both tests have confirmed, that the performance of the dominant hand is 
# significantly different from other variants.

# Tukey's HSD test and Fisher's LSD test indicate, that circles with diameters 3 cm and 
# 5 cm are significantly similar. On the other hand, the circle with diameter of 1 cm
# is significantly different from two other ones.


# Linear Regression -------------------------------------------------------

lm_data <- hit_data
lm_data$DIAMETER <- as.numeric(lm_data$DIAMETER)

lm_circle_1 <- lm(HITS_SUM ~ -1 + DIAMETER + HAND, data = lm_data)
summary(lm_circle_1)
opar <- par(mfrow=c(2,2),cex=.8)
plot(lm_circle_1)
par(opar)

lm_circle_2 <- lm(HITS_SUM ~ -1 + I(DIAMETER^2) + HAND, data = lm_data)
summary(lm_circle_2)
opar <- par(mfrow=c(2,2),cex=.8)
plot(lm_circle_2)
par(opar)

# Normality of residuals
shapiro.test(lm_circle_1$residuals)
shapiro.test(lm_circle_2$residuals)

# As Q-Q plots and Shapiro-Wilk test indicate, general assumptions for 
# performing the linear regression task are met. 
# According to the R-squared statistic, the model with the circle diameter 
# set to the power of 2 explains the hit data slightly worse. However,
# the difference is negligible. As a result, we choose the first model.