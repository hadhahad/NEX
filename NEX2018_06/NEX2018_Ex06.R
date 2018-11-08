#Instalation of any library
library(car)          # provides a set of useful functions for ANOVA designs and Regression Models;
library(lattice)      # provides some graphical enhancements compared to traditional R graphics, as well as multivariate displays capabilities;
library(lme4)         # the newer and enhanced version of the nlme package, for which additional data structure are available (nested or hierarchical model,. . . );
library(nlme)         # for handling mixed-effects models;
library(pwr)          # power analysis
library(agricolae)    # for Fisher LSD method
library(scatterplot3d)# for 3d scatter plot
library(alr3)
library(FrF2)          #for 2^k  factorial design
library(DoE.base)      # Full factorials, orthogonal arrays and base utilities for DoE packages
library(geoR)
library(qualityTools)  # need for wirePlot, contourPlot
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)

data <- read.csv("Problem_6_31.txt", sep = ";")
data <- data[1:(nrow(data) - 4), ]
summary(data)

exp <- FrF2(2^4, 4, replications = 1, randomize = FALSE, factor.names = c("A", "B", "C", "D"))
exp <- add.response(exp, data$Weight)
summary(exp)

# Main Effects plotfor response variable
MEPlot(exp)

# Interaction Plot matrix for response variable
IAPlot(exp)

# Anova