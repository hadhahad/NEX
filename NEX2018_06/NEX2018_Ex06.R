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

# Anova 0
anova(aov(data.Weight~A*B*C*D,  data=exp))
model0 = lm(data.Weight~A*B*C*D,  data=exp)
summary(model0)

# Daniel plot 
DanielPlot(exp,code=TRUE)

# Anova 1
anova(aov(data.Weight~A+C+A:B,  data=exp))
model1 = lm(data.Weight ~ A:B + A + B + C,  data=exp)
summary(model1)

# Get numeric variables
A.num <- as.numeric(data$Temperature)
B.num <- as.numeric(data$Concentration)
C.num <- as.numeric(data$Time)

# Construct the linear model
weight.lm <- lm(data.Weight ~ A.num:B.num + A.num + C.num, data=exp)
summary(weight.lm)

# Fit
new.data <- t(rbind(seq(100, 120, length.out = 10), 
                     seq(4, 8, length.out = 10),
                     C.num = seq(20, 30, length.out = 10)), 
              data)
colnames(new.data) <- c("A.num", "B.num", "C.num")
new.data <- as.data.frame(new.data)
new.data <- expand.grid(new.data)
new.data$fit  <- predict(weight.lm, new.data)

# Countour plots - only 2 variables in the model
contourplot(fit~A.num*C.num,new.data,xlab="A Temperature",ylab="C Concentration", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment")
contourplot(fit~C.num*D.num,new.data,xlab="C Concentration",ylab="D Stiring Rate", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment")
contourplot(fit~A.num*D.num,new.data,xlab="A Temperature",ylab="D Stiring Rate", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment")

# Countour plots - all 3 varaibles + interactions in the model
par(mfrow = c(2,2))
wirePlot(A, C, Filtration_qt, data = rate_qt,form = "Filtration_qt~A*C+A*D")
contourPlot(A, C, Filtration_qt, data = rate_qt,form = "Filtration_qt~A*C+A*D")
contourPlot(C, D, Filtration_qt, data = rate_qt,form = "Filtration_qt~A*C+A*D")
contourPlot(A, D, Filtration_qt, data = rate_qt,form = "Filtration_qt~A*C+A*D")
