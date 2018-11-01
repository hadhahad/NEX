# NEX 2018 - Exercise 5
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Data and exercises come from D.C. Montgomery: Design and Analysis of Experiment

#Get required libraries:
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
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)

# if you don't have required library installed use command
#install.packages("name")

# Task: Solve Problems 6.1 and 6.2 from Montgomery DAoE ed.8 - chapter 6

data <- read.table("Problem_6_1.txt",header=TRUE,sep=";")


# 1 -----------------------------------------------------------------------

# FrF2 tryout (with right ordering)
k <- 3
mlife <- FrF2(2^k, k, replications = 3, randomize = F, 
              factor.names = c("Speed", "Geometry", "Angle"),
              sort = "natural")
life <- c(data$LifeHours[seq(1, 22, by = 3)], 
          data$LifeHours[seq(2, 23, by = 3)],
          data$LifeHours[seq(3, 24, by = 3)])
mlife <- add.response(mlife, life)

# Numeric variables
Speed.num <- factor(data$CuttingSpeed)
Speed.num <- as.numeric(as.character(Speed.num))
Geometry.num <- factor(data$ToolGeometry)
Geometry.num <- as.numeric(as.character(Geometry.num))
Angle.num <- factor(data$CuttingAngle)
Angle.num <- as.numeric(as.character(Angle.num))

# ANOVA
mlife_aov <- aov(data$LifeHours ~ Speed.num + Geometry.num + Angle.num)
summary(mlife_aov)

mlife_aov_all <- aov(LifeHours ~ CuttingSpeed*ToolGeometry*CuttingAngle, data = data)
summary(mlife_aov_all)

# Interaction
interaction.plot(Angle.num,Geometry.num,data$LifeHours,type="b",pch=19, fixed=T,xlab="Angle",ylab="Geometry")
interaction.plot(Angle.num,Speed.num,data$LifeHours,type="b",pch=19, fixed=T,xlab="Angle",ylab="Geometry")
interaction.plot(Speed.num,Geometry.num,data$LifeHours,type="b",pch=19, fixed=T,xlab="Angle",ylab="Geometry")

# Linear model
mlife_lm <- lm(LifeHours ~ CuttingSpeed + ToolGeometry + CuttingAngle + CuttingSpeed:CuttingAngle, data = data)
anova(mlife_lm)
summary(mlife_lm) 

# Contour plot
temp_df <- data.frame("CuttingSpeed" = Speed.num, 
                      "ToolGeometry" = Geometry.num, 
                      "CuttingAngle" = Angle.num)
# New lm...
