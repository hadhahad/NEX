# Exercise 2
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Data and exercises come from D.C. Montgomery: Design and Analysis of Experiment

#Instalation of any library
library(car)     # provides a set of useful functions for ANOVA designs and Regression Models;
library(lattice) # provides some graphical enhancements compared to traditional R graphics, as well as multivariate displays capabilities;
library(lme4)    # the newer and enhanced version of the nlme package, for which additional data structure are available (nested or hierarchical model,. . . );
library(nlme)    # for handling mixed-effects models;
library(pwr)     # power analysis
library(agricolae)
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)

# Define directory
setwd("D:/Vyuka/NEX/Lecture_02/")

# Read data
Problem01 <- read.table("Data/Ex03_7.csv",header=TRUE,sep=";")
Problem02 <- read.table("Data/Ex03_10.csv",header=TRUE,sep=";")

Problem01

Problem01$TechniqueF  = factor(Problem01$Technique)

attach(Problem01)

plot(Tensile_Strength,TechniqueF)
plot(Tensile_Strength~TechniqueF)

pairs(Problem01)

plot(Tensile_Strength,Technique)
plot(Tensile_Strength~Technique)

scatterplot(Tensile_Strength~Technique)
scatterplot(Tensile_Strength ~ 1| Technique)






aov_problem1 =  aov(Tensile_Strength~Technique, data = Problem01)
summary(aov_problem1)

lm1_problem01 = lm(Tensile_Strength ~Technique, data = Problem01)
summary(lm1_problem01)
anova(lm1_problem01)

opar <- par(mfrow=c(2,2),cex=.8)
plot(lm1_problem01)
par(opar)

lm1_problem01$fitted.values  
lm1_problem01$residuals  
  
plot(lm1_problem01$fitted.values , lm1_problem01$residuals )




plot(lm1_problem01)

lm2_problem01 = lm(Tensile_Strength ~-1+Technique, data = Problem01)
summary(lm2_problem01)


