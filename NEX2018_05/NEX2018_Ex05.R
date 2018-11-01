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

Problem_61 <- read.table("Problem_6_1.txt",header=TRUE,sep=";")


