# NEX 2017 - Exercise 4
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Data and exercises come from D.C. Montgomery: Design and Analysis of Experiment

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
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)


# Solve Problems 4-23, 4.40, and 4-42 from Montgomery.

Problem_4_23 <- read.table("Data/Problem_4_23.txt",header=TRUE,sep=";")
Problem_4_40 <- read.table("Data/Problem_4_40.txt",header=TRUE,sep=";")
Problem_4_42 <- read.table("Data/Problem_4_42.txt",header=TRUE,sep=";")

