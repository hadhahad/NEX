# 01NEX - Exercise 1
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Data and exercises come from D.C. Montgomery: Design and Analysis of Experiment - Chapter 02

#Run of any library
library(pwr)    # power analysis
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)

# Define directory
#setwd("D:/Vyuka/NEX/2018/Lecture_01/")
setwd("D:/Vyuka/NEX/2018/01_Introduction_and_simple_comparative_experiments/")
# Read data
Problem20 <- read.table("Data_01/Ex02_20.csv",header=TRUE,sep=";")
Problem26 <- read.table("Data_01/Ex02_26.csv",header=TRUE,sep=";")
Problem32 <- read.table("Data_01/Ex02_30.csv",header=TRUE,sep=";")

# Solve all three problems


# 1 -----------------------------------------------------------------------

t.test(Problem20$Days, alternative = "greater", mu = 120) # yes, it is greater than 120 on the conf. level = 95%
t.test(Problem20$Days, alternative = "greater", mu = 120, conf.level = 0.99)
temp <- t.test(Problem20$Days, alternative = "two.sided", mu = 120, conf.level = 0.99)
temp$conf.int

# 2 -----------------------------------------------------------------------

var.test(Problem26$Type1, Problem26$Type2, ratio = 1, alternative = "two.sided", conf.level = 0.95) # equal variances
t.test(Problem26$Type1, Problem26$Type2, alternative = "two.sided", 
       mu = 0, paired = FALSE, conf.level = 0.95, var.equal = TRUE)
shapiro.test(Problem26$Type1)
shapiro.test(Problem26$Type2)


# 3 -----------------------------------------------------------------------

var.test(Problem32$X10.seconds, Problem32$X20.seconds, ratio = 1, alternative = "two.sided", conf.level = 0.95) # equal variances
t.test(Problem32$X10.seconds, Problem32$X20.seconds, alternative = "less", 
       mu = 0, paired = FALSE, conf.level = 0.95, var.equal = TRUE)
temp <- t.test(Problem32$X10.seconds, Problem32$X20.seconds, alternative = "two.sided", 
               mu = 0, paired = FALSE, conf.level = 0.95, var.equal = TRUE)
temp$conf.int

dotchart(Problem32$X10.seconds,labels=row.names(Problem32),cex=.7)
dotchart(Problem32$X20.seconds,labels=row.names(Problem32),cex=.7)

ggplot(Problem32) + 
  geom_dotplot(mapping = aes(x=X10.seconds, y=row.names(Problem32)), 
               binaxis='y', stackdir='center', fill = "red", alpha = 0.5, dotsize = 1.25) + 
  geom_dotplot(mapping = aes(x=X20.seconds, y=row.names(Problem32)), 
               binaxis='y', stackdir='center', fill = "blue", alpha = 0.5, dotsize = 1.5) + 
  scale_x_continuous(breaks = seq(1, 10), labels = seq(1, 10)) +
  xlab("Time") +
  ylab("Observation")

  
