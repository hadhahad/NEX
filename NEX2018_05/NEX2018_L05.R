# NEX 2018 - R codes used in Lecture 05
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Some parts of code is from R Companion to Montgomery?s DAoE
# http://www.aliquote.org/articles/tech/dae/ 

#Instalation of any library
library(car)          # provides a set of useful functions for ANOVA designs and Regression Models;
library(lattice)      # provides some graphical enhancements compared to traditional R graphics, as well as multivariate displays capabilities;
library(lme4)         # the newer and enhanced version of the nlme package, for which additional data structure are available (nested or hierarchical model,. . . );
library(nlme)         # for handling mixed-effects models;
library(pwr)          # power analysis
library(agricolae)    # for Fisher LSD method
library(scatterplot3d)# for 3d scatter plot
library(alr3)
library(DoE.base)      # Full factorials, orthogonal arrays and base utilities for DoE packages
library(rsm)           # Response-Surface Methods
library(qualityTools)
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)


# Define directory
setwd("D:/Vyuka/NEX/2018/05_2k_factorial_design_Introduction/")
library(FrF2)          #for 2^k  factorial design

##############################################################################
# Follow Chapter 6 - Montgomery DAOE
##############################################################################


##############################################################################
#  The 2^2 design
##############################################################################

yields <- read.table("yield.txt",header=T)
summary(yields)
attach(yields)
#rm(yield)
yield.sums <- aggregate(yield,list(reactant=reactant,catalyst=catalyst),sum)

summary(yield.aov1 <- aov(yield~reactant*catalyst))
summary(yield.aov2 <- aov(yield~reactant+catalyst))
anova(yield.aov2,yield.aov1)

reactant.num          <- reactant
levels(reactant.num)  <- c(25,15)
reactant.num          <- as.numeric(as.character(reactant.num))
catalyst.num          <- catalyst
levels(catalyst.num)  <- c(2,1)
catalyst.num          <- as.numeric(as.character(catalyst.num))
yield.lm              <- lm(yield~reactant.num+catalyst.num)
yield.lm               ## gives the coefficients of the LM
summary(yield.lm)

# 
reactant.num2          <- reactant
levels(reactant.num2)  <- c(-1,1)
reactant.num2          <- as.numeric(as.character(reactant.num2))
catalyst.num2          <- catalyst
levels(catalyst.num2)  <- c(-1,1)
catalyst.num2          <- as.numeric(as.character(catalyst.num2))
yield.lm2              <- lm(yield~reactant.num2+catalyst.num2)
yield.lm2               ## gives the coefficients of the LM
summary(yield.lm2)


tmp           <- list(reactant.num=seq(15,25,by=.5),catalyst.num=seq(1,2,by=.1))
new.data      <- expand.grid(tmp)
new.data$fit  <- predict(yield.lm,new.data)
contourplot(fit~reactant.num+catalyst.num,new.data,xlab="Reactant",ylab="Catalyst",
            main="Contour plot of Chemical process model with new predicted dataset",
            cex.lab=1.6, cex.axis=1.6, cex.main=1.0, cex.sub=1.5)

# scatterplot3d
s3d           <- scatterplot3d(reactant.num,catalyst.num,yield,type="n",angle=135,scale.y=1,xlab="Reactant",ylab="Catalyst")
s3d$plane3d(yield.lm,lty.box="solid",col="darkgray")


# another design of scatterplot3d
s3d <-scatterplot3d(reactant.num, catalyst.num, yield, pch=16,
                    highlight.3d=TRUE, type="h",
                    main="3D Scatter Plot with Vertical Lines and Regression Planes",
                    angle=135,scale.y=1, xlab="Reactant",ylab="Catalyst",zlab="Yield",
                    cex.lab=1.5, cex.axis=1.5, cex.main=1.1, cex.sub=1.5)
s3d$plane3d(yield.lm)

##############################################################################
#  The 2^3 design
##############################################################################

plasma <- read.table("plasma.txt",header=T)
attach(plasma)
plasma
plasma.df       <- data.frame(etch=c(plasma$R1,plasma$R2),rbind(plasma[,2:4],plasma[,2:4]))
plasma.df[,2:4] <- lapply(plasma.df[,2:4],factor)
attach(plasma.df)
plasma.aov1     <- aov(etch~A*B*C, data=plasma.df)
summary(plasma.aov1)
#
#A.num                 <- factor(A)
#levels(A.num)         <- c(0,1)
#A.num                 <- as.numeric(as.character(A.num))
#B.num                 <- factor(B)
#levels(B.num)         <- c(0,1)
#B.num                 <- as.numeric(as.character(B.num))
#C.num                 <- factor(C)
#levels(C.num)         <- c(0,1)
#C.num                 <- as.numeric(as.character(C.num))
#Plasma.01coded.lm     <- lm(etch~A.num*B.num*C.num)
#summary(Plasma.01coded.lm)
#
A.num                 <- factor(A)
levels(A.num)         <- c(-1,1)
A.num                 <- as.numeric(as.character(A.num))
B.num                 <- factor(B)
levels(B.num)         <- c(-1,1)
B.num                 <- as.numeric(as.character(B.num))
C.num                 <- factor(C)
levels(C.num)         <- c(-1,1)
C.num                 <- as.numeric(as.character(C.num))
Plasma.coded.lm     <- lm(etch~A.num*B.num*C.num)
summary(Plasma.coded.lm)

plasma.aov2     <- aov(etch~A*C, data=plasma.df)
summary(plasma.aov2)
anova(plasma.aov2,plasma.aov1)

Plasma.coded.reduced.lm     <- lm(etch~A.num*C.num)
summary(Plasma.coded.reduced.lm)


Gap.num                 <- factor(A)
levels(Gap.num)         <- c(0.8,1.2)
Gap.num                 <- as.numeric(as.character(Gap.num))
Flow.num                <- factor(B)
levels(Flow.num)        <- c(125,200)
Flow.num                <- as.numeric(as.character(Flow.num))
Power.num               <- factor(C)
levels(Power.num)       <- c(275,325)
Power.num               <- as.numeric(as.character(Power.num))



Plasma.lm     <- lm(etch~Gap.num+Power.num)
s3d           <- scatterplot3d(Gap.num,Power.num,etch,type="n",angle=135,scale.y=1,xlab="Gap",ylab="Power", main = "Response surface plot of etch rate from the Plasma experiment")
s3d$plane3d(Plasma.lm,lty.box="solid",col="darkgray")


Plasma.lm     <- lm(etch~Gap.num*Power.num)
tmp           <- list(Gap.num=seq(0.8,1.2,by=.05),Power.num=seq(275,325,by=15))
new.data      <- expand.grid(tmp)
new.data$fit  <- predict(Plasma.lm,new.data)
contourplot(fit~Gap.num+Power.num,new.data,xlab="Gap",ylab="Power", main = "Contour plot of etch  rate from the Plasma experiment")

cubePlot(lm(etch~A.num*C.num),"A.num","C.num","etch",
         main=paste("Cube plot for etch"),
         cex.title=1.2,cex.lab=par("cex.lab"), cex.ax=par("cex.axis"),  
         cex.clab=1.0, size=0.3, round=NULL,
         abbrev=4 ,y.margin.add=-0.1, modeled=T)

summary(Plasma.coded.reduced.lm)

summary(Plasma.lm)
summary(aov(Plasma.lm))
summary(plasma.aov2)
summary(plasma.aov1)
qqnorm(residuals(Plasma.coded.lm))
qqline(residuals(Plasma.coded.lm)) 


# interaction plot
interaction.plot(Gap.num,Power.num,etch,type="b",pch=19, fixed=T,xlab="Gap in cm",ylab="Etch rate")

##############################################################################
#### USING  FrF2 - 2^k design
##############################################################################

k = 3
# FrF2 specifies the number of runs in the fractional factorial design 
plan <-  FrF2(2^k, k, replications = 2, randomize = F,factor.names = c("Gap", "Flow", "Power"))
#plan < - FrF2(2^k, k, default.levels = c("low", "high"), factor.names = c("Factor A", "Factor B", "Factor C"))
plan <- add.response(plan, etch)
MEPlot(plan)
IAPlot(plan)

## Daniel plot and Half normal plot are usually used for unreplicatd design
# Daniel Plot with alpha = 0.5 and only significant factors
DanielPlot(plan,code=TRUE)
# Classical effects qqplot
qqplot(DanielPlot(plan,alpha=0.1)$x,DanielPlot(plan)$y)
qqline(DanielPlot(plan,alpha=0.1)$y)
# half normal plot of effects
DanielPlot(plan,code=TRUE,alpha=0.5,half=TRUE)

plasma.df
colnames(plasma.df) <- c("etch", "Gap", "Flow", "Power")
with(plasma.df, cubePlot(etch , Gap, Flow, Power, main = "Cube plot of raw data"))
cubePlot(lm(etch ~ Gap:Flow:Power, data = plasma.df), "Gap", "Flow", "Power", main = "Cube plot of means model without all three factors interactions")
cubePlot(lm(etch ~ Gap*Flow*Power, data = plasma.df), "Gap", "Flow", "Power", main = "Cube plot of means model with all interactions")



k=3
plan2a <-  FrF2(2^k, k, replications = 2, randomize = FALSE,factor.names = c("A", "B", "C"))
result <-  rnorm(16)
plan2a <-  add.response(plan2a,result)
k=4
plan2b <-  FrF2(2^k, k, replications = 2, randomize = FALSE,factor.names = c("A", "B", "C", "D"))

plan <- fac.design(nlevels=c(2,3,2,4))
result <- rnorm(2*3*2*4)
add.response(plan,response=result)

#Exercise:
#Problem 6.1 and 6.2
data61 <- read.table("Problem_6_1.txt",header=T,sep=";")
summary(data61)
names(data61) <- c("A","B","C","resp")

life <- c(data61$resp[seq(1,22,by=3)],data61$resp[seq(2,23,by=3)],data61$resp[seq(3,24,by=3)])
k=3
model1 <-  FrF2(2^k, k, replications = 3, randomize = FALSE,factor.names = c("A", "B", "C"))
model1 <- add.response(model1,response=life)
MEPlot(model1)
IAPlot(model1)
# compare with
interaction.plot(model1$A,model1$C,life)
interaction.plot(model1$C,model1$A,life)
