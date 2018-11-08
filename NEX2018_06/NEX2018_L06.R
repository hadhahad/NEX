# NEX 2018 - Exercise 6
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
library(geoR)
library(qualityTools)  # need for wirePlot, contourPlot
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)
setwd("D:/Vyuka/NEX/2017/06_2k_factorial_design/")


##############################################################################
### Single Replicate of the 2^4 design 
##############################################################################

# read data
rate_experiment             <- read.table("Pilot_Plant_Filtration_Rate_Experiment.csv",header=TRUE, sep = ";")
is.factor(rate_experiment$A)
summary(rate_experiment)
# factor varaibles are storered as numerical -> need to change
rate_experiment$A           <- factor(rate_experiment$A) 
rate_experiment$B           <- factor(rate_experiment$B) 
rate_experiment$C           <- factor(rate_experiment$C)
rate_experiment$D           <- factor(rate_experiment$D) 
is.factor(rate_experiment$A)
summary(rate_experiment)

# Create $2^k$ Factorial Design - with 4 factors (variables) coded as -1 and +1 
#rate           <-  FrF2(2^4, 4, replications = 1, randomize = FALSE,factor.names = c("A_Temperature", "B_Pressure", "C_Concentration", "D_Stirring_rate"))
rate            <-  FrF2(2^4, 4, replications = 1, randomize = FALSE,factor.names = c("A", "B", "C", "D"))
Filtration      <-  rate_experiment$Rate
# Add response variable - check the order of measurement (we used "randomize = FALSE")
rate            <-  add.response(rate, Filtration)
summary(rate)
# Main Effects plotfor response variable
MEPlot(rate)
# Interaction Plot matrix for response variable
IAPlot(rate)
#Boxplots
op <- par(mfrow=c(2,2))
with(rate, boxplot(Filtration ~ A , main = "Factor A - Temperature") )
with(rate, boxplot(Filtration ~ B , main = "Factor B - Pressure")     )
with(rate, boxplot(Filtration ~ C , main = "Factor C - Concentration") )
with(rate, boxplot(Filtration ~ D , main = "Factor D - Stirring rate")  )
title(main="Box plots of all factors", line=-1, outer=T, cex.main=1)
par(op)   

# linear model - data from dataframe "rate_experiment"
summary(rate_experiment)
model01 = lm(Rate~A+B+C+D,  data=rate_experiment)
summary(model01)

# the same linear model - data from FrF2 object "rate"
summary(rate)
model02 = lm(Filtration~A+B+C+D,  data=rate)
summary(model02)

# Model with all factors and interaction -> Explain why NA.
model0 = lm(Filtration~A*B*C*D,  data=rate)
summary(model0)

# Which variables are significant?
# Make plots by hand - need to double response 
model00 = lm(2*Filtration~A*B*C*D,  data=rate)
barplot(sort(model00$coeff[2:(2^4-1)]),las = 2)
qqnorm(model00$coeff[2:(2^4-1)],cex = 1.3,pch = 15)
qqline(model00$coeff[2:(2^4-1)],cex = 1.3,pch = 15)

# Make plots by DanielPlot function
opar <- par(mfrow=c(1,1))
## Daniel plot and Half normal plot are usually used for unreplicatd design
# Daniel Plot with alpha = 0.1 and only significant factors
DanielPlot(rate,code=TRUE)
# Classical effects qqplot
qqplot(DanielPlot(rate,alpha=0.1)$x,DanielPlot(rate)$y)
qqline(DanielPlot(rate,alpha=0.1)$y)
# half normal plot of effects
DanielPlot(rate,code=TRUE,alpha=0.1,half=TRUE)



# B not significant at SL 0.1 - discard B and have 2^3 design with two replicates
rate2            <-  FrF2(2^3, 3, replications = 2, randomize = FALSE,factor.names = c("A", "C", "D"),repeat.only=TRUE)
Filtration2      <-  c(45 , 48 , 71 , 65 , 68 ,  80 , 60 , 65 , 43 , 45 , 100 , 104 , 75 , 70 , 86 , 96)
rate2            <-  add.response(rate2, Filtration2)
anova(aov(Filtration2~A*C*D, data=rate2))
# same as original linear model without factor B
anova(aov(Filtration~A*C*D, data=rate))



# What step function find?
model_start_step <- lm(Filtration~(.)^2, data=rate)
summary(model_start_step)
model_final_step <- step(model_start_step)
summary(model_final_step)

# final model
anova(aov(Filtration~A*C+A*D,  data=rate))
model1 = lm(Filtration~A+C+D+A:C+A:D,  data=rate)
summary(model1)

library(coefplot) 
coefplot(model1, decreasing = F, sort = "natural")
#library(dynsurf) 
#plotCoeff(model1,sort = TRUE)

# another approach
# model with all factors and all interactions - we do not have "pure error" 
anova(aov(Filtration~(.)^4,  data=rate))
# model with all factors and all interactions - we omit the highest interaction 
anova(aov(Filtration~(.)^3,  data=rate))
# use only significant variables
anova(aov(Filtration~A+C+D+A:C+A:D,  data=rate))
model1 = lm(Filtration~A+C+D+A:C+A:D,  data=rate)
summary(model1)

# Final model validation
opar <- par(mfrow=c(2,2))
plot(model1)
par(opar)


#Take as a numeric -1, +1 instead of factor
rate$A.num <- 2*(as.numeric(rate$A)-1.5)
rate$C.num <- 2*(as.numeric(rate$C)-1.5)
rate$D.num <- 2*(as.numeric(rate$D)-1.5)
rate.lm       <- lm(Filtration~A.num*C.num+A.num*D.num,data=rate)
summary(rate.lm)
tmp           <- list(A.num=seq(-1,1,by=.05),C.num=seq(-1,1,by=0.05),D.num=seq(-1,1,by=0.05),data=rate)


model1.num = lm(Filtration~A*C*D-C*D,  data=rate)
summary(model1.num)

new.data2 <- t(rbind(seq(-1,1,by=.05),seq(-1,1,by=.05),seq(-1,1,by=.05)))
colnames(new.data2) <-c("A.num","C.num","D.num")
new.data2 <- as.data.frame(new.data2)
new.data2 <- expand.grid(new.data2)

new.data       <- expand.grid(tmp)
new.data$fit   <- predict(rate.lm,new.data)
new.data$fit2  <- predict(rate.lm,new.data2)

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


rate3             <- read.table("Pilot_Plant_Filtration_Rate_Experiment3.csv",header=TRUE, sep = ";")
summary(rate3)
anova(aov(Rate~A*C+A*D,data=rate3))

rate4            <-  FrF2(2^3, 3, replications = 2, randomize = FALSE,factor.names = c("A", "C", "D"),repeat.only=TRUE)
rate4            <-  add.center(rate4,2)
Filtration4      <-  c(45 , 48 , 71 , 65 , 68 ,  80 , 60 , 65 , 43 , 45 , 100 , 104 , 75 , 70 , 86 , 96, 73,75,66,69)
rate4            <-  add.response(rate4, Filtration4)
anova(aov(Filtration2~A*C+A*D,  data=rate2))
anova(aov(Filtration4~A*C+A*D,  data=rate4))

summary(lm(Filtration4~A*C+A*D+iscube(rate4), rate4))

#195.13 + 48.75 + 1.51
#245.39






# Pareto plot
rate_effects  <-  DanielPlot(rate,alpha=0.1)$x

source("pareto.R")  
pareto(rate_effects, names = c("A","B","C","D","AB","AC","AD","BC","BD","CD","ABC","ABD","ACD","BCD","ABCD"))

rate_qt = fracDesign(k = 4, replicates = 1,random.seed     = 0)  
Filtration_qt= rate$Filtration[rate_qt[,1]]
response(rate_qt) = Filtration_qt 
opar = par(mfrow = c(1,2))
paretoPlot(rate_qt)
normalPlot(rate_qt)
par(opar)




##############################################################################
# Follow Chapter 6 - Montgomery DAOE
##############################################################################

##############################################################################
### Single Replicate of the 2^4 design 
##############################################################################


rate_experiment             <- read.table("Pilot_Plant_Filtration_Rate_Experiment.csv",header=TRUE, sep = ";")
summary(rate_experiment)
rate_experiment$A           <- factor(rate_experiment$A) 
rate_experiment$B           <- factor(rate_experiment$B) 
rate_experiment$C           <- factor(rate_experiment$C)
rate_experiment$D           <- factor(rate_experiment$D) 
summary(rate_experiment)

#rate           <-  FrF2(2^4, 4, replications = 1, randomize = FALSE,factor.names = c("A_Temperature", "B_Pressure", "C_Concentration", "D_Stirring_rate"))
rate            <-  FrF2(2^4, 4, replications = 1, randomize = FALSE,factor.names = c("A", "B", "C", "D"))
Filtration      <-  rate_experiment$Rate
rate            <-  add.response(rate, Filtration)
summary(rate)
# Main Effects plotfor response variable
MEPlot(rate)
# Interaction Plot matrix for response variable
IAPlot(rate)
#Boxplots
opar <- par(mfrow=c(2,2))
with(rate, boxplot(Filtration ~ A , main = "Factor A - Temperature") )
with(rate, boxplot(Filtration ~ B , main = "Factor B - Pressure")     )
with(rate, boxplot(Filtration ~ C , main = "Factor C - Concentration") )
with(rate, boxplot(Filtration ~ D , main = "Factor D - Stirring rate")  )
title(main="Box plots of all factors", line=-1, outer=T, cex.main=1)
par(opar)    


#NPK        <- expand.grid(A=mp,B=mp,C=mp,D=mp)
#Rate       <- c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96)
#filtr      <- cbind(NPK,Rate)
#filtr


## Daniel plot and Half normal plot are usually used for unreplicatd design
# Daniel Plot with alpha = 0.5 and only significant factors
DanielPlot(rate,code=TRUE)
# Classical effects qqplot
qqplot(DanielPlot(rate,alpha=0.1)$x,DanielPlot(rate)$y)
qqline(DanielPlot(rate,alpha=0.1)$y)
# half normal plot of effects
DanielPlot(rate,code=TRUE,alpha=0.5,half=TRUE)
# Pareto plot
rate_effects  <-  DanielPlot(rate,alpha=0.1)$x

source("pareto.R")  
pareto(rate_effects, names = c("A","B","C","D","AB","AC","AD","BC","BD","CD","ABC","ABD","ACD","BCD","ABCD"))

rate_qt = fracDesign(k = 4, replicates = 1,random.seed     = 0)  
Filtration_qt= rate$Filtration[rate_qt[,1]]
response(rate_qt) = Filtration_qt 
opar = par(mfrow = c(1,2))
paretoPlot(rate_qt)
normalPlot(rate_qt)
par(opar)

# B not significant - discard B and have 2^3 design with two replicates
rate2            <-  FrF2(2^3, 3, replications = 2, randomize = FALSE,factor.names = c("A", "C", "D"),repeat.only=TRUE)
Filtration2      <-  c(45 , 48 , 71 , 65 , 68 ,  80 , 60 , 65 , 43 , 45 , 100 , 104 , 75 , 70 , 86 , 96)
rate2            <-  add.response(rate2, Filtration2)
anova(aov(Filtration2~A*C*D, data=rate2))
anova(aov(Filtration~A*C*D, data=rate))
# same as original linear model without factor B
anova(aov(Filtration~A*B*C*D,  data=rate))
# final model
anova(aov(Filtration~A*C+A*D,  data=rate))
model1 = lm(Filtration~A+C+D+A:C+A:D,  data=rate)
summary(model1)


# another approach
# model with all factors and all interactions - we do not have "pure error" 
anova(aov(Filtration~(.)^4,  data=rate))
# model with all factors and all interactions - we omit the highest interaction 
anova(aov(Filtration~(.)^3,  data=rate))
# use only significant variables
anova(aov(Filtration~A+C+D+A:C+A:D,  data=rate))
model1 = lm(Filtration~A+C+D+A:C+A:D,  data=rate)
summary(model1)

opar <- par(mfrow=c(2,2))
plot(model1)
par(opar)

#rsm must make use of the special functions FO, TWI, PQ, or SO (for ?first-order,?, ?two-way interaction,? ?pure quadratic,? and ?second-order,? respectively),
#CR       = coded.data(data = rate[,c(-2, -4)])
#CR.rsm1 <- rsm(Filtration ~ FO(A,C), data = CR)
#summary(CR.rsm1)


#battery     <- read.table("Data/battery.txt",header=TRUE)
#Temperature <- unclass(battery$Temperature)
#Material    <- unclass(battery$Material)
#CR          <- coded.data(battery, x1~((Temperature -mean(Temperature ))), x2 ~ (Material-2))
#CR.rsm1     <- rsm(Life ~ FO(x1,x2), data = CR)
#summary(CR.rsm1)
#
#contour(CR.rsm1, ~ x1 + x2, image = TRUE),  at = summary(CR.rsm1)$canonical$xs)

rate$A.num <- 2*(as.numeric(rate$A)-1.5)
rate$C.num <- 2*(as.numeric(rate$C)-1.5)
rate$D.num <- 2*(as.numeric(rate$D)-1.5)
rate.lm       <- lm(Filtration~A.num*C.num+A.num*D.num,data=rate)
summary(rate.lm)


tmp           <- list(A.num=seq(-1,1,by=.05),C.num=seq(-1,1,by=0.05),D.num=seq(-1,1,by=0.05),data=rate)
new.data      <- expand.grid(tmp)


new.data2 <- t(rbind(seq(-1,1,by=.05),seq(-1,1,by=.05),seq(-1,1,by=.05)))
colnames(new.data2) <-c("A.num","C.num","D.num")
new.data2 <- as.data.frame(new.data2)
new.data2 <- expand.grid(new.data2)

new.data$fit   <- predict(rate.lm,new.data)
new.data$fit2  <- predict(rate.lm,new.data2)

names(new.data)
head(new.data)

par(mfrow = c(2,2))
contourplot(fit~A.num*C.num,new.data,xlab="A Temperature",ylab="C Concentration", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment")
contourplot(fit~A.num*C.num,new.data[new.data$D.num == -1,],xlab="A Temperature",ylab="C Concentration", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment: D = -1")
contourplot(fit~A.num*C.num,new.data[new.data$D.num ==  -0.5,],xlab="A Temperature",ylab="C Concentration", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment: D = -0.5")
contourplot(fit~A.num*C.num,new.data[new.data$D.num ==  0,],xlab="A Temperature",ylab="C Concentration", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment: D = 0")
contourplot(fit~A.num*C.num,new.data[new.data$D.num ==  1,],xlab="A Temperature",ylab="C Concentration", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment: D = 1")

contourplot(fit~C.num*D.num,new.data,xlab="C Concentration",ylab="D Stiring Rate", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment")
contourplot(fit~C.num*D.num,new.data[new.data$A.num == -1,],xlab="C Concentration",ylab="D Stiring Rate", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment")
contourplot(fit~C.num*D.num,new.data[new.data$A.num == 0,],xlab="C Concentration",ylab="D Stiring Rate", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment")
contourplot(fit~C.num*D.num,new.data[new.data$A.num == 1,],xlab="C Concentration",ylab="D Stiring Rate", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment: A = 1")




contourplot(fit~A.num*D.num,new.data,xlab="A Temperature",ylab="D Stiring Rate", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment: C = 1")
contourplot(fit~A.num*D.num,new.data[new.data$C.num == 0,],xlab="A Temperature",ylab="D Stiring Rate", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment: C = 0")


par(mfrow = c(2,2))
wirePlot(A, C, Filtration_qt, data = rate_qt,form = "Filtration_qt~A*C+A*D")
contourPlot(A, C, Filtration_qt, data = rate_qt,form = "Filtration_qt~A*C+A*D")
contourPlot(A, D, Filtration_qt, data = rate_qt,form = "Filtration_qt~A*C+A*D")
contourPlot(C, D, Filtration_qt, data = rate_qt,form = "Filtration_qt~A*C+A*D")

rate3             <- read.table("Data/Pilot_Plant_Filtration_Rate_Experiment3.csv",header=TRUE, sep = ";")
summary(rate3)
anova(aov(Rate~A*C+A*D,data=rate3))

rate4            <-  FrF2(2^3, 3, replications = 2, randomize = FALSE,factor.names = c("A", "C", "D"),repeat.only=TRUE)
rate4            <-  add.center(rate4,2)
Filtration4      <-  c(45 , 48 , 71 , 65 , 68 ,  80 , 60 , 65 , 43 , 45 , 100 , 104 , 75 , 70 , 86 , 96, 73,75,66,69)
rate4            <-  add.response(rate4, Filtration4)
anova(aov(Filtration2~A*C+A*D,  data=rate2))
anova(aov(Filtration4~A*C+A*D,  data=rate4))

summary(lm(Filtration4~A*C+A*D+iscube(rate4), rate4))

MEPlot(rate4[iscube(rate4)])
MEPlot(lm(Filtration4~A*C+A*D+iscube(rate4), rate4))




### Rest (bordel)
195.13 + 48.75 + 1.51
245.39



FrF2(4, 3, replications = 2,generators = "AB",randomize = FALSE)
FrF2(nfactors=7, resolution=5)

FrF2(8, 3, replications = 2, blocks = 2)


