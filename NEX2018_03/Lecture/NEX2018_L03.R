# R codes used in Lecture 03
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Some parts of code is from R Companion to Montgomery DAoE
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
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)



getwd()                                         # find where are you at the moment 
setwd("D:/Vyuka/NEX/2018/03_Factorial_design")  # Define working directory

##############################################################################
# Follow Chapter 4 - Montgomery DAOE
##############################################################################


# The Hardness Testing Example
# We use b = 4 blocks:

# input data
resp      = c(9.3, 9.4, 9.6, 10.0,  9.4, 9.3, 9.8, 9.9, 9.2, 9.4, 9.5, 9.7, 9.7, 9.6, 10.0, 10.2)
Data1     = data.frame(Tip=gl(4,4,16),block=gl(4,1,16),resp)
Data1

# visualization
boxplot(resp~Tip,  data=Data1,xlab="Tip",    main="Responses averaged over treatments")
boxplot(resp~block,data=Data1,xlab="blocks", main="Responses averaged over block")
with(Data1, interaction.plot(Tip,block,resp,col=1:6))
with(Data1, interaction.plot(block,Tip,resp,col=1:6))


# two-way Anova    and model adequacy checking
Data1.aov = aov(resp~block+Tip,Data1)
summary(Data1.aov)
Data1.aov2 = aov(resp~Tip,Data1)
summary(Data1.aov2)




opar <- par(mfrow=c(2,2),cex=.8)
plot(Data1.aov)
par(opar)

# choice of sample size for  Hardness Testing   data
# Suppose that we would like to be able to determine 
# the appropriate number of blocks to run if we are interested in
# detecting a true max difference in readings of 0.4 with a
# high probability and the estimate of the standard deviation is 0.1.

# Increasing the number of blocks increases the number of replicates 
# and the number of error degrees of freedom, making the design more sensitive

b      = seq(3,8,by=1)  # number of blocks
a     = 4               # number of treatment levels
D     = 0.4             # max difference in group means
sigma = 0.1             # standard deviation

Fi_sq = b*(0.4)^2/(2*a*(0.1^2)) 
Fi    = sqrt(Fi_sq) 
Fi
powers <- power.anova.test(groups=a,n=b,between.var = (D^2/2)/(a), within.var=sigma^2,sig.level=.05)$power
rbind(b , Fi, powers)



# Vascular Graft Experiment
x              <- scan("Data/vascgraft.txt")
PSI.labels     <- c(8500,8700,8900,9100)
vasc.graft     <- data.frame(PSI=gl(4,6,24),block=gl(6,1,24),x)

# visualization
boxplot(x~PSI,data=vasc.graft,xlab="PSI",main="Responses averaged over blocks")
boxplot(x~block,data=vasc.graft,xlab="Blocks", main="Responses averaged over treatments")
with(vasc.graft, interaction.plot(PSI,block,x,col=1:6))

# two-way Anova    and model adequacy checking
vasc.graft.aov <- aov(x~block+PSI,vasc.graft)
summary(vasc.graft.aov)
opar <- par(mfrow=c(2,2),cex=.8)
plot(vasc.graft.aov)
par(opar)

# delete the 10th observation
x2 <- x
x2[10] <- NA
vasc.graft2 <- data.frame(PSI=gl(4,6,24),block=gl(6,1,24),x2)

#We wish to estimate the missing value, so that its contribution to error sum
# of squares is minimal.


###########################################################
# Latin Square Design for the Rocket Propellant  Problem

rocket <- read.table("Data/rocket.txt",header=T)

matrix(rocket$treat,nr=5,byrow=T)

plot(y~op+batch+treat,rocket)

rocket.lm <- lm(y~factor(op)+factor(batch)+treat,rocket)
anova(rocket.lm)

plot.design(y~factor(op)+factor(batch)+treat,data=rocket)

# if we do not consider factor batch
rocket.lm2 <- lm(y~factor(op)+treat,rocket)
anova(rocket.lm2)

###########################################################
# Balanced incomplete Blocks design for Catalists Experiment


Catalists      = matrix(c(73,NA,73,75,74,75,75,NA,NA,67,68,72,71,72,NA,75),nc=4)
Catalists.df   = data.frame(rep=as.vector(tab.4.21),
                            treat=factor(rep(1:4,4)),
                            block=factor(rep(1:4,each=4)))

boxplot(rep~treat,  data=Catalists.df,xlab="Tip",    main="Responses averaged over treatments")
boxplot(rep~block,  data=Catalists.df,xlab="blocks", main="Responses averaged over block")                          

summary(aov(rep~treat+block+Error(block),Catalists.df))
anova(lm(rep~block+treat,Catalists.df))

Catalists.lm  = lm(rep~block+treat,Catalists.df)
summary(Catalists.lm)
treat.coef    =  Catalists.lm$coef[5:7]

# effect for catalyst 4 (baseline) is missing, so we add it
treat.coef    = c(0,treat.coef)
pairwise.diff = outer(treat.coef,treat.coef,"-")

summary(Catalists.lm)

crit.val <- qtukey(0.95,4,5)
ic.width <- crit.val*0.6982/sqrt(2)

xx <- pairwise.diff[lower.tri(pairwise.diff)]
plot(xx,1:6,xlab="Pairwise Difference
     (95% CI)",ylab="",xlim=c(-5,5),pch=19,cex=1.2,axes=F)
axis(1,seq(-5,5))
mtext(c("4-1","4-2","4-3","1-2","1-3","2-3"),side=2,at=1:6,line=2,las=2)
segments(xx-ic.width,1:6,xx+ic.width,1:6,lwd=2)
abline(v=0,lty=2,col="lightgray")

Catalists.lm.crd <- lm(rep~treat,Catalists.df)
(summary(Catalists.lm.crd)$sig/summary(Catalists.lm)$sig)^2

require(lattice)
xyplot(rep~treat|block,Catalists.df,
       aspect="xy",xlab="Catalyst",ylab="Response",
       panel=function(x,y) {
         panel.xyplot(x,y)
         panel.lmline(x,y)
       })

require(lme4)
print(Catalists.lm = lmer(rep~treat+(1|block),Catalists.df),corr=F)

print(Catalists.lm0 = lmer(rep~-1+treat+(1|block),Catalists.df))

coef(Catalists.lm)[[1]]$`(Intercept)`
mean(coef(Catalists.lm)[[1]][,1])

col <- c(1,1,0,1,1,1,0,0,0,1,0)
perm <- function(x) { 
  s <- length(x)
  m <- matrix(nc=s,nr=s)
  y <- rep(x,2)
  m[,1] <- x
  for (i in 2:s) { m[,i] <- y[i:(s+i-1)] }
  m
}
col.perm <- perm(col)
bib11 <- rbind(rep(0,11),col.perm)
# check that the design is well balanced
apply(bib11[-1,],1,sum)
apply(bib11,2,sum)


##############################################################################
# Chapter 5 and 6 - Montgomery DAOE
##############################################################################

##############################################################################
# The two-factor factorial design
##############################################################################

The two-factor factorial design

battery             <- read.table("Data/battery.txt",header=TRUE)
battery$Material    <- as.factor(battery$Material)
battery$Temperature <- as.factor(battery$Temperature)
summary(battery)
#show(battery)
head(battery)
attach(battery)


# box plot
opar <- par(mfrow=c(1,2),cex=.8)
plot(Life~Material,data=battery)
plot(Life~Temperature,data=battery)
par(opar)

# interaction plot
with(battery, interaction.plot(Temperature,Material,Life,type="b",pch=19, fixed=T,xlab="Temperature (.F)",ylab="Average life"))

# effects plot
plot.design(Life~Material*Temperature,data=battery)

#Now, the two-way ANOVA model, including an interaction effect, is computed as  follows:
battery.aov <- aov(Life~Material*Temperature,data=battery)
summary(battery.aov)
summary(battery.aov2 <- aov(Life~Material+Temperature,data=battery))
anova(battery.aov2,battery.aov)

opar <- par(mfrow=c(2,2),cex=.8)
plot(battery.aov)
par(opar)

# more checking of interaction

mm2 <- with(battery, tapply(Life,list(Material,Temperature),mean))
mm2 <- as.vector(mm2)
plot(fitted(battery.aov2)[seq(1,36,by=4)],mm2-fitted(battery.aov2)[seq(1,36,by=4)],
xlab="",ylab=expression(bar(y)[ij.]-hat(y)[ijk]),
pch=19,axes=FALSE,ylim=c(-30,30))
axis(1,at=seq(0,200,by=50),pos=0)
text(155,4,expression(hat(y)[ijk]),pos=4)
axis(2,at=seq(-30,30,by=10),las=1)
yy <- order(fitted(battery.aov2)[seq(1,36,by=4)])
xx.fit <- fitted(battery.aov2)[seq(1,36,by=4)]
yy.fit <- mm2-fitted(battery.aov2)[seq(1,36,by=4)]
lines(xx.fit[yy],predict(loess(yy.fit[yy]~xx.fit[yy])),col="lightgray",lwd=2)
# Any pattern in these quantities  is suggestive of the presence of interaction.
# This structure is the result of interaction between material types and temperature




# Compute means and variance for all levels and factors 

with(battery, tapply(Life,list(Material,Temperature),mean))
with(battery, tapply(Life,list(Material,Temperature),var))
with(battery, tapply(Life,list(Material,Temperature),sd))

# Model checking should be done on each ?treatment? (i.e. crossing each factor level together).
# Apply Tukey HSD test to test the pairwise comparisons between the treatment group means
TukeyMaterial = TukeyHSD(battery.aov,which="Material") 
TukeyMaterial
summary(battery.aov)
battery.aov_70 <- aov(Life~Material,data=battery[battery$Temperature==70,])
TukeyMaterial_70 = TukeyHSD(battery.aov_70,which="Material") 
TukeyMaterial_70
plot(TukeyMaterial_70)


TukeyTemperature = TukeyHSD(battery.aov,which="Temperature") 
TukeyTemperature



opar <- par(mfrow=c(1,2),cex=.8)
plot(TukeyMaterial)
plot(TukeyTemperature)
par(opar)



# Apply Fisher LSD test to test the pairwise comparisons between the treatment group means
# LSD.test(battery.aov, trt, DFerror, MSerror, alpha = 0.05, p.adj=c("none","holm", "hochberg", "bonferroni", "BH", "BY", "fdr"), group=TRUE, main = NULL)
LSDMaterial1 = LSD.test(battery.aov, "Material" ,alpha = 0.05, p.adj="none", group=TRUE, main = "Fisher LSD test for Battery data - pairwise comparisons between the treatment Material group means")
LSDMaterial2 = LSD.test(battery.aov, "Material" ,alpha = 0.05, p.adj="bonferroni", group=TRUE, main = "Fisher LSD test for Battery data - pairwise comparisons between the treatment Material group means")
plot(LSDMaterial1)

LSDTemperature1 = LSD.test(battery.aov, "Temperature" ,alpha = 0.05, p.adj="none", group=TRUE, main = "Fisher LSD test for Battery data - pairwise comparisons between the treatment Material group means")
LSDTemperature2 = LSD.test(battery.aov, "Temperature" ,alpha = 0.05, p.adj="bonferroni", group=TRUE, main = "Fisher LSD test for Battery data - pairwise comparisons between the treatment Material group means")
plot(LSDTemperature1)

# Since the interaction in the model is significant, the effect of Material depends on which level of Temperature is considered.
# we compute the three means at Temperature=70.F
mm <- with(subset(battery, Temperature==70),aggregate(Life,list(Material=Material),mean))
mm
# next the studentized t quantile times the error type (based on pooled SD from ANOVA)
val.crit <- qtukey(.95,3,27)*sqrt(unlist(summary(battery.aov))[["Mean Sq4"]]/4)
val.crit
# finally compare the observed difference of means with the critical value
diff.mm <- c(d.3.1=mm$x[3]-mm$x[1],d.3.2=mm$x[3]-mm$x[2],d.2.1=mm$x[2]-mm$x[1])
diff.mm
names(which(diff.mm > val.crit))
#In conclusion, only Material type 3 vs. type 1 and Material type 2 vs. type 1 appear to be significantly different when Temperature is fixed at 70.F.


# Choice of sample size 5.3.5
n     = c(2, 3, 4, 5)
a     = 3               # number of treatment levels    A
b     = 3               # number of treatment levels    B
D     = 40              # max difference in group means
sigma = 25              # standard deviation

Fi_sq = (n*b*(D)^2)/(2*a*(sigma^2))
Fi    = sqrt(Fi_sq)
errorDF = a*b*(n-1)
powers  = power.anova.test(groups=3, n=n, between.var = (n*(D^2))/((n-1)), within.var=sigma^2,sig.level=.05)$power
rbind(n , Fi ,errorDF, powers)


# General Factorial Design - Response curves, surface

battery$Temperature.num <- as.numeric(as.character(battery$Temperature))
battery.aov3 <- aov(Life~Material+Temperature.num+I(Temperature.num^2)+Material:Temperature.num+Material:I(Temperature.num^2), data = battery)
summary(battery.aov3)

battery.lm2 <- lm(Life~Material+Temperature.num+Material:I(Temperature.num^2), data = battery)
summary(battery.lm2)


new       <- data.frame(Temperature.num=rep(seq(15,125,by=5),3),(Material=gl(3,23)))
new$fit   <- predict(battery.aov3,new)
opar      <- par(las=1)
# we first plot the fitted values
with(new, interaction.plot(Temperature.num,Material,fit,legend=FALSE,xlab="Temperature",ylab="Life",ylim=c(20,190)))
txt.leg <- paste("Material type",1:3)
text(5:7,new$fit[new$Temperature.num==c(45,55,65)]-c(3,3,-20),txt.leg,pos=1)
# next the observed values
points(rep(c(1,15,23),each=12),battery$Life,pch=19)
par(opar)



### Another example

impurity             <- read.table("Data/impurity.txt",header=TRUE)
impurity$Temperature <- as.factor(impurity$Temperature)
impurity$Pressure    <- as.factor(impurity$Pressure)
# we have only one replication - interaction hidden in errors
summary(impurity.aov <- aov(N~Temperature*Pressure,data=impurity))
# model without interaction
summary(impurity.aov <- aov(N~Temperature+Pressure,data=impurity))

library(alr3)
residual.plots(lm(N~Temperature+Pressure,impurity))


##############################################################################
#  General factorial design, response curves and surfaces
##############################################################################

tool      <- read.table("Data/toollife.txt",header=TRUE)
tool.lm   <- lm(Life~Angle*Speed+I(Angle^2)*I(Speed^2)+Angle:I(Speed^2)+I(Angle^2):Speed,tool)
summary(tool.lm )
tmp.angle <- seq(15,25,by=.1)
tmp.speed <- seq(125,175,by=.5)
tmp       <- list(Angle=tmp.angle,Speed=tmp.speed)
new       <- expand.grid(tmp)
new$fit   <- c(predict(tool.lm,new))

require(lattice)
contourplot(fit~Angle*Speed,data=new,cuts=8,region=T,col.regions=gray(7:16/16))


bottling <- read.table("Data/bottling.txt",header=TRUE, colClasses=c("numeric",rep("factor",3)))
summary(bottling)
attach(bottling)

opar   <- par(mfrow=c(2,2),cex=.8)
boxplot(Deviation~.,data=bottling,las=2,cex.axis=.8,ylab="Deviation")
abline(h=0,lty=2)
par(las=1)
mm    <- with(bottling, tapply(Deviation,Carbonation,mean))
ss    <- with(bottling, tapply(Deviation,Carbonation,sd))
bp    <- barplot(mm,xlab="Carbonation",ylab="Deviation",ylim=c(-2,9))
arrows(bp,mm-ss/sqrt(4),bp,mm+ss/sqrt(4),code=3,angle=90,length=.1)
with(bottling, interaction.plot(Carbonation,Pressure,Deviation,type="b"))
with(bottling, interaction.plot(Carbonation,Speed,Deviation,type="b"))
par(opar)

summary(bottling.aov  <- aov(Deviation~.^3,bottling))
summary(bottling.aov2 <- aov(Deviation~.,bottling))
anova(bottling.aov2,bottling.aov)

detach(bottling)






##############################################################################
#  Blocking in a factorial design
##############################################################################

intensity <- read.table("Data/intensity.txt",header=TRUE, colClasses=c("numeric",rep("factor",3)))
require(lattice)
xyplot(Intensity~Ground|Operator,data=intensity,groups=Filter,panel=function(x,y,...)
       {
        subs <- list(...)$subscripts
        panel.xyplot(x,y,pch=c(1,19),...)
        panel.superpose(x,y,panel.groups="panel.lmline",lty=c(1,2),...)
       },
       key=list(text=list(lab=as.character(1:2)),lines=list(lty=1:2,col=1),
       corner=c(1,.95),title="Filter Type",cex.title=.8),col=1)

intensity.aov <- aov(Intensity~Ground*Filter+Error(Operator),intensity)
summary(intensity.aov)
intensity.aov



##############################################################################
#  The 2^2 design
##############################################################################

yield <- read.table("Data/yield.txt",header=T)
attach(yield)
rm(yield)
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
yield.lm              ## gives the coefficients of the LM

s3d           <- scatterplot3d(reactant.num,catalyst.num,yield,type="n",angle=135,scale.y=1,xlab="Reactant",ylab="Catalyst")
s3d$plane3d(yield.lm,lty.box="solid",col="darkgray")

tmp           <- list(reactant.num=seq(15,25,by=.5),catalyst.num=seq(1,2,by=.1))
new.data      <- expand.grid(tmp)
new.data$fit  <- predict(yield.lm,new.data)
contourplot(fit~reactant.num+catalyst.num,new.data,xlab="Reactant",ylab="Catalyst")





