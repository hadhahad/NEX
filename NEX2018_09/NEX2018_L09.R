# R codes used in 01NEX - Lecture 09
# Written by J. Franc - jiri.franc@fjfi.cvut.cz


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
library(rsm)           # Response-Surface Methods
library(qualityTools)
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)


# Define directory
setwd("D:/Vyuka/NEX/2018/09_3k_factorial_design/")

data_55   <- read.table("Data/Ex55.csv",header=T,sep=";")
data_55f  = data_55
data_55f$Angle = factor(data_55f$Angle)
data_55f$Speed = factor(data_55f$Speed)

data_55c = data_55
data_55c$Angle = data_55c$Angle -20
data_55c$Speed = data_55c$Speed -150

model55_1 = lm(Life ~ Angle * Speed,data = data_55)
model55_1f = lm(Life ~ Angle * Speed,data = data_55f)

summary(model55_1f)
summary(aov(lm(Life~Angle*Speed,data=data_55f)))

summary(lm(Life~Angle+Speed,data=data_55))

data_55
summary(lm(Life ~ Angle + Speed + Angle:Speed + I(Angle^2)+I(Speed^2) 
           + I(Angle^2):Speed+I(Speed^2):Angle + I(Angle^2):I(Speed^2)  ,data = data_55))

summary(aov(lm(Life ~ Angle + Speed + Angle:Speed + I(Angle^2)+I(Speed^2) 
           + I(Angle^2):Speed+I(Speed^2):Angle + I(Angle^2):I(Speed^2)  ,data = data_55)))


##############################################################################
# Follow Chapter 9 & 11 - Montgomery DAOE
##############################################################################

#EXAMPLE 11 - 1,2

time  = c(30, 40, 30, 40)
temp  = c(150, 150, 160,160)
yield = c(39.3, 40.9, 40.0, 41.5)
x1 = (time - 35)/5
x2 = (temp - 155)/5
data11_1 = data.frame(time,temp,x1,x2,yield)

model1 = lm( yield ~ x1 + x2,data = data11_1)
summary(model1)


#mod1=lm(yield~x1+x2+I(x1^2)+I(x2^2)+x1*x2,data=data11_1)
mod1=lm(yield~x1+x2,data=data11_1)
summary(mod1)
x=seq(-1,1,0.1); 
y=seq(-1,1,0.1)
f = function(x,y){
   new.x=data.frame(x1=x,x2=y)
   predict(mod1,new.x)
   }
z = outer(x,y,f)
persp(x,y,z,theta=30,phi=30,expand=0.5,col="lightblue", xlab="Time",ylab="Temperature",zlab="Yield")
image(x,y,z,axes=F,xlab="Time",ylab="Temperature")
contour(x,y,z,levels=seq(35,45,by=0.1),add=T,col="peru")
axis(1,at=seq(-1,1,by=1),labels=c(55,60,65))
axis(2,at=seq(-1,1,by=1),labels=c(160,165,170))
box()

pf(0.063,1,4, lower.tail=F)

time  = c(30, 40, 30, 40, 35,35,35,35,35)
temp  = c(150, 150, 160,160, 155,155,155,155,155)
yield = c(39.3, 40.9, 40.0, 41.5, 40.5, 40.3, 40.7, 40.2,40.6)
x1 = (time - 35)/5
x2 = (temp - 155)/5
data11_2 = data.frame(time,temp,x1,x2,yield)

model2 = lm( yield ~ x1*x2,data = data11_2)
summary(model2)
summary(lm( yield ~ x1+x2+I(x1^2)+I(x2^2)+x1*x2,data = data11_2))
summary(lm( yield ~ time+temp+I(time^2)+I(temp^2)+time*temp,data = data11_2))




# dataset from Montgomery DAOE - (old version)
time  = c(80,90,80,90,78,92,85,85,85,85,85,85,85)
temp  = c(170,170,180,180, 175,175,168,182,175,175,175,175,175)
yield = c(76.3,79.2 ,77.0 ,80.8 ,75.5 ,80.0 ,75.0 ,77.5 ,80.9 ,79.7 ,80.5 ,80.4 ,80.0 )


#Time Temperature x1 x2 Yield
#80 170 -1    -1     76.3
#90 170  1    -1     79.2
#80 180 -1     1     77.0
#90 180  1     1     80.8
#78 175 -1.414 0     75.5
#92 175  1.414 0     80.0
#85 168  0    -1.414 75.0
#85 182  0     1.414 77.5
#85 175  0     0     80.9
#85 175  0     0     79.7
#85 175  0     0     80.5
#85 175  0     0     80.4
#85 175  0     0     80.0
x1 = (time - 85)/5
x2 = (temp - 175)/5
data11_3 = data.frame(time,temp,x1,x2,yield)

model3=lm(yield~x1+x2+I(x1^2)+I(x2^2)+x1*x2,data=data11_3)
summary(model3)
summary(aov(model3))


model4=lm(yield~x1+x2+I(x1^2)+I(x2^2),data=data11_3)
summary(model4)

x=seq(-1.5,1.5,0.2); 
y=seq(-1.5,1.5,0.2)
f = function(x,y){
  new.x=data.frame(x1=x,x2=y)
  predict(model4,new.x)
}
z = outer(x,y,f)
persp(x,y,z,theta=30,phi=30,expand=0.5,col="lightblue", xlab="Time",ylab="Temperature",zlab="Yield")
image(x,y,z,axes=F,xlab="Time",ylab="Temperature")
contour(x,y,z,levels=seq(70,90,by=0.5),add=T,col="peru")
axis(1,at=seq(-1,1,by=1),labels=c(80,85,90))
axis(2,at=seq(-1,1,by=1),labels=c(170,175,180))
box()





# dataset from Montgomery DAOE - eight edition

#Time Temperature x1      x2    y1   y2 y3 
#80      170     -1      -1     76.5 62 2940
#80      180     -1       1     77.0 60 3470
#90      170      1      -1     78.0 66 3680
#90      180      1       1     79.5 59 3890
#85      175      0       0     79.9 72 3480
#85      175      0       0     80.3 69 3200
#85      175      0       0     80.0 68 3410
#85      175      0       0     79.7 70 3290
#85      175      0       0     79.8 71 3500
#92.07   175      1.414   0     78.4 68 3360
#77.93   175     -1.414   0     75.6 71 3020
#85      182.07   0       1.414 78.5 58 3630
#85      167.93   0      -1.414 77.0 57 3150


time  = c(80,80,90,90,85,85,85,85,85,92.07,77.93,85,85)
temp  = c(170,180,170,180, 175,175,175,175,175,175,175,182.07,167.93)
y1    = c(76.5,77.0,78.0,79.5,79.9,80.3,80.0,79.7,79.8,78.4,75.6,78.5,77.0)
y2    = c(62,60,66,59,72,69,68,70,71,68,71,58,57)
y3    = c(2940,3470,3680,3890,3480,3200,3410,3290,3500,3360,3020,3630,3150)

x1 = (time - 85)/5
x2 = (temp - 175)/5
data11_3b = data.frame(time,temp,x1,x2,y1,y2,y3)

model3b=lm(y1~x1+x2+I(x1^2)+I(x2^2)+x1:x2,data=data11_3b)
summary(model3b)

summary(aov(model3b))

model3bb=lm(y1~x1+x2+I(x1^2)+I(x2^2),data=data11_3b)
summary(model3bb)
summary(aov(model3bb))

model3b$coeff

b = matrix(c(model3b$coeff[2],model3b$coeff[3]),2,1)
B = matrix(c(model3b$coeff[4], model3b$coeff[6]/2,
             model3b$coeff[6]/2,model3b$coeff[5]),2,2) 
cbind(b,B)
x_stat       = -1/2 * solve(B) %*% b
x_stat_natur = c((5*x_stat[1]+85), (5*x_stat[2] +175))
y_stat_natur = predict(model3b,data.frame(x1 = x_stat[1], x2 = x_stat[2]))
x_stat
x_stat_natur
y_stat_natur

x=seq(-1.5,1.5,0.01); 
y=seq(-1.5,1.5,0.01)
f = function(x,y){
    new.x=data.frame(x1=x,x2=y)
    predict(model3b,new.x)
}
z = outer(x,y,f)
persp(x,y,z,theta=30,phi=30,expand=0.5,col="lightblue", xlab="Time",ylab="Temperature",zlab="Yield")
image(x,y,z,axes=F,xlab="Time",ylab="Temperature")
contour(x,y,z,levels=seq(70,90,by=0.5),add=T,col="peru")
points(x_stat[1],x_stat[2],pch=19)
text(x_stat[1]+0.05,x_stat[2]+0.05,"Stationary point",pos=4,cex=.6)
axis(1,at=seq(-1,1,by=1),labels=c(80,85,90))
axis(2,at=seq(-1,1,by=1),labels=c(170,175,180))
box()





model_y1=lm(y1~x1+x2+I(x1^2)+I(x2^2)+x1:x2,data=data11_3b)
summary(model_y1)

model_y2=lm(y2~x1+x2+I(x1^2)+I(x2^2)+x1:x2,data=data11_3b)
summary(model_y2)

model_y3=lm(y3~x1+x2,data=data11_3b)
summary(model_y3)


f1 = function(x,y){
    new.x=data.frame(x1=x,x2=y)
    predict(model_y1,new.x)
}
f2 = function(x,y){
    new.x=data.frame(x1=x,x2=y)
    predict(model_y2,new.x)
}
f3 = function(x,y){
    new.x=data.frame(x1=x,x2=y)
    predict(model_y3,new.x)
}
z1 = outer(x,y,f1)
z2 = outer(x,y,f2)
z3 = outer(x,y,f3)

op <- par(mfrow=c(2,3))

persp(x,y,z,theta=30,phi=30,expand=0.5,col="lightblue", xlab="Time",ylab="Temperature",zlab="Yield")
persp(x,y,z2,theta=30,phi=30,expand=0.5,col="lightblue", xlab="Time",ylab="Temperature",zlab="Yield")
persp(x,y,z3,theta=30,phi=30,expand=0.5,col="lightblue", xlab="Time",ylab="Temperature",zlab="Yield")

image(x,y,z,axes=F,xlab="Time",ylab="Temperature")
contour(x,y,z,levels=seq(70,90,by=0.5),add=T,col="peru")
axis(1,at=seq(-1,1,by=1),labels=c(80,85,90))
axis(2,at=seq(-1,1,by=1),labels=c(170,175,180))
box()

image(x,y,z2,axes=F,xlab="Time",ylab="Temperature")
contour(x,y,z2,levels=seq(50,100,by=1),add=T,col="peru")
axis(1,at=seq(-1,1,by=1),labels=c(80,85,90))
axis(2,at=seq(-1,1,by=1),labels=c(170,175,180))
box()

image(x,y,z3,axes=F,xlab="Time",ylab="Temperature")
contour(x,y,z3,levels=seq(3000,4000,by=50),add=T,col="peru")
axis(1,at=seq(-1,1,by=1),labels=c(80,85,90))
axis(2,at=seq(-1,1,by=1),labels=c(170,175,180))
box()

par(op)


# find
#     y1 (yield)               > 78.5,
#62 < y2 (viscosity)           < 68
#     y3 (molecular weight Mn) < 3400

z4 = z1
z4[z1<78.5]         = 0
z4[(z2<62)|(z2>68)] = 0
z4[z3>3400]         = 0
z4[z4>0]            = 1 
table(z4)

opar <- par(mfrow=c(1,1))
contour(x,y,z1,axes=F,levels=seq(78.5,90,by=50),col="peru")
contour(x,y,z2,levels=seq(62,68,by=6),add=T,col="red")
contour(x,y,z3,levels=seq(3400,4000,by=1000),add=T,col="blue")
.filled.contour(x,y,z4,levels=c(0.1,1.1),col="red")
axis(1,at=seq(-1.4,1.4,by=0.2),labels=seq(78,92,by=1))
axis(2,at=seq(-1.4,1.4,by=0.2),labels=seq(168,182,by=1))
par(opar)


#.filled.contour(x,y,z,levels=c(78.5,90),col=c(0,rgb(1,1,0,0.5)))
#.filled.contour(x,y,z,levels=c(78.5,90),col="red")

# ??? can I use filled.contour ???



## toollife experiment
# tool     <- read.table("Data/toolife.txt",header=TRUE)
# tool$f.Angle <- factor(tool$Angle)
# tool$f.Speed <- factor(tool$Speed)
# summary(tool)
# tool_anova1 <- with(tool,anova(lm(Life~Angle*Speed+Angle^2+Speed^2)))
# tool_anova1 
# summary(lm(Life~Angle+Speed+Angle:Speed+I(Angle^2)+I(Speed^2), data = tool))
# 
# 


