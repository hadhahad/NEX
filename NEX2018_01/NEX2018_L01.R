# 01NEX - R codes used in Lecture 01
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Some parts of code is from R Companion to Montgomery's DAoE

#Instalation of some libraries
library(car)     # provides a set of useful functions for ANOVA designs and Regression Models;
library(lattice) # provides some graphical enhancements compared to traditional R graphics, as well as multivariate displays capabilities;
library(pwr)     # power analysis
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)
library(evir) # extremal distributions
library(MASS)
library(sm)
library(boot) 
library(gplots)
library(ggplot2)
library(nortest)


# Plot of standard normal distribution probability function (from definition)
x = seq(-4,4,length=200)
y = 1/sqrt(2*pi)*exp(-x^2/2)
plot(x,y,type="l",lwd=2,col="red")

# Plot of standard normal distribution probability function (used R function dnorm)
x=seq(-4,4,length=200)
x=dnorm(x,mean=0,sd=1)
plot(x,type="l",lwd=2,col="red")


# Display the Student's t distributions with various
# degrees of freedom and compare to the normal distribution
x <- seq(-4, 4, length=100)
hx <- dnorm(x)
degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")
plot(x, hx, type="l", lty=2, xlab="x value", ylab="Density", main="Comparison of t Distributions")
for (i in 1:4){
               lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
               }
legend("topright", inset=.05, title="Distributions", labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)


# Display the Chisq distributions with various
# degrees of freedom and compare to the normal distribution
x      <- seq(0, 20, length=100)
degf   <- c(1, 3, 5 ,10, 20)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=5","df=10", "df=20")
plot(x,  dchisq(x,degf[1]), pch = "." , lwd=2, xlab="x value", col = "red", ylab="Density", main="Comparison of Chi-squared Distributions")
for (i in 1:5){
               lines(x, dchisq(x,degf[i]), lwd=2, col=colors[i])
              }
legend("topright", inset=.05, title="Distributions", labels, lwd=2, lty=c(1, 1, 1, 1, 1), col=colors)




# Random generation for diferent distributions, plot histograms and show how works CLT

n = 1000                # number of observations
m = seq(-5, 5, 0.01)
cells = 18

x0 = rnorm(n,mean=0,sd=1)
x1 = rt(n,10)
x2 = rchisq(n, 15, ncp=0)
x3 = rf(n, 4, 10)
x4=  runif(n, 0, 1)
x6 = rlnorm(n, meanlog = 0, sdlog = 0.125)
x7 = rweibull(n, shape = 5, scale = 1)


for (k in 1:n) {
    x0 = rnorm(n,mean=0,sd=1)
    x1 = rt(n,10)
    x2 = rchisq(n, 15, ncp=0)
    x3 = rf(n, 4, 10)
    x4=  runif(n, 0, 1)
    y[k]  = sum(x0+x1+x2+x3+x4)/n
}
mean_y = 10/8 + 0.5 + 15

par(mfrow = c(3,2))
hist(  x0, breaks = cells, prob=TRUE, ylim = c(0,0.5),main="normal(0,1)")
lines( m, dnorm(m, mean=0, sd=1), lty=3)
rug(   x0)
hist(x1, breaks = cells, prob=TRUE, ylim = c(0,0.5), main="t(5)")
lines( seq(min(x1), max(x1), 0.01), dt(seq(min(x1), max(x1), 0.01),10), lty=3)
rug(   x1)
hist(x2, breaks = cells, prob=TRUE, main="chisq(15)")
lines( seq(min(x2), max(x2), 0.01), dchisq(seq(min(x2), max(x2), 0.01), 15), lty=3)
rug(   x2)
hist(x3, breaks = cells, prob=TRUE, main="F(4,10)")
lines( seq(min(x3), max(x3), 0.01), df(seq(min(x3), max(x3), 0.01), 4,10), lty=3)
rug(   x3)
hist(x4, breaks = cells, prob=TRUE, main="uniform(0,1)")
lines( seq(min(x4), max(x4), 0.01), dunif(seq(min(x4), max(x4), 0.01), 0, 1), lty=3)
rug(   x4)
#hist(x5, breaks = cells, prob=TRUE, main="lnorm(0,0.125)")
#lines( m, dlnorm(m, meanlog=0, sdlog = 0.125), lty=3)
#rug(   x5)
#hist(x6, breaks = cells, prob=TRUE, main="weibull(5,1)")
#lines( m, dweibull(m,shape = 5, scale = 1), lty=3)
#rug(   x6)
hist(y, breaks  = 3*cells, prob=TRUE, main="application of CLT")
lines( seq(min(y), max(y), 0.01), dnorm(seq(min(y), max(y), 0.01), mean=mean_y, sd=sqrt(var(y))), lty=3)
#lines(density(y, bw=0.5))
rug(   y)




# Tension Bond Strength data for the Portland Cement Formulation Experiment
# Mongomery - Design and Analysis of Experiment 6th Edition (Tab. 2-1, p. 24)

# Tension Bond Strength data (Tab. 2-1, p. 24)
y1      = c(16.85,16.40,17.21,16.35,16.52,17.04,16.96,17.15,16.59,16.57)
y2      = c(16.62,16.75,17.37,17.12,16.98,16.87,17.34,17.02,17.08,17.27)
y       = c(y1,y2)
y.means = c(mean(y1),mean(y),mean(y2))
y.sd    = c(sd(y1),sd(y),sd(y2))
opar    = par(mfrow=c(2,1),mar=c(5,7,4,2),las=1)
stripchart(data.frame(Modified=y1,Together=y,Unmodified=y2),xlab=expression("Strength (kgf/cm^2)"),pch=19,xlim = c(16.2,17.6))
arrows(c(y.means,y.means[2]),c(1.5,1.5,2.5,2.5),c(y.means,y.means[2]),c(1.1,1.9,2.9,2.1),length=.1)
text(y.means,c(1.2,1.8,2.8),round(y.means,2),pos=4,cex=.8)
rd = rnorm(1000,mean=y.means[2],sd=y.sd[2])        # if y is data frame: sd = as.numeric(apply(y,2,sd))
hist(y,xlab="Observations",ylab="Relative frequency", main="Histogram for Tension Bold Strength Data", breaks = 7, freq = FALSE, ylim=c(0,1.6), xlim = c(16.2,17.6) )
partition = seq(min(rd ), max(rd ), 0.01)
lines(partition, dnorm(partition, y.means[2], y.sd[2]), col = "red")
lines(partition, dnorm(partition, y.means[1], y.sd[1]), col = "blue")
lines(partition, dnorm(partition, y.means[3], y.sd[3]), col = "blue")
par(opar)

opar    = par(mfrow=c(1,1))
boxplot(data.frame(Modified=y1,Unmodified=y2),ylab="Strength (kgf/cm^2)",las=1, main = "Box plots for the portland cement tension bond strength data")


cement_data = data.frame(Strength=c(y1,y2),Modification = c(rep("Modified",times=length(y1)),rep("Unmodified",times=length(y2))))
summary(cement_data)
qplot(Modification, Strength, data=cement_data, geom=c("boxplot", "jitter"),
      fill=Modification,
      xlab= "Modification",
      ylab="Strength (kgf/cm^2)")

## est densities
colors <- c("red", "blue", "darkgreen")
labels <- c("Modified", "Unmodified", "Together")
plot(density(y1,bw = "nrd0", adjust = 2,), xlim = c(15,19), ylim = c(0,1.4), col = colors[1], xlab = "", main = "")
par(new=TRUE)   
plot(density(y2,bw = "nrd0", adjust = 2,), xlim = c(15,19), ylim = c(0,1.4), col = colors[2], xlab = "", main = "")
par(new=TRUE)   
plot(density(y,bw = "nrd0", adjust = 2,), xlim = c(15,19),  ylim = c(0,1.4), col = colors[3], xlab = "")
legend("topright", inset=.05, title="Densities", labels, lwd=2, lty=c(1, 1, 1), col=colors)



## Plot using a qqplot
colors <- c("red", "blue", "darkgreen")
labels <- c("Modified", "Unmodified", "Together")
plot.new()
qqnorm(y1, pch = 3, lwd = 2, col = "red", ylim = c(16, 17.8))
qqline(y1, col = "red")
par(new=TRUE)     
qqnorm(y2, pch = 4, lwd = 2, col = "blue", ylim = c(16, 17.8))
qqline(y2, col = "blue")
par(new=TRUE)     
qqnorm(y, pch = 1, lwd = 2, col = "darkgreen", ylim = c(16, 17.8))
qqline(y, col = "darkgreen")
legend("topleft", inset=.05, title="Legend", labels, lwd=2, lty=c(1, 1, 1), col=colors)





var.test(y1, y2, ratio = 1, alternative = "two.sided", conf.level = 0.95)
var.test(y1, y2, ratio = 1, alternative = "two.sided", conf.level = 0.99)

t.test(y1, y2 , alternative = "two.sided", mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
t.test(y1, y2 , alternative = "two.sided", mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.99)
t.test(y1, y2 , alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(y1, y2 , alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.99)

n1 = length(y1)
n2 = length(y2)
S1 = sd(y1)
S2 = sd(y2)
Sp2 = ((n1-1)*S1^2 + (n2-1)*S2^2)/(n1+n2-2)  
Sp = sqrt(Sp2)

t0 = (mean(y1) - mean(y2))/(Sp*sqrt(1/n1 + 1/n2))

qt(.975, df = 18)
qt(.025, df = 18)

# confidence interval
L_CI = (mean(y1) - mean(y2) - qt(.975, df = 18)*Sp*sqrt(1/n1 + 1/n2))
U_CI = (mean(y1) - mean(y2) + qt(.975, df = 18)*Sp*sqrt(1/n1 + 1/n2))
L_CI
U_CI


# Performs the test of normality.

shapiro.test(y1)
ks.test(y1, "pnorm", mean(y1), sd(y1))
ad.test(y1)

shapiro.test(y2)
ks.test(y2, "pnorm", mean(y2), sd(y2))
ad.test(y2)

shapiro.test(y)
ks.test(y, "pnorm", mean(y), sd(y))
ad.test(y)

#pwr.t.test(10, d = NULL, sig.level = 0.05, power = NULL, type = c("two.sample", "one.sample", "paired"), alternative = c("two.sided", less","greater")
#power.t.test(n = NULL, delta = NULL, sd = 1, sig.level = 0.05, power = NULL, type = c("two.sample", "one.sample", "paired"),alternative = c("two.sided", "one.sided"), strict = FALSE)
diff.t  = abs(mean(y1) - mean(y2))
sigma.t = 0.284
#calculation of requiared sample size
power.t.test(power = .95,    delta = diff.t, sd = sigma.t, sig.level = 0.05, type = "two.sample", alternative = "two.sided")

#calculation of the power of the test
power.t.test(n = 10, delta = diff.t, sd = sigma.t, sig.level = 0.05, type = "two.sample", alternative = "two.sided")
pwr.t.test(  n = 10,     d =  (diff.t/sigma.t)   , sig.level = 0.05, type = "two.sample", alternative = "two.sided")



ptab<-cbind(NULL, NULL)       # initalize ptab

for (i in c(.1, .15, .2, .25, .30, .35, .40, .45, .50, 0.55, 0.6)){
  pwrt<-power.t.test(sd=0.284,delta=i,power=.8,sig.level=.05,type="two.sample",alternative="two.sided")
  ptab<-rbind(ptab, cbind(pwrt$d, pwrt$n))
}

ptab
png(paste("Sample_curve.png",sep=""),width = 1200, height = 600)
plot(ptab[,1],ptab[,2],type="b",xlab="effect size delta",ylab="sample size")
par(opar) 
dev.off()


pwrt<-power.t.test(delta=0.278, sd=0.284, n=c(5,10,15,20,25,30,35,40,45,50,100),sig.level=.05,type="two.sample",alternative="two.sided")

png(paste("Power_curve.png",sep=""),width = 1200, height = 600)
plot(pwrt$n,pwrt$power,type="b",xlab="sample size",ylab="power")
par(opar) 
dev.off()


# ANOVA test - (will be done in the second lecture)
data.cement = data.frame(Response =(c(y1,y2)), Factor =  c(rep("Modified",10), (rep("Unmodified",10))))
summary(aov(Response~Factor,data=data.cement))
anova(lm(Response~Factor,data=data.cement))
anova(lm(Response~Factor-1,data=data.cement))







