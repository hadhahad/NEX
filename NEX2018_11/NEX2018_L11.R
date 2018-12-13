# 01NEX - R codes used in Lectures 11
# Written by J. Franc - jiri.franc@fjfi.cvut.cz

#Instalation of any library
library(car)          # provides a set of useful functions for ANOVA designs and Regression Models;
library(lattice)      # provides some graphical enhancements compared to traditional R graphics, as well as multivariate displays capabilities;
library(lme4)         # the newer and enhanced version of the nlme package, for which additional data structure are available (nested or hierarchical model,. . . );
library(nlme)         # for handling mixed-effects models;
library(gregmisc)
library(pwr)          # power analysis
library(agricolae)    # for Fisher LSD method
library(scatterplot3d)# for 3d scatter plot
library(alr3)
library(FrF2)          #for 2^k  factorial design
library(DoE.base)      # Full factorials, orthogonal arrays and base utilities for DoE packages
library(rsm)           # Response-Surface Methods
library(qualityTools)
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)


setwd("D:/Vyuka/NEX/2018/11_Nested_and_split_plot/")



###############################  An Introduction to   Statistical Methods   and Data Analysis
tablets <- read.table(file="Data/tablets.txt",header=TRUE)
tablets$site = factor(tablets$site)
tablets$batch = factor(tablets$batch)

# wrong way
summary(aov(content ~ site*batch , data=tablets))

#better way
summary(aov(content ~ site + batch%in%site , data=tablets))
summary(aov(content ~ site/batch, data=tablets) )
summary(aov(content ~ site + Error(batch %in% site), data=tablets))

#good way
summary(lme (content ~ 1,    random = ~1|site/batch, data = tablets))
summary(lmer(content ~ 1 + (1|site/batch),        data = tablets))

summary(lme (content ~ site, random = ~1|site/batch, data = tablets))
summary(lmer(content ~ site + (1|site/batch),        data = tablets))

m1 = lme(content ~ site, random = ~1|site/batch, data = tablets)
m2 = lm(content ~ site, data = tablets)
anova(m1,m2)





data_purity <- read.table("Data/purity.txt",header=TRUE,sep=";")
data_purity$Suppliers <- factor(data_purity$Suppliers, labels = c("Supplier 1", "Suplier 2", "Suplier 3"))
data_purity$Batches   <- factor(data_purity$Batches)

head(data_purity)
str(data_purity)
summary(data_purity)


# Wrong way !!!!
purity_m1_lm   <- lm(Purity ~ Suppliers+Batches, data = data_purity)
summary(purity_m1_lm)
anova(purity_m1_lm)

# Better, but still wrong !
purity_m1_lme   <- lme(Purity ~ Suppliers, random = ~1|Batches, data = data_purity)
summary(purity_m1_lme)
anova(purity_m1_lme)


# Better but still wrong - acount with nested design, but not mixed one
purity_m1_lm   <- lm(Purity ~ Suppliers/Batches, data = data_purity)
summary(purity_m1_lm)
anova(purity_m1_lm)

data_purity$Batches2 <- factor(c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,10,11,11,11,12,12,12))
purity_m2_lm   <- lm(Purity ~ Suppliers+Batches2, data = data_purity)
summary(purity_m2_lm)
anova(purity_m2_lm)


# anova with  fixed both nad batches nested
summary(aov(Purity ~ Suppliers + Batches%in%Suppliers , data=data_purity))
summary(aov(Purity ~ Suppliers/Batches, data=data_purity))
summary(aov(Purity ~ Suppliers + Batches2, data=data_purity))


# anova with batches nested and random 
summary(aov(Purity ~ Suppliers + Error(Batches %in% Suppliers), data=data_purity))
summary(aov(Purity ~ Suppliers + Error(Suppliers/Batches), data=data_purity))
summary(aov(Purity ~ Suppliers + Error(Batches2), data=data_purity))


aov.after.age.time <- aov(value ~ age*time + Error(subject/time), data=data.long)
summary(aov.after.age.time)


library(nlme)
# Right way
purity_m2_lme   <- lme(Purity ~ Suppliers, random = ~1|Batches2, data = data_purity)
summary(purity_m2_lme)
anova(purity_m2_lme)
VarCorr(purity_m2_lme)
intervals(purity_m2_lme)

purity_m2_lme <- lme(Purity ~ 1, random = ~1|Suppliers/Batches, data = data_purity)
summary(purity_m2_lme)
anova(purity_m2_lme)



purity_m2_lme2 <- lme(Purity ~ Suppliers, random = ~ 1|Batches2, data = data_purity, method="ML")
#lme(Drug~1, data=content, random=~1 | Site, method="ML")
anova(purity_m2_lme1, purity_m2_lme2)


library(lme4)
# Sammilar
purity_m2_lmer <- lmer(Purity ~ Suppliers + (1|Batches2), data = data_purity)
summary(purity_m2_lmer)
anova(purity_m2_lmer)



#####################################################


paper <- read.table("Data/paper2.txt",header=TRUE,sep=",")
paper$Replicates <- factor(rep(c(1,2,3),each=12))
paper$Temp   <- factor(paper$Temp)
paper$Method <- factor(paper$Method)
summary(paper)

aov_paper <- aov(Strength ~ Temp*Method +Replicates:Temp + Error(Replicates/Method), data = paper)
summary(aov_paper)



strength.fit <- lme(fixed=Strength ~ Temp + Method + Temp:Method,
                    random=~ 1 |blocks/Method,
                    data=paper)
anova(strength.fit, type="marginal")




#####################################################
# Paper strength.
# Split-Plot design with main plots in CRD


# Paper strengh was measured in an experiment where different batches of
# pulp were selected and randomly assigned to 3 different methods of
# pulping. Each batch was then split into parts and each part was cooked at
# different temperatures. Finally the strength of the paper was measured.

options(useFancyQuotes=FALSE) # renders summary output corrects
options(error = expression(NULL))  # let the script run in batch even if error detected
options(width=200)

# which libraries are needed
library(ggplot2)
library(gridExtra)
library(lmerTest)
library(lsmeans)
library(nlme)
library(plyr)

# get the schwarz functions for summaries etc
source("schwarz.functions.r")


# Read in the data
strength <- read.csv('paper.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
# Don't forget to change method and temp to factors
strength$Method <- as.factor(strength$Method)
strength$Temp   <- as.factor(strength$Temp)
strength$Trt    <- with(strength, interaction(Method,Temp))
strength[1:5,]



# Get side by side dot plots for each treatment
plot.dotplot <- ggplot(data=strength, aes(x=Trt, y=Strength))+
  ggtitle("Dot plots to check for outliers")+
  xlab("Treatment (Method.Temperature)")+ylab("Paper strength")+
  geom_point(position=position_jitter(height=0.2, width=0.2), size=2)+
  geom_boxplot(alpha=0.1)
plot.dotplot

#ggsave(plot=plot.dotplot, file="paper_dotplot.png", height=4, width=6, units="in")



# Make a nice report showing summary statistics by treatment in using the plyr()
# package as implemented in my summary function 
# Compute some summary statistics for each group
report <- ddply(strength, c("Temp","Method"),  
                sf.simple.summary, 
                variable="Strength")
report




# check to see if the sd increases with the mean
ggplot(data=report, aes(x=log(mean),y=log(sd)))+
  ggtitle("Does the SD increase with the mean?")+
  geom_point()



# The interaction plot
# We don't compute the se or confidence intervals for each
# point because of the split-plot structure makes it difficult
# to do so from the raw data.
plot.interaction <- ggplot(data=report, aes(x=Temp, y=mean, 
                                            group=Method, color=Method, linetype=Method, shape=Method ))+
  ggtitle("Interaction plot based on raw data")+
  xlab("Temperature")+ ylab("Mean Strength")+
  geom_point(size=3)+
  geom_line()
plot.interaction

#ggsave(plot=plot.interaction, file="paper_interaction.png", height=4, width=6, units="in")





######################################################################################
# Classical analysis using aov() function
# check that all relevant variables are factors
str(strength)
strength.fit.aov <- aov( Strength ~ Method * Temp + Error(Batch), data = strength)
summary(strength.fit.aov)

# The problem with the aov() function is that it is difficult to get estimates
# of lsmeans or pairwise comparisons etc.





#########################################################################################
# Now for the analysis using lme in the nlme package

library(nlme)
# Check that all relevant variables are factors
str(strength)
strength.fit <- lme(fixed=Strength ~ Temp + Method + Temp:Method,
                    random=~ 1 | Batch,
                    data=strength)

# Get the tests for Fixed effects.
# These are conditional on the variance component estimates
# We specify the type argument to get the marginal (Type III) tests.
# Because the design is balanced, the default type="sequential" (TYpe I)
# are the same, but this is generally NOT true. Always use the Type III tests.
anova(strength.fit, type="marginal")

# Get the estimates of the variance components
summary(strength.fit)
nlme::VarCorr(strength.fit)  # Need to override lmer() VarCOrr


# Get the lsmeans for each level using the lsmeans package
library(lsmeans)

# We load the lsmeans package. It does the lsmeans for lme objects
# but the df is NOT specified (groan) and you need to specify by hand
temp.lsmo   <- lsmeans(strength.fit,  ~Temp)
method.lsmo <- lsmeans(strength.fit,  ~Method)
tm.lsmo     <- lsmeans(strength.fit,  ~Temp:Method)

summary(temp.lsmo)
summary(method.lsmo)
summary(tm.lsmo)



# get the residual plot and normal probability plot
# Because these are trellis graphs, you must create the objects and then use
# the print command to position them in the proper locations

plot1 <- qqnorm(strength.fit)
plot2 <- plot(strength.fit, main="Residual plot")
plot3 <- plot(strength.fit, Strength ~ fitted(.), abline=c(0,1),
              main="Observed vs Predicted")
print(plot1, position=c(0,.5,.5, 1), more=TRUE)
print(plot2, position=c(0, 0,.5, .5), more=TRUE)
print(plot3, position=c(.5,0, 1, .5))







##-------------------- Using lmer() function  ------------------
# The lmer() function provides a much simple way to specify 
# models with random effect. HOWEVER, the ordinary lmer() function
# does NOT produce p-values.
# See https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html
#     for details on why LMER does NOT produce p-values.
# Also see http://glmm.wikidot.com/faq for further information.

# Fortunately, the lmerTest package has recently been produced which 
# adds p-values based on a Satterthaite approximation 
#


# Check that all relevant variables are factors
str(strength)

##lmermodelb;
strength.fit.lmerTest <- lmerTest::lmer(Strength ~ Temp + Method + Temp:Method +
                                          (1 | Batch),
                                        data=strength)


##lmerFtestsb;
# Get the ANOVA table to test for fixed effects
cat("Test for fixed effects using the Satterthwaite approximate\n")
anova(strength.fit.lmerTest)  # uses Satterhwaite approximation
cat("\n\nTest for fixed effects using the Kenward-Roger approximate\n")
anova(strength.fit.lmerTest, ddf="Kenward-Roger")
cat("\n\nThe default output from lmer doesn't have p-values\n")
anova(strength.fit.lmerTest, ddf="lme4")        # the table from the original lmer() without p-values.


# Summary table
summary(strength.fit.lmerTest)


# We repeat the above for the Methods factor
# Now get the lsmeans for the Temperature effect and construct the various plots.
# Notice that the lmerTest package and the lsmeans package BOTH provide lsmeans()
# functions so you have to be careful that you get the correct function!
method.lsmo   <- lsmeans::lsmeans(strength.fit.lmerTest, ~Method)
# 
# # Find the CLD report with a tukey adjustments to get simultaneous confidence intervals and make a plot
# method.cld <- cld(method.lsmo, adjust='tukey')
# method.cld
# 
# # get a cld bar plot
# method.cld.plot1 <- sf.cld.plot.bar(method.cld, "Method")+
#   xlab("Method")+
#   ylab("Mean paper strength and 95% ci")+
#   ggtitle("Comparison of mean strength over methods with cld")
# method.cld.plot1
# 
# # get a cld line plot
# method.cld.plot2 <- sf.cld.plot.line(method.cld, "Method")+
#   xlab("Method")+
#   ylab("Mean paper strength and 95% ci")+
#   ggtitle("Comparison of mean strength over methods with cld")
# method.cld.plot2
# 
# method.pairs <- pairs(method.lsmo, adjust='tukey')
# summary(method.pairs, infer=TRUE)
# ##***part-lsmeans-methode;
# sink()
# ggsave(plot=method.cld.plot1, file='paper-R-method-cld-plot1.png', height=4, width=6, units="in")
# ggsave(plot=method.cld.plot2, file='paper-R-method-cld-plot2.png', height=4, width=6, units="in")
# 
# 
# 
# 
# # We repeat the above for the Temp:Methods interaction
# # Now get the lsmeans for the Temperature effect and construct the various plots.
# # Notice that the lmerTest package and the lsmeans package BOTH provide lsmeans()
# # functions so you have to be careful that you get the correct function!
# tm.lsmo   <- lsmeans::lsmeans(strength.fit.lmerTest, ~Temp:Method)
# 
# # Find the CLD report with a tukey adjustments to get simultaneous confidence intervals and make a plot
# tm.cld <- cld(tm.lsmo, adjust='tukey')
# tm.cld
# 
# # get a cld bar plot but because there are two factors we need to add a new variable to the cld report
# tm.cld$TM <- interaction(tm.cld$Temp, tm.cld$Method)
# tm.cld.plot1 <- sf.cld.plot.bar(tm.cld, "TM")+
#   xlab("Temp.Method")+
#   ylab("Mean paper strength and 95% ci")+
#   ggtitle("Comparison of mean strength over temp.methods with cld")
# tm.cld.plot1
# 
# # get a cld line plot
# tm.cld.plot2 <- sf.cld.plot.line(tm.cld, "TM")+
#   xlab("Temp.Method")+
#   ylab("Mean paper strength and 95% ci")+
#   ggtitle("Comparison of mean strength over temp.methods with cld")
# tm.cld.plot2
# 
# tm.pairs <- pairs(tm.lsmo, adjust='tukey')
# summary(tm.pairs, infer=TRUE)

#ggsave(plot=tm.cld.plot1, file='paper-R-tm-cld-plot1.png', height=4, width=6, units="in")
#ggsave(plot=tm.cld.plot2, file='paper-R-tm-cld-plot2.png', height=4, width=6, units="in")





# Extract the variance components
vc <- VarCorr(strength.fit.lmerTest)
vc




# Diagnostic plots. 
diag.qqplot <- ggplot(strength.fit.lmerTest, aes(sample=.resid)) + 
  stat_qq()+
  ggtitle("Q-Q plot for residuals")

diag.resplot <- ggplot(strength.fit.lmerTest, aes(x=.fitted, y=.resid))+
  ggtitle("Residual vs Predicted plot")+
  geom_point() + geom_hline(yintercept=0)

diag.qqplot.batch <- ggplot( data.frame(batch.eff=ranef(strength.fit.lmerTest)$Batch$"(Intercept)") , aes(sample=batch.eff))+
  stat_qq()+
  ggtitle("Q-Q plot for batch effect")

diag.plot <- arrangeGrob(diag.qqplot, diag.resplot, diag.qqplot.batch, nrow=2)
diag.plot
#ggsave(plot=diag.plot, file='paper-R-diagplot.png', height=4, width=6, units="in")



