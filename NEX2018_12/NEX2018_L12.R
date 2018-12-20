# 01NEX - R codes used in Lectures 12 - Longitudinal Data Analysis
# Written by J. Franc - jiri.franc@fjfi.cvut.cz


setwd("D:/Vyuka/NEX/2017/12_Longitudinal_Data_Analysis/")


#### Orthodont data analysis: dental study:   Pothoff and Roy (1964)

#library( spida )   # see notes on installation 
library( nlme )   
library( lattice )  
data ( Orthodont ) 

head(Orthodont) 
summary(Orthodont) 
attach(Orthodont)
table(Sex)





xyplot(distance ~ age | Subject, data=Orthodont, as.table=T)

xyplot(distance~age | Subject,data=Orthodont,
       prepanel = function(x, y) prepanel.loess(x, y, family="gaussian"),
       xlab = "Age", ylab = "distance",
       panel = function(x, y) {
           panel.xyplot(x, y)
           panel.loess(x,y, family="gaussian") },
        as.table=T)

xyplot(distance~age | Subject,data=Orthodont,
       panel = function(x, y){
           panel.xyplot(x, y)
           panel.lmline(x, y)
       }, as.table=T)

xyplot(distance ~ age  | Sex, data = Orthodont, groups = Subject,
       type = "o", panel = panel.superpose)


xyplot(distance ~ age  | Sex, data = Orthodont, groups = Subject,
       type = "o", panel = function(x, y){
           panel.xyplot(x, y)
           panel.lmline(x, y)
       })


boxplot(distance[Sex == "Male"] ~ age[Sex == "Male"], xlab = "Age", ylab = "Distance", main ="Boxplot for Male" )
boxplot(distance[Sex == "Female"] ~ age[Sex == "Female"], xlab = "Age", ylab = "Distance", main ="Boxplot for Female" )


interaction.plot(age[Sex == "Male"], Subject[Sex == "Male"], distance[Sex == "Male"]) 




###############################  Pooling
# wrong way


model_pool1 <- lm ( distance ~ age * Sex , Orthodont) 
summary(fit) 

summary(aov( distance ~ age * Sex , Orthodont))


fit2 <- lm( distance ~ age + age:Sex ,Orthodont ) 
summary( fit2 ) 


fit3 <- lm( distance ~ age + Sex ,Orthodont ) 
summary( fit3 ) 


summary(lm ( distance ~ age + Sex , Orthodont))
summary(aov( distance ~ age + Sex , Orthodont))




summary(lm( distance ~ age *Subject , Orthodont))

##Ftest date by date
fValues <- rep(0, 4)
names(fValues) <- levels(as.factor(age))
for (i in 1:4){
    fValues[as.integer(i)] <- anova(lm(distance[age == unique(age)[i]] ~ Sex[age == unique(age)[i]], data = Orthodont))[1, 4]
    }
fValues
qf(0.95,1,25)

#### Correlation structure


fit <- lme( distance ~ age * Sex, Orthodont, 
            random = ~ 1 + age | Subject, 
            correlation  = corAR1 ( form = ~ 1 | Subject)) 
fit
summary(fit)

m_gls<-gls(distance ~ age + Sex,
        correlation=corCompSymm(form=~1|Subject),data=Orthodont)
m_gls
summary(m_gls)



m_lme<-lme(distance ~ age * Sex,
        random=~1+age|Subject,
        correlation=corAR1(form=~1|Subject),
        data=Orthodont)
m_lme
summary(m_lme)
plot(Variogram(m_lme, form = ~1|Subject, data = Orthodont))



m_lme2<-lme(distance ~ age * Sex,
           random=~1+age|Subject,
           correlation=corExp(form=~1|Subject),
           data=Orthodont)
m_lme2
summary(m_lme2)

plot(Variogram(m_lme2, form = ~1|Subject, data = Orthodont))



    
intervals(m_lme)


VarCorr( m_lme )
getVarCov( m_lme )

# better for longer time series

