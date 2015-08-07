#################################################
###Linear modelling refresher and mixed models###
#################################################

data(iris)

#linear model 
#look at petal length as a function of other measurements

mod1 <- lm(Petal.Length ~ Petal.Width + Sepal.Length +Sepal.Width, data=iris)
summary(mod1)
plot(mod1)

mod2 <- lm(Petal.Length ~ Petal.Width + Sepal.Length, data=iris)
summary(mod2)

#compare models
anova(mod1, mod2) #difference between model performance is significantly different so stick with more complex model

plot(Petal.Length ~ Sepal.Length, data=iris, col=Species, pch=16)

##Mixed effects models##
#install.packages('lme4')
library(lme4)

#fit model using maximum likelihood
iris.mix <- lmer(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width + (1|Species), REML=FALSE, data=iris)
summary(iris.mix)

#residual plots
plot(residuals(iris.mix))
qqnorm(residuals(iris.mix))



####Nested or crossed random effects####
#Nested
data(oats, package='MASS') #load oats data from the package MASS
str(oats)

#change column names for ease of understanding
colnames(oats) <- c('Batch', 'Variety', 'Nitrogen', 'Yield')

#use head() and tail() can see there are multiple varieties within each batch therefore variety is NESTED in batch
#we want to see how nitrogen affects yield N.B. this is a mixed model but not regression

mod.oats1 <- lmer(Yield ~ Nitrogen + (1|Batch/Variety), REML=FALSE, data=oats)
summary(mod.oats1)


##Crossed random effects (a prioiri knowledge suggests this is wrong)
mod.oats2 <- lmer(Yield ~ Nitrogen + (1|Batch) + (1|Variety), REML=FALSE, data=oats)
summary(mod.oats2)

###Test random effects structure using reduced maximum likelihood
oats.nest <- lmer(Yield ~ 1 + (1|Batch/Variety), data=oats)
summary(oats.nest)
summary(oats.nest)$AIC #REML criterion = 582.2

oats.cross <- lmer(Yield ~ 1 + (1|Batch) + (1|Variety), data=oats) 
summary(oats.cross) #REML criterion = 661.9

#nested structure has lower AIC so use this RE structure


rm(list=ls())

#############################
###Mixed models using nlme###
#############################
install.packages('nlme')
library(nlme)

#iris mixed model
data(iris)

iris.lme <- lme(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width, random = ~ 1|Species, data=iris)
summary(iris.lme) #look, there's p values!

#Nested
data(oats, package='MASS')
colnames(oats) <- c('Batch', 'Variety', 'Nitrogen', 'Yield')

oats.lme1 <- lme(Yield ~ Nitrogen, random = ~ 1|Batch/Variety, data=oats)
summary(oats.lme1) #note nlme reports SD instead of variances

#Crossed
#It's awkward to fit crossed random effects in lme, I just wouldn't do it
