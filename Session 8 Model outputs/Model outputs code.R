###################################
###R Clinic Model Output Session###
###################################

##Output of lm

x1 <- c(2,3,3,3,4,7)
y1 <- c(2,3,2,1,2,6)

#let's plot the data to have a look at it
plot(x1, y1, xlim=c(0,8), ylim=c(0,8)) #plot the data with axis limits of 0-8

#linear regression of y against x

mod <- lm(y1 ~ x1)
summary(mod)

#let's plot model line on graph
abline(mod, col='red')

#how influential is outlier
influence.measures(mod)

####Analysis of variance
data(InsectSprays)
str(InsectSprays)

aov.ins <- aov(count ~ spray, data=InsectSprays)
summary(aov.ins)
summary.lm(aov.ins)

#Post hoc test for comparisons - Tukey's Honestly Significant Difference
TukeyHSD(aov.ins)

#Let's look at a plot
boxplot(count ~ spray, data=InsectSprays, xlab='Spray', 
        ylab='Number of Insects', main='Insect Spray Experiment', col='darkgoldenrod2')
