## t tests
## One sample t-test ##

# E.g A farmer has 10 hedgerows
# There has been a mean of 2 birds recorded per hedge. 
# The landowner sows wild bird cover to encourage bird populations
# The number of birds are resurveyed
# Is the new count of birds per hedge different?

birds <- c(4, 2, 5, 4, 6, 3, 1, 4, 3, 5)

t.test(birds, alternative = "greater", mu=2)


## Two sample t-test ##

# Two categories of people - breakfast eaters, and non-breakfast-eaters
# Number of biscuits eaten at coffee morning counted
# Is there a different in biscuit consumption between those who did or didn't eat breakfast?

breaky <- c(1,2,1,1,2,1,3,2,2,1)
no_breaky <- c(1,2,4,5,4,8,3,5,7,9)

t.test(breaky, no_breaky)

## ANOVA ##

# E.g. One way Analysis of Variance using iris data
data(iris)

one.way <- aov(iris$Petal.Length ~ iris$Species)
summary(one.way)

# Model checking

par(mfrow=c(2,2)) # change how many plots shown in window
plot(one.way) #produces diagnostic plots

## Linear regression ##

model1 <- lm(iris$Petal.Length ~ iris$Sepal.Length)
summary(model1)

par(mfrow=c(1,1)) #change back to one plot in the window
plot(iris$Petal.Length ~ iris$Sepal.Length)
abline(model1, col='red') #plots lm line, slope and intercept can be found in summary

#model diagnostics
par(mfrow=c(2,2))
plot(model1)


## Remove everything from environment ##

rm(list=ls())
