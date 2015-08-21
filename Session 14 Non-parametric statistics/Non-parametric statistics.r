## Non Parametric statistics Snippets session
## 19 Aug 2015
rm(list=ls())

setwd("C:\\Users\\40115267\\Desktop")

pig <- read.csv("pig vocalisation.csv")
str(pig)

###########  Spearman Corrrelation ##################

## is grunt rate in isolation test correlated with grunt rate in novel object test?

## quick check if grunt rate is normally distributed using histograms
hist(pig$si.grunt)
hist(pig$no.grunt)

## run the spearman correlation
cor.test(pig$si.grunt, pig$no.grunt, method = "spearman")

## look at relationship between the two variables using scatterplot
plot(pig$si.grunt ~ pig$no.grunt)

########  Wilcoxon Rank Sum test  #####################

## is there a difference in si.grunt between males and females

# histogram to look at the distribution of "si.grunt"
hist(pig$si.grunt)

# boxplot to look at median si.grunt in males and females
boxplot(pig$si.grunt ~ pig$sex)

## Null hypothesis = no difference in median si.grunt between males and females

wilcox.test(pig$si.grunt ~ pig$sex, mu=0, alt="two.sided", conf.int=T, conf.level=0.95,paired=F, exact=T, correct=T)

####### Wilcoxon Signed Rank test   ##############################

## use immer data in MASS package
library(MASS)
str(immer)

boxplot(immer$Y1, immer$Y2)

## code for wilcoxon signed ranks test
wilcox.test(immer$Y1, immer$Y2, paired=TRUE)

#######  Kruskall Wallace test  ##################################

## use mpg data: want to know if there's a difference in mpg 
## on the highway between the 3 different drive types (front, rear and 4 wheel)
library(ggplot2)
data(mpg)
str(mpg)

## boxplot
boxplot (mpg$hwy ~ mpg$drv)

## run the test
kruskal.test(hwy ~ drv, data=mpg)

## post-hoc
pairwise.wilcox.test(mpg$hwy, mpg$drv)





