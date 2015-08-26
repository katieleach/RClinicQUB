#################################################
###Model selection and model averaging###
#################################################

### linear models refresher

data(iris)

#linear model 
#look at petal length as a function of other measurements

mod1 <- lm(Petal.Length ~ Petal.Width + Sepal.Length +Sepal.Width, data=iris)
summary(mod1)

mod2 <- lm(Petal.Length ~ Petal.Width + Sepal.Length, data=iris)
summary(mod2)

mod3 <- lm(Petal.Length ~ Petal.Width + Sepal.Length, data=iris)

#compare models
anova(mod1, mod2) #difference between model performance is significantly different 
###                       so stick with more complex model


#### working with more complex datsets with many explanatory variables
data1 <- read.csv("beedata.csv")
str(data1)

mod1<- lm(bees ~  wind + temp + hotness + car + flowertype, data = data1)

summary(mod1)


######### variable selection 



### first remove variables that have no logical reason to effect the response: here we remove "car" 
### which is the type of car the researcher was driving.

mod2<- lm(bees ~  wind + temp + hotness + flowertype, data = data1)

anova(mod1,mod2)

### note: that even though removing "car" doesn't significantly improve the model it should still be removed because it's nonsensical

########################################

############### next look for correlations between predictors


str(data1)
cor(data1[,2:4])

#          bees        temp         wind
# bees  1.00000000 0.033165617 -0.548860487
# temp  0.03316562 1.000000000  0.008838102
# wind -0.54886049 0.008838102  1.000000000

cor.test(data1$temp, data1$wind)

# Pearson's product-moment correlation

# data:  data1$temp and data1$wind
# t = 0.1526, df = 298, p-value = 0.8788
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.1045073  0.1219569
# sample estimates:
#         cor 
# 0.008838102 

#####
plot(data1)

### something strange with temp and hotness plot these separately... 

plot(data1$temp ~data1$hotness)

#### These are correlated - i.e. overcast is always below 15, hot sunney is always over 20. 
#### Choose the better explanatory variable, in this case temperature, as it is measured with 
#### much greater accuracy

mod3<- lm(bees ~  wind + temp + flowertype, data = data1)


summary(mod1)

summary(mod3)



#### the all subsets approach

moda<- lm(bees ~  wind + temp + flowertype, data = data1)
AIC(moda)
# [1] 1521.519
summary(moda)



modb<- lm(bees ~  wind + temp, data = data1)
AIC(modb)
#[1] 1521.276

modc<- lm(bees ~  wind, data = data1)
AIC(modc)
#[1] 1519.897
plot(modc)

hist(residuals(modc))
shapiro.test(residuals(modc))

modd<- lm(bees ~  temp + flowertype, data = data1)

AIC(modd)
#[1] 1621.6

mode<- lm(bees ~  temp, data = data1)
AIC(mode)

modf <- lm(bees ~ flowertype, data = data1)
AIC(modf)
## [1] 1619.988
AIC(moda,modb,modc,modd, mode,modf)

################### 
# automatic all subsets approach is available in MuMIn, function = "dredge"
library("MuMIn")

######### set "na" options to na.fail

global_mod<- lm(bees ~  wind + temp + flowertype, data = data1, na.action = na.fail)

d1 <- dredge(global_mod)

print(d1)  ### here the best model contains only wind therefore with this approach we would say
           ### "the most parsimonious model by step-wise selection contained only "wind" and report the
           ### following summary table

modc<- lm(bees ~  wind, data = data1)
plot(modc)

summary(modc)
# Call:
#   lm(formula = bees ~ wind, data = data1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.6545 -1.7470  0.1605  2.2530 11.2530 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  21.2846     0.8528   24.96   <2e-16 ***
#   wind         -1.9075     0.1683  -11.34   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.027 on 298 degrees of freedom
# Multiple R-squared:  0.3012,  Adjusted R-squared:  0.2989 
# F-statistic: 128.5 on 1 and 298 DF,  p-value: < 2.2e-16

### An alternative approach is model averaging, here we consider multiple top models and average the
### p-values and parameter estimates across those models. 
### As a general rule of thumb we say that models within two AIC(c) of the top model are potentially 
### the top model here and therefore these are the ones which are usually averaged accross, this is 
### adjusted with the argument subset.. in the function model.avg (see below..)


data2 <- stdize(data1[,3:4])

str(data2)

data1 <- cbind(data1, data2)


###############
global_mod_st<- lm(bees ~  z.wind + z.temp + flowertype, data = data1, na.action = na.fail)
d2 <-dredge(global_mod_st)


mod_avg <- model.avg(d2, subset = delta <2, fit = T)
summary(mod_avg)


#                                           2))
# Call:
#   model.avg.default(object = get.models(object = d1, subset = delta < 
#                                           2))
# 
# Component model call: 
#   lm(formula = bees ~ <3 unique rhs>, data = data1, na.action = na.fail)
# 
# Component models: 
#   df  logLik    AICc delta weight
# 3   3 -756.95 1519.98  0.00   0.45
# 13  7 -753.09 1520.57  0.59   0.33
# 23  4 -756.64 1521.41  1.43   0.22
# 
# Term codes: 
#   flowertype       temp       wind 
# 1          2          3 
# 
# Model-averaged coefficients: 
#   Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)       20.92035    1.04886     1.05272  19.873   <2e-16 ***
#   wind              -1.88637    0.17132     0.17200  10.967   <2e-16 ***
#   flowertypebramble  0.36406    0.54930     0.55157   0.660   0.5092    
# flowertypepear    -0.03756    0.54975     0.55203   0.068   0.9458    
# flowertypeplum     1.16629    0.55190     0.55418   2.105   0.0353 *  
#   flowertyperose    -0.17450    0.54936     0.55163   0.316   0.7517    
# temp               0.04605    0.05870     0.05894   0.781   0.4346    
# 
# Full model-averaged coefficients (with shrinkage): 
#                     Estimate Std. Error Adjusted SE z value Pr(>|z|)    
# (Intercept)       20.92035    1.04886     1.05272  19.873   <2e-16 ***
#   wind              -1.88637    0.17132     0.17200  10.967   <2e-16 ***
#   flowertypebramble  0.12133    0.36057     0.36173   0.335    0.737    
# flowertypepear    -0.01252    0.31787     0.31918   0.039    0.969    
# flowertypeplum     0.38871    0.63543     0.63609   0.611    0.541    
# flowertyperose    -0.05816    0.32764     0.32891   0.177    0.860    
# temp               0.01008    0.03341     0.03350   0.301    0.764    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Relative variable importance: 
#   wind flowertype temp
# Importance:          1.00 0.33       0.22
# N containing models:    3    1          1

### HERE ALWAYS REPORT THE SECOND SET of P-VALUES i.e. (with shrinkage). This means that values 
### for each variable are averaged across all the model (with an estimate of 0 and P = 1, where 
### the variable wasn't in a model)
###
