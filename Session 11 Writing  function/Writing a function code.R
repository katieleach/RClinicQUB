########################
###Writing a function###
########################

#This code will go through a few examples of varying complexity
#It will also include a few you might want to try yourself

#################
###E.g. 1 MEAN###
#################

my.vec <- c(3,3,4,6,5,2,3,5,7,4,9,2)

mean.func <- function(x) {
  sum.x <- sum(x) #can use inbuilt functions e.g. sum and length within user written function
  length.x <- length(x)
  m <- (sum.x/length.x)
  return(m)
}

mean.func(my.vec) #4.41
mean(my.vec) # yep this matches our function

###but we can write this function a lot more neatly

mean.func2 <- function(x) {
  sum(x)/length(x)
}

mean.func2(my.vec) # yep this matches it too

#############################################################
##e.g. 2 Raise to the power of and put result in a sentence##
#############################################################

pow <- function(x, y) {
  res <- x^y #do calculation
  return(paste(x, 'raised to the power', y, 'is', res, sep=' '))
}

pow(3,4)
pow(5,3)
pow(x=5, y=3)
pow(y=3, x=5) #order doesn't matter if arguments are names

#what if we wanted y to have a default of 2
pow2 <- function(x, y=2){
  res <- x^y
  return(paste(x, 'raised to the power', y, 'is', res, sep=' '))
}

pow2(5)
pow2(5,3)


###################################
##e.g. species-area relationships##
###################################

grassland <- read.csv('c:/Users/Hannah/Documents/R clinic/Writing a function/grassland.csv', header=TRUE)

sar.func <- function(x, site){
  
  x <- x[x$Site==site, ] #subset data so that can calculate SAR for each site
  
  x$rich <- rowSums(x[, 3:dim(x)[2]]) #create column of species richness
  
  if(any(x$rich == 0)) { warning('Sites with no species have been removed')}
  x <- x[x$rich != 0, ] #remove sites with no species
  
  
  power.lm <- lm(log(rich) ~ log(x[,2]), data=x)
  power.nls <- nls(rich ~ c * (x[,2])^z, data=x, start=list('c'=exp(coef(power.lm)[1]), 'z'=coef(power.lm)[2]))
  power.AIC <- AIC(power.nls)
  
  expo.lm <- lm(rich ~ log(x[,2]), data=x)
  expo.nls <- nls(rich ~ log(c * (x[,2])^z), data=x, start=list('c'=log(coef(expo.lm)[1]), 'z'=coef(power.lm)[2]))
  expo.AIC <- AIC(expo.nls)
  
  #out <- list('power z' = coef(power.nls)[2], 'power AIC' = power.AIC, 
  #           'exponential z' = coef(expo.nls)[2], 'exponential AIC' = expo.AIC)
  
  out <- list('power z' = summary(power.nls)[[10]][2], 'power AIC' = power.AIC, 
              'exponential z' = summary(expo.nls)[[10]][2], 'exponential AIC' = expo.AIC)
  
  return(out)
}



#######CHALLENGE#######

#Write a function called percent.func that takes a vector of decimals and converts
#them to percentages rounded to 1 decimal place

vec1 <- c(0.6374, 0.7334, 0.821, 0.555, 0.6432)

percent.func(){
  
}

