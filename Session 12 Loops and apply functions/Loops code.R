###############################################
#####for loops, apply family and aggregate#####
###############################################

##### for loops

for(counter in 1:10){
  print('Hello world!')
}


#the vector after in doesn't have to start at 1 or even have to contain numbers!

for(species in c('Heliodoxa rubinoides', 
                 'Boissonneaua jardini', 
                 'Sula nebouxii'))
{
  print(paste('The species is', species))
}

#use loops to fill in an empty matrix

my.vec <- c(3, 5, 6, 3, 7, 4, 7, 2, 8, 9)
times.vec <- c(2, 4, 6, 8, 10)

mat <- matrix(nrow = 10, ncol=5)

for (i in 1:5){
  res <- my.vec*times.vec[i]
  mat[,i] <- res
}

####################################################################################

#################################
####Apply family of functions####
#################################

## Appy functions can often do the same job as loops, but using simpler syntax

## generate some data
mat <- matrix(data=cbind(rnorm(30, 0), rnorm(30, 2), rnorm(30, 5)), nrow=30, ncol=3)

# apply() arguments:
# 1 = data
# 2 = margin (row or column)
# 3 = function

## R rule of thumb - rows and then columns. 
# We use 1 as the second argument in apply to specify rows, and 2 for columns

##calculate mean of columns (so we use 2)

m.col <- apply(mat, 2, mean)

m.col

## write own function for apply where we determine how many -ve values in each column

apply(mat, 2, function(x) length(x[x<0]))


## compare outputs of sapply and lapply

# lapply() takes a list and outputs a list
lapply(1:3, function(x) x^3)

# sapply() outputs data in the simplest form. 
sapply(1:3, function(x) x^3)


##########################
###e.g. using aggregate###
##########################

data(mtcars)
head(mtcars)


## To use aggregate() data must be a list
# We can coerce data into a list using the function list()

## calculate the mean of car attributes for each combination gears and cylinders

## Here we use three arguaments
# 1) data
# 2) by() is list of variables that we cross to form new observations
# 3) FUN - the function we are using

agg.data <- aggregate(mtcars, by=list(mtcars$cyl, mtcars$gear), FUN=mean) 

agg.data

# If want names on group columns then name objects in list

agg.data <- aggregate(mtcars, by=list(Cylinders=mtcars$cyl, Gears=mtcars$gear), FUN=mean) 

agg.data