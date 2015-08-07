
#install.packages('datasets')

###Load library and iris dataset###
library(datasets) 
data(iris)

###Look at structure of data###

head(iris) #gives top 6 rows of dataset
tail(iris) #gives last 3 rows of dataset

dim(iris) #gives dimensions 1st number = rows, 2nd number = columns

str(iris) #look at structure of dataset


###Specifying a column of data

iris$Sepal.Length #gives values of Sepal.Length column
iris[ ,1]

###Extract single value from data###
iris[2,1] #gives value of 2nd row, 1st column

###Extract subset of data
iris[1:5, 3:4] #gives rows 1-5 of columns 3 and 4


##2 ways of looking at different factor levels of Species
levels(iris$Species) 
levels(iris[ ,5])


###Basic summary statistics###
#e.g.
max(iris$Sepal.Length)
min(iris$Sepal.Length)

###Questions: What is the range of petal widths?###
range(iris$Petal.Width)
2.5 - 1.5


###Graphics###
#histogram
hist(iris$Sepal.Length)

#density plot
plot(density(iris$Sepal.Length))

#pairs plot
pairs(iris)

#scatter plot
plot(iris$Sepal.Length, iris$Sepal.Width)

###Questions: Make a scatterplot for how petal width varies with petal length?
plot(iris$Petal.Length, iris$Petal.Width)

#boxplot
plot(iris$Sepal.Length ~ iris$Species)

#tree
install.packages('tree')
library(tree)
regtree <- tree(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length + Species, data=iris)
plot(regtree)
text(regtree)
