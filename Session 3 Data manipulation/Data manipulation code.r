##SESSION 3 - DATA MANIPULATION##

# Load and explore iris dataset
data(iris)
str(iris)

# Extract specific levels of a factor
setosa.species <- iris[iris$Species == "setosa", ]
setosa.species

# Subset and drop all other columns
sepal.length <- iris[ , "Sepal.Length", drop=FALSE]
sepal.length

# Subset using the specific function
subset(iris, Sepal.Width < 3)

# Subset based on two conditions - AND
subset(iris, Species=="virginica" & Sepal.Width < 3 )

# Subset based on either one of two conditions - OR
subset(iris, Species=="virginica" | Sepal.Width < 3 )

# Row / Column mean and sums
colMeans(iris[,1:4])
colSums(iris[,1:4])
rowMeans(iris[,1:4])
rowSums(iris[,1:4])

# Add a new row
new.record <- c(4.9, 2.5, 1.7, 1.1, "setosa")
iris.new <- rbind(iris, new.record)
iris.new

# Add a new column
sepal.length.new <- sepal.length*3
iris.new <- cbind(iris, sepal.length.new)
iris.new

# Column names
colnames(iris.new)
colnames(iris.new) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species",
                        "Sepal.Length*3")
str(iris.new)

#Stack data
stack(iris)

#Round data
round(iris[,1:4], 0)

# Sort data
sort(iris$Petal.Length)
sort(iris$Petal.Length, decreasing=TRUE)  

# Order data
order(iris$Petal.Length)                  
iris[order(iris$Petal.Length), ]   
iris[order(iris$Species), ]   

# Merge dataframes
A <- data.frame(letter=LETTERS[1:5], a=1:5)
A
B <- data.frame(letter=LETTERS[sample(10)], x=runif(10))
B
merge(A, B)   
merge(A, B, all=TRUE)
na.omit(merge(A,B, all=TRUE))

# Match values - returns a vector of positions
match(c("B", "E"), LETTERS)
match(c("B", "3", "E"), LETTERS)  

## END ##
