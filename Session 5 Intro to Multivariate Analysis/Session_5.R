## An intro to multivariate statistics ###

# Install and load the package 'vegan' ###

install.packages("vegan")
library("vegan")


##############################################
## Unconstrained ordination example - PCA ####
##############################################

# load iris data set
data(iris)

# examine structure using str function
str(iris)

# The first four variables are numeric continuous data, so lets use these for the PCA. 
ir.con <- iris[ , 1:4]

# The data may not be measured on the same scale, so it is often a good idea** to scale your data to make variables comparible 
# **NB there are many different opinions on data transformation, rather than one prescriptive direction 

pca.iris <- rda(ir.con, scale = TRUE)

# Take a look at the result 

summary(pca.iris)

## basic plotting (ordination plot = biplot)
biplot(pca.iris, scaling = 3)
text(pca.iris, display = "species")


####################################################################
#### Constrained ordination example - redundancy analysis (RDA) ####
####################################################################

# load data sets (must have package 'vegan loaded')
data(dune)
data(dune.env)

# take a look at the data
str(dune)
str(dune.env)

## We want to look a the structure of the response matrix 'dune', taking into account the influence of the explanitory matrix (dune.env)

## Firstly we can do an RDA using just one explanitory variable from the 'dune.env' matrix

rda.dune.man <- rda(dune ~ Management, dune.env)

summary(rda.dune.man)

plot(rda.dune.man)

## You may also want to use the entire environmental matrix
## We use '.' to signify the whole environmental matrix when we aren't specifying variables

rda.dune <- rda(dune ~ ., dune.env)

summary(rda.dune)

plot(rda.dune)


###########################################################
### Clustering example - hierarchical cluster analysis ####
###########################################################

# load data
data(mtcars)

## firstly compute the distance matrix - here we are using 'euclidean' distance again
d <- dist(mtcars, method = "euclidean")

## carry out clustering
hc <- hclust(d) 

## Plot your dendrogram
plot(hc)

## You can select clusters to draw a border around using the 'k' arguament
rect.hclust(hc, k=5)
