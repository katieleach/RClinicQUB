## Introduction to Phylogenetics in R ##

## EXPLORING PHYLOGENETIC TREES ##

# Install packages

install.packages("ape")
install.packages("nlme")
install.packages("caper")
install.packages("arm")

library(ape)
library(nlme)
library(caper)
library(arm)

## CHANGE WD ##
 
setwd("H:/PhD/General Information/Phylo_Practical")

# Read in the phylogeny
phylogeny <- read.nexus("phylo.nexus")

# Phylogenetic tree properties
phylogeny

# Structure of the phylogenetic tree
str(phylogeny)
phylogeny$tip.label
phylogeny$edge.length

# Plot the entire phylogeny (NB. This may take a while!)
plot(phylogeny)

?plot.phylo

plot.phylo(phylogeny, type = "fan", cex = 0.2, edge.color="hotpink", tip.color="green")


# Zoom in to plot phylogenetic relationships between Lepus spp. (hares)
zoom(phylogeny, list(grep("Lepus", phylogeny$tip.label)),
     subtree = FALSE, cex=0.5)

# Root phylogeny on specific outgroup
rerootedphylo <- root(phylogeny, "Monodelphis_glirina") #Amazonian red-sided opossum
# Clear plots first
plot(rerootedphylo, cex=0.5)

# Compute branch lengths using different method - Grafen 
phylogeny_new <- compute.brlen(phylogeny, method="Grafen")
phylogeny_new$edge.length

# Drop tips from phylogeny
culledphylogeny <- drop.tip(phylogeny, "Rattus_rattus")
zoom(culledphylogeny, list(grep("Rattus", culledphylogeny$tip.label)),
     subtree = FALSE, cex=0.5)

# Compute pairwise differences using branch lengths
distances <- cophenetic.phylo(phylogeny)

# Computes branching times considering tree is ultrametric
time <- branching.times(phylogeny)
time <- cbind(phylogeny$tip.label, time)


## CONTROLLING FROR PHYLOGENY ##

# Read in data
lh.data <- as.data.frame(read.csv("data.csv"))

# Explore the structure of the data
str(lh.data)

par(mfrow=c(2,2))
hist(lh.data$maximum_lifespan_yr, col = rainbow(8), main="Maximum Lifespan", xlab=NA)
hist(lh.data$mass_g, col = rainbow(8), main="Body Mass", xlab=NA)

# In this case the data needs to be log-transformed
hist(log(lh.data$maximum_lifespan_yr), col = rainbow(8), main="Log(Maximum Lifespan)", xlab=NA)
hist(log(lh.data$mass_g), col = rainbow(8), main="Log(Body Mass)", xlab=NA)

# Linear regression
model.ols <- lm(log(maximum_lifespan_yr) ~ log(mass_g) + volancy + log(mass_g):volancy, lh.data)

# Produce a summary of the results
summary(model.ols)

# Plot the model results
par(mfrow=c(2,2))
plot(model.ols)

# Observe the relationship between maximum lifespan and body mass
par(mfrow=c(1,1))
plot(log(maximum_lifespan_yr) ~ log(mass_g), data=lh.data, col="blue", pch=16)

#Add a regression line from the model
abline(model.ols, col="red", lwd=3)

# Plot again but with the interaction of volancy
plot(log(maximum_lifespan_yr) ~ log(mass_g), data=lh.data, col="blue", pch=16)

# Colour volant species points
points(log(maximum_lifespan_yr[volancy == "volant"]) ~ log(mass_g[volancy == "volant"]),
       data = lh.data, col = "hotpink", pch = 16)

# Add non-volant regression line
abline(lm(log(maximum_lifespan_yr[volancy == "nonvolant"]) ~ log(mass_g[volancy == "nonvolant"]),
          data = lh.data), col = "blue", lwd=3)

# Add volant regression line
abline(lm(log(maximum_lifespan_yr[volancy == "volant"]) ~ log(mass_g[volancy == "volant"]),
          data = lh.data), col = "hotpink", lwd=3)

# Add legend
legend("bottomright", c("Volant", "Non-Volant"), col = c("hotpink", "blue"),
       lwd=3)

# Observe the position of certain species groups
plot(log(maximum_lifespan_yr) ~ log(mass_g), data=lh.data, col="blue", pch=16)
abline(model.ols, col="red", lwd=3)

# Look at primates
points(log(maximum_lifespan_yr[order == "Primates"]) ~
         log(mass_g[order == "Primates"]),
       data = lh.data, col = "black", pch = 16)

# Look at rodents
points(log(maximum_lifespan_yr[order == "Rodentia"]) ~
         log(mass_g[order == "Rodentia"]),
       data = lh.data, col = "purple", pch = 16)

# Add legend
legend("bottomright", c("Primates", "Rodents"), col = c("black", "purple"),
       pch=16)

## Phylogenetically-controlled regression

# Prepare the data
compdata <- comparative.data(phy=phylogeny, data=lh.data, names.col = species, 
                             na.omit = FALSE, warn.dropped = TRUE)

# Run the model (NB. This may take a while!). ML indicates that this model uses the maximum likelihood estimate of lambda. 
model.pgls <- pgls(log(maximum_lifespan_yr) ~ log(mass_g) + volancy, 
                   data = compdata, lambda = "ML")

# Produce a summary of the results
summary(model.pgls)

pgls.confint(model.pgls, "lambda")$ci.val

# Check the model outputs as for the linear regression model. In the first plot you shouldn't see any data with a studentized residual >+-3. The points of the second plot should approximately fall on the line. And the third and fourth plots should show a fairly random scattering of points. You want to avoid any clear patterns.
par(mfrow = c(2, 2))
plot(model.pgls)

# Then plot the original data again but with lines of best fit from the linear regression and phylogenetically-controlled regression
par(mfrow = c(1,1))
plot(log(maximum_lifespan_yr) ~ log(mass_g), data=lh.data, col="blue", pch=16)
abline(model.ols, col="red", lwd=3)
abline(model.pgls, col="black", lwd=3)
legend("bottomright", c("Linear", "Phylo.-controlled"), col = c("red", "black"), lwd=3)

## Full models

# Separate the dataset into volant and non-volant datasets. Linear regression models can be explored initially, as above, and then phylogentically-controlled regressions can be run on the full models for volant and  non-volant mammal species. 

volantspp <- droplevels(lh.data[lh.data$volancy=="volant", ])
nonvolantspp <- droplevels(lh.data[lh.data$volancy=="nonvolant", ])

model.ols.vol <- lm(log(maximum_lifespan_yr) ~ log(mass_g) + foraging_environment +
                   daily_activity, volantspp)
summary(model.ols.vol)

model.ols.nvol <- lm(log(maximum_lifespan_yr) ~ log(mass_g) + fossoriality + foraging_environment +
                      daily_activity, nonvolantspp)
summary(model.ols.nvol)

compdata.vol <- comparative.data(phy=phylogeny, data=volantspp, names.col = species, 
                                 vcv = TRUE, na.omit = F, warn.dropped = TRUE)
compdata.nvol <- comparative.data(phy=phylogeny, data=nonvolantspp, names.col = species, 
                                  vcv = TRUE, na.omit = F, warn.dropped = TRUE)

model.pgls.vol <- pgls(log(maximum_lifespan_yr) ~ log(mass_g) + foraging_environment +
                         daily_activity - 1, data = compdata.vol, lambda = "ML")
summary(model.pgls.vol)
model.pgls.nvol <- pgls(log(maximum_lifespan_yr) ~ log(mass_g) + foraging_environment + relevel(fossoriality, ref="semifossorial") + daily_activity - 1, data = compdata.nvol, lambda = "ML")
summary(model.pgls.nvol)

par(mfrow=c(1,2))
coefplot(as.vector(c(model.pgls.vol$model$coef[2:7])), as.vector(c(model.pgls.vol$sterr[2:7])), 
         varnames=c("Body Mass", "Aquatic", "Arboreal", "In Air", "Terrestrial", "Crepuscular", "Nocturnal"), cex.var=0.7, main="Volant", xlim=c(-1,2.5))

coefplot(as.vector(c(model.pgls.nvol$model$coef[2:10])), as.vector(c(model.pgls.nvol$sterr[2:10])), varnames=c("Body Mass", "Aquatic", "Arboreal", "Semi-Aboreal", "Terrestrial", "Fossorial", "Non-Fossorial", "Crepuscular", "Diurnal", "Nocturnal"), cex.var=0.7, main="Non-Volant", xlim=c(-1,2.5))
