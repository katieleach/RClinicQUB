## Example 1 - Zebra dataset ##

install.packages("rgdal")
library(rgdal)
# Read in data
zebras = readOGR("C:/Users/Katie/Desktop", "zebra") ##Change directory
summary(zebras)
table(zebras$individual) 

# Transform coordinate system
zebras = spTransform(zebras, CRS("+init=epsg:3857"))
install.packages("dismo")
library(dismo)
# Plot on Google Maps
bg = gmap(extent(zebras))
plot(bg)
plot(zebras, pch = 19, col = "#00000020", add=TRUE)

# Plot individuals in different colours
palette(rainbow(9))
plot(bg)
plot(zebras, col = zebras$individual, add = TRUE)
legend("topleft", fill=rainbow(9), 
       legend = levels(zebras$individual), 
       bg = "white", inset = 0.25)

# Plot species in different colours
palette(rainbow(2))
plot(bg)
plot(zebras, col = zebras$species, add = TRUE)
legend("bottom", fill=rainbow(2), 
       legend = levels(zebras$species), 
       bg = "white", inset = 0.05)

# Plot convex hull for individual 1
install.packages("rgeos")
library(rgeos)
splitz = split(zebras, zebras$individual)
chull1 = gConvexHull(splitz[[1]])
plot(bg)
plot(zebras, col = "black", cex = 0.5, 
     add = TRUE)
plot(chull1, border = "red", lwd = 4, 
     add = TRUE)

# Calculate area and % intersect between individual 1 and 5
area1 = gArea(chull1)
chull2 = gConvexHull(splitz[[5]]) #Individual 5
area2 = gArea(chull2)
overlap = gIntersection(chull1, chull2)
percent = round(100 * gArea(overlap)/area1)
percent

# Plot convex hull and intersect
plot(bg)
plot(chull1, add = TRUE, col = "#00FF0080")
plot(chull2, add = TRUE, col = "#FF00FF80")
plot(overlap, add=TRUE, col="black")
legend("topleft", fill=c("#00FF0080", 
                         "#FF00FF80",
                         "black"), 
       legend = c("Individual 1", 
                  "Individual 5", 
                  "Overlap"), 
       bg = "white", inset = 0.25)


## Example 2 - Wheat dataset ##
install.packages("nlme")
library(nlme)

# Read in data and plot
data(Wheat2)
names(Wheat2)
plot(latitude~longitude, col=as.numeric(Block), data=Wheat2)

## OPTION 1 to incorporate spatial autocorrelation in data ##
# GLS between yield, variety and block
model1 <- gls(yield~variety+Block, data=Wheat2, method='ML')
summary(model1)
# Save residuals
resid <- data.frame(Wheat2$latitude, Wheat2$longitude, 
                    residuals(model1, type='n'))
install.packages("geoR")
library(geoR) 
geodat <- as.geodata(resid)
# Plot variogram
par(mfrow=c(1,2))
geodat.v1 <- plot(variog(geodat, uvec=1:50, max.dist=30, 
                         option='bin'))

# Update GLS models with different correlation structures
out1a <- update(model1, corr=corSpher(c(20,.4), 
                         form=~latitude+longitude, nugget=T))
out1b <- update(model1, corr=corExp(c(20,.4), 
                         form=~latitude+longitude, nugget=T))
out1c <- update(model1, corr=corGaus(c(20,.4), 
                         form=~latitude+longitude, nugget=T))
out1d <- update(model1, corr=corRatio(c(20,.4), 
                         form=~latitude+longitude, nugget=T))

# Assess AIC values
AIC(model1, out1a, out1b, out1c, out1d)

# Save residuals for best model and plot variogram
resid2 <- data.frame(Wheat2$latitude, Wheat2$longitude, 
                     residuals(out1d, type='n'))
geodat2 <- as.geodata(resid2)
geodat.v2 <- plot(variog(geodat2, uvec=1:50, max.dist=30, 
                         option='bin'))

## OPTION 2 to incorporate spatial autocorrelation in data ##
install.packages("spdep")
library(spdep)

# Create spatial neighbourhood object (distance matrix)
nb = knn2nb(knearneigh(cbind(Wheat2$latitude, Wheat2$longitude)))
# Test for spatial autocorrelation
moran.test(Wheat2$yield, listw = nb2listw(nb))
# Plot
par(mfrow=c(1,1))
moran.plot(Wheat2$yield, listw = nb2listw(nb, style = "C"))

# GLM without spatial component
mod1 <- glm(yield ~ variety + Block, data = Wheat2)
summary(mod1)

# With spatial component
mod2 <- spautolm(yield ~ variety + Block, data = Wheat2, listw = nb2listw(nb), 
               family = "SAR")
summary(mod2)

AIC(mod1, mod2)
