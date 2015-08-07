## Session 6 - Beginner's Data Visualisation in R ##

# Scatter plots
install.packages("gcookbook")
library(gcookbook)
data(heightweight)
str(heightweight)

plot(heightweight$heightIn, heightweight$weightLb)

plot(heightweight$heightIn, heightweight$weightLb, main="School children")

plot(heightweight$heightIn, heightweight$weightLb, main="School children", 
     xlab="Height (Inches)", ylab="Weight (Lb)")

plot(heightweight$heightIn, heightweight$weightLb, main="School children", 
     xlab="Height (Inches)", ylab="Weight (Lb)", pch=17)

plot(heightweight$heightIn, heightweight$weightLb, main="School children", 
     xlab="Height (Inches)", ylab="Weight (Lb)", pch=17, col="blue")

plot(heightweight$heightIn, heightweight$weightLb, main="School children", 
     xlab="Height (Inches)", ylab="Weight (Lb)", pch=17, col="blue", cex=2, 
     cex.axis=2, cex.lab=2, cex.main=2, font.main=2)

mod1 <- lm(heightweight$weightLb~heightweight$heightIn)
summary.lm(mod1)
abline(mod1, lwd=5)
text(70, 70, paste("Slope=", round(mod1$coefficients[2],2)), cex=2)

plot(heightweight$heightIn, heightweight$weightLb, main="School children", 
     xlab="Height (Inches)", ylab="Weight (Lb)", pch=17, 
     col=c("red","blue")[unclass(heightweight$sex)])

legend("bottomright", col=c("red", "blue"), legend=c("Female", "Male"), pch=17)

# Bar plots
par(mfrow=c(1,2))
counts <- table(heightweight$sex, heightweight$heightIn)
barplot(counts, main="School Children by Height and Sex",
        xlab="Height (Inches)", col=c("red","blue"),
        legend = c("Female", "Male"), beside=TRUE)

counts2 <- table(heightweight$sex, heightweight$weightLb)
barplot(counts2, main="School Children by Weight and Sex",
        xlab="Weight (Lb)", col=c("red","blue"),
        legend = c("Female", "Male"), beside=TRUE)

# Histograms

par(mfrow=c(1,1))
hist(heightweight$ageYear, xlab="Age (Years)", main="School children")

# Density plots

plot(density(heightweight$ageYear), xlab="Age (Years)", main="School children")
install.packages("sm")
library(sm)
sm.density.compare(heightweight$ageYear, heightweight$sex, xlab="Age (Years)", 
                   col=c("red", "blue"), lty=1)
title(main="School children")
legend(locator(1), legend=c("Female", "Male"), col=c("red", "blue"), lty=c(1,2))

# Box plots

boxplot(heightweight$weightLb~heightweight$sex, 
        names=c("Female","Male"), ylab="Weight (Lb)")
boxplot(heightweight$heightIn~heightweight$sex, 
        names=c("Female","Male"), ylab="Height (Inches)")
boxplot(heightweight$weightLb~heightweight$ageMonth, 
        xlab="Age (Months)", ylab="Weight (Lb)")
boxplot(heightweight$heightIn~heightweight$ageMonth, 
        xlab="Age (Months)", ylab="Height (Inches)")

# 3D plots

install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(heightweight$weightLb,heightweight$heightIn,heightweight$ageMonth, 
              main="3D Scatterplot", xlab="Weight (Lb)", ylab="Height (Inches)", 
              zlab="Age (Months)")

install.packages("Rcmdr")
library(Rcmdr)
scatter3d(heightweight$weightLb,heightweight$heightIn,heightweight$ageMonth, 
          main="3D Scatterplot", xlab="Weight (Lb)", ylab="Height (Inches)", 
          zlab="Age (Months)")

# Output plots

## Using RStudio ... or

#change directory
jpeg("C:/Users/Katie/Desktop/myplot.jpg", width = 480, height = 480, units = "px")
plot(heightweight)
dev.off()

# Add images to plots

plot(heightweight$heightIn, heightweight$ageMonth, 
     xlab="Height (Inches)", ylab="Age (Months)")
install.packages("TeachingDemos")
install.packages("png")
library(TeachingDemos)
library(png)

#change directories
img <- readPNG(source="C:/Users/Katie/Pictures/baby.png")
my.symbols(x=52, y=160, ms.image, MoreArgs=list(img=img), ysize=10, symb.plots=TRUE, add=TRUE)
img2 <- readPNG(source="C:/Users/Katie/Pictures/adult.png")
my.symbols(x=70, y=200, ms.image, MoreArgs=list(img=img2), ysize=20, xsize=1.5, 
           symb.plots=TRUE, add=TRUE)

## ADVANCED DATA VISUALISATION - GGPLOT2 ##
install.packages("ggplot2")
install.packages("devtools")
install.packages("cats")
library(ggplot2)
this_base = "0002_add-background-with-cats-package"
devtools::install_github("hilaryparker/cats")
library(cats)
ggplot(heightweight, aes(heightIn, weightLb)) +
  add_cat() + geom_point()

## SWIRL DEMO ##
install.packages("swirl")
library(swirl)
swirl()
