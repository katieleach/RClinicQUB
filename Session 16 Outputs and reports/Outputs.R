####################
###Outputs from R###
####################

### Writing out data files
#First let's create some data

x <- rnorm(30)
y <- rnorm(30)
z <- rnorm(30)

data.xyz <- data.frame(x, y, z)

write.csv(data.xyz, 'q:/data.xyz.csv', row.names=FALSE) #writes csv file

write.table(data.xyz, 'q:/data.xyz.txt', row.names=FALSE) #writes text file


### Create text file containing model summary

library(nlme)
library(spdep)

install.packages('RANN')

#load wheat2 data
data(Wheat2)

#create neighbourhood object using k nearest neighbours

neigh <- knn2nb(knearneigh(cbind(Wheat2$latitude, Wheat2$longitude)))

#use lm with spatial errors

mod.sp <- spautolm(yield ~ variety + Block, data=Wheat2, listw=nb2listw(neigh), family='SAR')

summary(mod.sp)

out <- capture.output(summary(mod.sp))

cat(out,file="q:/out1.txt",sep="\n",append=TRUE)

#cat(out, file='c:/Users/Hannah/Documents/R Clinic/Session 16 Outputs/out1.txt', sep="\n", append=TRUE)


####Outputting graphics

#we're going to use the mtcars data for an example of how to output a pretty plot
install.packages('corrplot')
library(corrplot)

data(mtcars)

cor.cars <- cor(mtcars, method='pearson')
corrplot(cor.cars)

#jpeg('c:/Users/Hannah/Documents/R Clinic/Session 16 Outputs/corr.jpg', width=480, height=480,units='px')

jpeg('q:/corr.jpg', width=480, height=480, units='px')

corrplot(cor.cars)
dev.off()

##########################################
############ R Markdown ##################
##########################################

# Install and load markdown and knitr

install.packages("markdown")
install.packages("knitr")

library(markdown)
library(knitr)


#########################
### Shiny apps in R #####
#########################

## Create interactive web apps with your analysis, using only R language

library(shiny)

runExample("01_hello")

## There are some great tutorials and examples of shiny apps at shiny.rstudio.com

