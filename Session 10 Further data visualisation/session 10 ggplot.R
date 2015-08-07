## Session 10 - Further Data Visualisation in R ##

##################################################################
#### SWIRL - good training package to learn R in R!
## This is the code to install Swirl and to 
## download the Exploratory Data Analysis package

install.packages("swirl")
library(swirl)
install_from_swirl("Exploratory_Data_Analysis")
swirl()

###################################################################
##################################################################

library(ggplot2)

## scatterplot with qplot
qplot(cty, hwy, data=mpg)

## add a factor to the plot
qplot(cty, hwy, data=mpg, colour=drv)

## split the plot by the factor drv
qplot(cty, hwy, data=mpg, facets=.~drv)


## make boxplot by  using the argument "geom"

qplot(drv, cty, data=mpg, geom="boxplot")

## splitting boxplot by more than 1 factor
str(mpg)
## cyl is not seen as a factor by R so use as.factor to define it as a factor
mpg$cyl <- as.factor(mpg$cyl)
qplot(drv, cty, data=mpg, geom="boxplot", fill=cyl)

## histogram - just specify the variable and the data frame

qplot(cty, data=mpg)

## make split plot histograms

qplot(cty, data=mpg, facets=drv~.)


#######################################################################
####### ggplot2 - Average length of hedgerow in UK and Ireland ########
#######################################################################

install.packages("dplyr")

# First create dataframe for each country/state with total length of hedgerow and total area km2 

country <- c("England", "Scotland", "Wales", "Ireland", "NI")
hedge <- c(558150, 88710, 48680, 382000, 118619)
area <- c(130395, 78387, 20761, 70273, 13843)

cha <- data.frame(country, hedge, area)

cha

# We want to know the average length of hedgerow per km2 of each of these regions
# One way to do this is using the package dplyr
# We can use the function mutate() to add a column based on data in the existing dframe

library('dplyr')

hra <- mutate(cha, average = hedge/area)

hra

# we might want to plot values of the variable 'average' onto the graph later, so it might be a good idea to round the numbers
hra[ ,'average'] <- round(hra[ ,'average'], 2)

# We can use qplot to quickly look at this data

qplot(hra$country, hra$average, geom = "bar", stat = "identity")

# It might make a prettier graph to have the bars in ascending order
hra$country <- ordered(hra$country, levels = c("NI", "Ireland", "England", "Wales", "Scotland"))

###################
##### ggplot #####
##################

## Order of arguments in ggplot = dataframe, aes + geom
## We can add 'layers' to our plot using the '+' operator 
## geom_bar is our first layer, and call barplot
# stat = "identity" - maps the value of the data to the y aesthetic (the alternative being 'stat = "bin", which maps the number of cases)
# fill colour - can use web colours in R
# colour = outline colour

p1 <- ggplot(hra, aes(x = country, y = average)) + 
  geom_bar(stat = "identity", fill = "#00CCCC", colour = "black")

p1

## Change the y label using ylab()
p2 <- p1 + ylab("Average length (km)")

p2

## Add title using ggtitle(). (*km^2 creates superscript 2)
p3 <- p2 + ggtitle(expression(paste("Average length of hedgerow per " *km^2)))

p3

## Add averages to the bars - vjust alters the height of the numbers (vertical justification))
p4 <- p3 + geom_text(aes(label=average), vjust=1.5)

p4

### We use theme() to change the general look of the plot ####

## To get rid of the wierd grey background grid
p5 <- p4 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())

p5

## get rid of the x axis label
p6 <- p5 + theme(axis.title.x=element_blank())

p6

## Bold face and larger text (+ vjust to move the bigger label away from the plot)

p7 <- p6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))

p7

## Axis text bold face, black and bigger

p8 <- p7 +  theme(axis.text.x=element_text(face="bold", size=15, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))


p8

## Change plot title
p9 <- p8 + theme(plot.title = element_text(face="bold", size=25))

p9


################################
#### How I would write it  ####
###############################

p <- ggplot(hra, aes(x = country, y = average)) + 
          geom_bar(stat = "identity", fill = "#00CCCC", colour = "black") 

p +  ylab("Average length (km)") +
  ggtitle(expression(paste("Average length of hedgerow per " *km^2))) +
  geom_text(aes(label=average), vjust=1.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5)) +
  theme(axis.text.x=element_text(face="bold", size=15, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=25)) 
 

## There are many ways to get the same result in ggplot, there is no 'right' way : )
