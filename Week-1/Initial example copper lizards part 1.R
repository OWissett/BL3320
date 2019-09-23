###make sure your data files are in your "temp" folder

###Import data for PC (for computer lab workstations replace C with an H)
data1 <- read.table("InitialGLMexample.csv", header=T, sep="," )

###for Mac
##make sure you have a folder called temp that you have mad within your
##documents folder
##if using a Mac remove the hashes from in front of the 2 lines below
#setwd("~/Documents/temp")
#data1 <- read.table("Initial GLM example.csv", header=T, sep="," )


##Look at data file
data1

##Variation in a variable
hist(data1$mass)

##Boxplot
plot(data1$mass~data1$sex)
##Note this doesn't work because R thinks sex is a covariate

##Defining factors
data1$sex<-factor(data1$sex)

##Plot boxplot for factor
plot(data1$mass~data1$sex)

##GLM
model1 <- lm(mass ~ sex, data=data1)
###Statistical significance = TYPE III ANOVA F statistics
drop1(model1,~.,test="F")
##Biological significance
summary(model1)
##Testing model assumptions
par(mfrow=c(2,2))  ##plots 2 by 2 graphs on page
plot(model1)

###illustrate results
###THESE VALUES COME FROM THE SUMMARY TABLE
y.means <- c(10.28, 5.87) ##mean predicted values 
y.se <- c(0.4258, 0.6022)
####Use this bit of code below to CREATE A FUNCTION TO draw error bars 
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
    if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
    arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)}

par(mfrow=c(1,1))
barx <- barplot(y.means,ylim=c(0,12))
error.bar(barx,y.means, y.se)
mtext(side=3,"Mass of copper lizards by sex", cex=1.5)
mtext(side=2, adj=0.5,line=2.8,"Mass of copper lizards (g)")
mtext(side=1, line=1, adj = 0.25, "Male")
mtext(side=1, line=1, adj = 0.75, "Female")

###illustrate results
###THESE VALUES COME FROM THE SUMMARY TABLE
y.means <- c(3.6, 4.2, 8.1) ##mean predicted values 
y.se <- c(0.2, 0.3, 0.1)
####Use this bit of code below to CREATE A FUNCTION TO draw error bars 
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)}

par(mfrow=c(1,1))
barx <- barplot(y.means,ylim=c(0,12), col = c("blue", "yellow", "red"))
error.bar(barx,y.means, y.se)
mtext(side=3,"Density of chimps in each country", cex=1.5)
mtext(side=2, adj=0.5,line=2.8,"Mean chimp density per km (+\- 1 SE)")
mtext(side=1, line=1, adj = 0.12, "Congo")
mtext(side=1, line=1, adj = 0.5, "Gabon")

mtext(side=1, line=1, adj = 0.87, "Cameroun")








ggplot(data = data1, aes(length, ))


#######################################EXERCISE
##The mean chimp densities for 3 areas (Congo, Gabon & Cameroun are 
##3.6, 4.2 & 8.1 chimps per 
##square kilometer
## The standard errors of these measurements are 0.2, 0.3, 0.1
## Illustrate and fully label these results 

chimpDat <- data.frame("density" = c(3.6, 4.2, 8.1), 
                       "country" = c("Congo", "Gabon", "Cameroun"),
                       "std err" = c(0.2, 0.3, 0.1))

ggplot(chimpDat) + geom_bar()