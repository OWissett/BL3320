## TO IMPORT .CSV FILE
##To make this import work create a folder (also called directory) called
##temp on the C: drive of your computer
## then put the data file into this folder ie the folder called temp

###make sure your data files are in your "temp" folder

###Import data for PC (for computer lab workstations replace C with an H)
data <- read.table("basic.csv", header=T, sep="," )

###for Mac
##make sure you have a folder called temp that you have made within your
##documents folder
##if using a Mac remove the hashes from in front of the 2 lines below
#setwd("~/Documents/temp")
#data <- read.table("basic.csv", header=T, sep="," )


data

###Look at data
data
head(data)
str(data)

###Look at dependent variable - what we are predicting - growth
hist(data$growth)

###Tell R which predictor variables are factors - i.e. light 1 = LOW and 2 = HIGH
data$light <- factor(data$light)

###plot some graphs
####First what is the effect of light treatment on growth
plot(growth ~ light, data = data)

###Refine your plot
plot(growth ~ light, data = data, ylab="Growth of Plant cm", xlab="Light Intensity",
   las=1, col="pink")
text(1, 17, "LOW", cex=2)
text(2, 17, "HIGH", cex=2)

##Have complete freedom with your plot
plot(growth ~ light, data = data, col="blue", axes=FALSE, xlab="", ylab="")
axis(2, las=1, c(0,5,10,15, 20))
#axis(1, las=1, c( 1, 2))
mtext(side=1, line=2, adj = 0.25, "Low",cex=2)
mtext(side=1, line=2, adj = 0.75, "High",cex=2)
mtext(side=2, line=2.5, "Plant growth (cm)",cex=1.5)

####Second - what is the effect of temperature on growth
plot(growth ~ temp, data=data, pch=as.numeric(light))

###plotting lines
##Overall line
abline(lm(growth ~ temp, data=data), lty=1)

###Separate lines - subset the data
##this is note
low <- subset(data, light=="1")
low
high <- subset(data, light=="2")
high
abline(lm(growth ~ temp, data=low), lty=3, col="blue")
abline(lm(growth ~ temp, data=high), lty=2, col="red")

#######Produce a pretty graph as example on the board....
#try googling "R plot different symbols" or "R change size of axis labels"

####################################################################################
####################################################################################

#####Further practice to try at after you have done the introductory workshop 
##### Test whether the differences and relationships observed are actually 
##### statistically significant and identify the best model

##Answers in "basic MODEL.R"

##sample starter model
model1 <- lm(growth ~ temp + light + temp*light, data=data)
