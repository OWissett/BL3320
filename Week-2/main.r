########################
##      Libraries     ##
########################

## Nice little function that I found that will install packages if they are not 
## installed.

{
  ipak <- function(pkg){
    
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    
    if (length(new.pkg)) 
      
      install.packages(new.pkg, dependencies = TRUE)
    
    sapply(pkg, require, character.only = TRUE)
    
  }
  
  libs <- c("ggplot2", 
            "gridExtra",
            "itsmr",
            "pracma",
            "git2r",
            "car",
            "minpack.lm",
            "ggthemes",
            "Hmisc")
  
  ipak(libs) ## Installs and loads all packages listed in "libs".
  libs <- NULL ##Clear the memory.
}

######################
##    Load Data     ##
######################

dat1 <- read.csv("data1.csv", header = T, sep = ",")
dat2 <- read.csv("data2.csv", header = T, sep = ",")

######################
##  Data Formating  ##
######################

dat1$Shoe.Size <- factor(dat1$Shoe.Size)

dat1Monday <- subset(dat1, Lecture == "Monday")
dat1Tuesday <- subset(dat1, Lecture == "Tuesday")
dat1Wednesday <- subset(dat1, Lecture == "Wednesday")


# There is a relation between sex and height. 

### First plot the histogram (hint-height)
#### What can you say about the distribution of individual heights in the class?

########################
##     Modelling      ##
########################

my.Models <- function()
{
  myGLMs <- list()
  
  ## Add Models to myGLMs
  
  myGLMs[[1]] <-  glm(Height ~ Gender, data = dat1) ## Height is dependent on gender
  
  return(myGLMs)
}


##Purpose: Print a given string onto a graphical plot
##INPUT: my_stext, a string that is printed onto the plot
##OUTPUT: a plot that is outputted to the current graphics writer.
my.PrintTextToPlot <- function(my_text){
  
  par(mar=c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, my_text, 
       cex = 2, col = "black")
  par(mar = c(5, 4, 4, 2) + 0.1)
}




###################################
##    USER CALL FUNCTIONS        ##
###################################

##Purpose: Summarises the GLMs
##INPUT:   m, the lower index number for the model to be summarised; n, the upper
##         index number for the model to be summarised. Values must be integers.
##OUTPUT:  Text file called GLM_sum.txt containing summary and AIC data 

my.Summary <- function(m, n){
  model <- my.Models()
  sink("GLM_sum.txt", append = F)
  
  for(i in m:n){
    
    print(paste("############### Model: ", toString(i), "###############"))
    print(summary(model[[i]]))
    
  }
  
  sink()
  
}

##Purpose: Creates summary plots for the models
##INPUT:   m, the lower index number for the model to be summarised; n, the upper
##         index number for the model to be summarised. Values must be integers
##         MODE: 1 - Single Test, and will out test to Plot. 0 - List Test, and will ouput PNG file
##         model: glm/lm (assuming MODE==1) or list of GLMs (assuming MODE==0)
##OUTPUT:  Plots of models outputted to graphics

my.ModelPlot <- function(m, n, MODE, model){
  
  #if(MODE==1){
  
  # AIC_text <- toString(AIC(model))
  # Form_text <- toString(model[["call"]])
  # par(mfrow=c(3,2))
  # plot(model)
  # my.PrintTextToPlot(paste("AIC: ", AIC_text, "\n Formula:\n ", Form_text))
  # hist(resid(model), main = "Histogram of Residues for Model", xlab="Residue Value")
  #}
  #else if(MODE==0 & isNumeric(m) & isNumeric(n) & length(model)>1)
  # {
  
  for(i in m:n){
    
    AIC_text <- toString(AIC(model[[i]]))
    Form_text <- toString(model[[i]][["call"]])
    
    png(filename = paste("Analysis Plots/Graph", toString(i), ".png"),
        width=1920, 
        height=1080)
    
    par(mfrow=c(3,2))
    plot(model[[i]])
    
    my.PrintTextToPlot(paste("AIC: ", AIC_text, "\n Formula: ", Form_text))
    hist(resid(model[[i]]), main = paste("Histogram of Residues for Model: ",
                                         toString(i)), xlab="Residue Value")
    
    dev.off()
    #dev.off()
    
  }
}



my.Plot <- function()
{
  
  ## Create the GGPlots here
  
  plot1 <- ggplot()
  
  plot2 <- ggplot()
  
  png(filename = "Output/Graph.png",
      width=1920, 
      height=1080)
  
  grid.arrange(plot1, plot2, ncol = 2)
  
  dev.off()
  
}

















