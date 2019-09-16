########################
##      Libraries     ##
########################

## Nice little function that I found that will install packages if they are not 
## installed.

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
          "minpack.lm")

ipak(libs) ## Installs and loads all packages listed in "libs".

########################
##      Load Data     ##
########################



##List of CSV data frames
whDat <- read.csv("warthog.csv", header=T, sep = ',')

whDat$habitat <- factor(whDat$habitat)
whDat$season <- factor(whDat$season)
whDat$gsize <- as.numeric(whDat$gsize)

my.Models <- function()
  {
    myGLMs <- list()

    myGLMs[[1]] <- glm(gsize~habitat,data = whDat)                             ## gsize is only affected by habitat
    myGLMs[[2]] <- glm(gsize~season, data = whDat)                             ## gsize is only affected by season
    myGLMs[[3]] <- glm(gsize~habitat + season, data = whDat)                   ## gsize is affected by habitat and season
    myGLMs[[4]] <- glm(gsize~habitat + season + season*habitat, data = whDat)  ## gsize is affect by habitat and season, and there is a relation between habitat and season
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
      dev.off()
      
    }
  #}else{print("Enter Valid Parameters: 0 - List Test Mode, 1 - Single Test Mode")}
}

my.Plot <- function()
{
  
  dev.off() ## Ensure the plot stream is set back to normal, rather than to the file stream.
  
  plot <- ggplot(whDat, aes(habitat, gsize, colour = season)) +
          geom_point() +
          theme_void() + 
          labs(title = "Pygmy warthog's group size is dependent on the habitat",
               subtitle = "Uluguru National Park, Uganda",
               caption = "Counts were carried out from a moving vehicle for 25 x 1km transects in both habitats",
               x = "Season",
               y = "Number of warthogs spotted per transect",
               colour = "Season") +
          scale_colour_manual(labels = c("Dry", "Wet"), values = c("deepskyblue1", "green4"))
  
  grid.arrange(plot)
  
}









