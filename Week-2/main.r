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
  
  plot3 <- ggplot(data = dat1, aes(x = Lecture,
                                   y = Height,
                                   colour = Gender)) + 
    geom_boxplot() +
    labs(title = "Difference in height (cm) between genders",
         x = "Lecture", 
         y = "Height (cm)")
  
  plot2 <- ggplot(data = dat1, aes(Height)) +
    geom_histogram(binwidth = 45) +
    theme_bw()
  
  
  p1 <- ggplot(dat1, aes(x = Height)) +                            # data & aesthetics
    geom_histogram(aes(),binwidth = 5,
                   colour = "black", 
                   fill = "lightblue")+                          # type of plot & aesthetics
    scale_x_continuous(name = "student's Height (cm)",           # x axis scale type
                       breaks = seq(120, 210, 20),               # x axis limits min, max and intervals
                       limits=c(140, 200)) +                     # x axis limits
    scale_y_continuous(name = "Count") +                         # y axis title
    ggtitle("Students height") +                                 # plot title
    theme_economist()+                                           # aesthetics of plot
    theme(axis.line = element_line(size=1, colour = "black"),    # adds black line
          panel.grid.major = element_blank(),                    # removes major grid lines
          panel.grid.minor = element_blank(),                    # removes minor grid lines
          panel.border = element_blank(),                        # removes panel border
          panel.background = element_blank(),                    # removes background colour
          plot.title=element_text(size = 20, family="Tahoma"),   # type of plot title
          text=element_text(size = 20, family="Tahoma"),         # type and size of axis labels
          axis.text.x=element_text(colour="black", size = 12),   # type and size of x-axis ticks
          axis.text.y=element_text(colour="black", size = 12),   # type and size of y-axis ticks
          axis.line.y = element_line(size=1, colour = "black"))  # type and thickness o
  
  
  
  h<-data.frame(Height = 10*rnorm(1000))
  
  p1_line <- ggplot(h, aes(x = Height)) + 
    geom_histogram(aes(y =..density..), 
                   colour = "black", 
                   fill = "lightblue",
                   breaks = seq(-50, 50, by = 10))+
    stat_function(fun = dnorm, args = list(mean = mean(h$Height), sd = sd(h$Height)))+# this creates the line 
    scale_y_continuous(name = "Count") +                        
    ggtitle("Students height") +                                
    theme_economist()+                                          
    theme(axis.line = element_line(size=1, colour = "black"),    
          panel.grid.major = element_blank(),                   
          panel.grid.minor = element_blank(),                    
          panel.border = element_blank(),                        
          panel.background = element_blank(),                    
          plot.title=element_text(size = 20, family="Tahoma"),   
          text=element_text(size = 20, family="Tahoma"),         
          axis.text.x=element_text(colour="black", size = 12),   
          axis.text.y=element_text(colour="black", size = 12),   
          axis.line.y = element_line(size=1, colour = "black"))
  
  
  
  
  #png(filename = "Output/Graph.png",
  #    width=1920, 
  #    height=1080)
  
  grid.arrange(plot1, ncol = 1)
  
  #dev.off()
  
}





# There is a relation between sex and height. 
my.Summary(1, 1)

### First plot the histogram (hint-height)
hist(dat1$Height)

#### What can you say about the distribution of individual heights in the class?
# The female heights follow a normal distribution. Males also follow a normal 
# distribution.










