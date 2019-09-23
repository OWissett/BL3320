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



























