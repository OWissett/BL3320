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