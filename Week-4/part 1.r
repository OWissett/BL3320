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
            "ggthemes")
  
  ipak(libs) ## Installs and loads all packages listed in "libs".
  libs <- NULL ##Clear the memory.
}

#################
##  Load Data  ##
#################

mData <- read.csv("part1_data.csv", header = T, sep = ",")
mData$Trial <- factor(mData$Trial)

################
## Plot Data  ##
################

## Michaelis-Menten

nlsModel1 <- nls(rate ~ (Vm * FBP) / (Km + FBP), 
                 start = list(Vm = 100, Km = 2.5),
                 data = mData)

mmPlot <- ggplot(data = mData, aes(x = FBP, y = rate)) +
  geom_point(aes(shape = Trial)) +
  labs(title = "Michaelis-Menten Plot",
       x = "[FBP] (µM)",
       y = "Rate (µmol/min mg)")+ 
  geom_smooth(method = "nls",
              formula = y ~ (Vm * x) / (Km + x),
              method.args = list(start = c(Vm = coef(nlsModel1)[[1]],
                                           Km = coef(nlsModel1)[[2]])),
              se = F) +
  theme_classic()

mmPlot

## Lineweaver-Burk Plot

lbModel <- lm(formula = recipRate ~ recipConc, data = recipDat)

summary(lbModel)

## vmax
Vmax <- 1/coef(lbModel)[[1]]
Vmax
## Km
Km <- coef(lbModel)[[2]]*Vmax
Km

recipDat <- data.frame(recipRate = sapply(mData$rate, function(x) return(1/x)),
                       recipConc = sapply(mData$FBP, function(x) return(1/x)),
                       Trial = mData$Trial)

lbPlot <- ggplot(data = recipDat, aes(x = recipConc, y = recipRate)) +
  geom_point(shape = recipDat$Trial) +
  geom_smooth(method = "glm",
              se = F,
              fullrange = T) +
  labs(title = "Lineweaver-Burk Plot",
       x = "1/[FBP]",
       y = "1/v") +
  lims(x = c(-1, 2.3),
       y = c(0, 0.1)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(axis.line = element_blank())

lbPlot