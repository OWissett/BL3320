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

mData <- read.csv("part2_data.csv", header = T, sep = ",")
mData$Trial <- factor(mData$Trial)
mData$AMP <- factor(mData$AMP)

################
## Plot Data  ##
################

## Michaelis-Menten

nlsModel2 <- nls(rate ~ (Vm * FBP) / (Km + FBP), 
                 start = list(Vm = 100, Km = 2.5),
                 data = mData)

nlsModel0 <- nls(rate ~ (Vm * FBP) / (Km + FBP), 
                 start = list(Vm = 100, Km = 2.5),
                 data = subset(mData, AMP == 0))

nlsModel70 <- nls(rate ~ (Vm * FBP) / (Km + FBP), 
                 start = list(Vm = 50, Km = 2.5),
                 data = subset(mData, AMP == 70))

nlsModel140 <- nls(rate ~ (Vm * FBP) / (Km + FBP), 
                 start = list(Vm = 30, Km = 2.5),
                 data = subset(mData, AMP == 140))

mmPlot <- ggplot(data = mData, aes(x = FBP, y = rate,  colour = AMP)) +
  geom_point(aes(shape = Trial)) +
  labs(title = "Michaelis-Menten Plot",
       x = "[FBP] (µM)",
       y = "Rate (µmol/min mg)")+ 
  geom_smooth(method = "nls",
              formula = y ~ (Vm * x) / (Km + x),
              method.args = list(start = c(Vm = coef(nlsModel2)[[1]],
                                           Km = coef(nlsModel2)[[2]])),
              se = F) +
  theme_classic() +
  scale_colour_viridis_d("AMP (µM)", option = "D")


mmPlot


## Lineweaver-Burk Plot

recipDat <- data.frame(recipRate = sapply(mData$rate, function(x) return(1/x)),
                       recipConc = sapply(mData$FBP, function(x) return(1/x)),
                       Trial = mData$Trial,
                       AMP = mData$AMP)

##Linear Models for Lineweaver-Burks
{
lbModel0 <- lm(formula = recipRate ~ recipConc, 
               data = subset(recipDat, AMP == 0))

lbModel70 <- lm(formula = recipRate ~ recipConc, 
                data = subset(recipDat, AMP == 70))

lbModel140 <- lm(formula = recipRate ~ recipConc, 
                data = subset(recipDat, AMP == 140))
}

##Calculations for LB
{
  ## Model 0
  ## vmax
  Vmax0 <- 1/coef(lbModel0)[[1]]
  Vmax0
  ## Km
  Km0 <- coef(lbModel0)[[2]]*Vmax0
  Km0
  
  ## Model 70
  ## vmax
  Vmax70 <- 1/coef(lbModel70)[[1]]
  Vmax70
  ## Km
  Km70 <- coef(lbModel70)[[2]]*Vmax70
  Km70
  
  ## Model 140
  ## vmax
  Vmax140 <- 1/coef(lbModel140)[[1]]
  Vmax140
  ## Km
  Km140 <- coef(lbModel140)[[2]]*Vmax140
  Km140
}

lbPlot <- ggplot(data = recipDat, 
                 aes(x = recipConc, 
                     y = recipRate, 
                     colour = AMP)) +
  geom_point(aes(shape = Trial)) +
  geom_smooth(method = "glm",
              se = F,
              fullrange = T) +
  labs(title = "Lineweaver-Burk Plot",
       x = "1/[FBP]",
       y = "1/v") +
  lims(x = c(-1, 2.3),
       y = c(-0.1, 0.1)) +
  theme_light() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_shape_manual(values = c(1, 2, 3),
                     labels = c("1", "2", "3"),
                     name = "Trial") +
  scale_colour_viridis_d("AMP (µM)")

lbPlot