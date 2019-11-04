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
  
  ipak(libs)   ## Installs and loads all packages listed in "libs".
  libs <- NULL ##Clear the memory.
}

###############
## Load Data ##
###############

AlienData <- read.csv("NASA_alien_parasite_data.csv")


##################
## Process Data ##
##################

AlienData$infectedBool <- factor(sapply(AlienData$infected, YNtoBool))

YNtoBool <- function(x){
  if(x == "Yes") return(1)
  else if(x == "No") return(0)
  return(-1) ## -1 error code
}


######################
## Models and Stuff ##
######################

inv.logit <- function(x){exp(x)/(1+exp(x))}

model1 <- glm(infectedBool ~ job, 
              data = AlienData,
              family = binomial)

plot(model1)

summary(model1) 



## Question 4.

sal.val <- seq(min(AlienData$salary), 
               max(AlienData$salary), 
               length.out = 100)

model2 <- glm(infectedBool ~ salary,
              data = AlienData,
              family = binomial)

pred1 <- predict(model2, newdata = data.frame(salary = sal.val))

invLogitPred1 <- inv.logit(pred1)


plot(AlienData$salary, as.numeric(AlienData$infectedBool) - 1)

lines(sal.val, invLogitPred1)


summary(model2)

#There is a significant relation between salary and infection


## Question 5

model3 <- glm(infectedBool ~ salary * job,
              data = AlienData,
              family = binomial)

model4 <- glm(infectedBool ~ salary + job,
              data = AlienData,
              family = binomial)

## Generate a inverse logistic predictors
sal.valG <- seq(min(subset(AlienData$salary, AlienData$job == "ground")), 
               max(subset(AlienData$salary, AlienData$job == "ground")), 
               length.out = 100)
sal.valA <- seq(min(subset(AlienData$salary, AlienData$job == "astronaut")), 
                max(subset(AlienData$salary, AlienData$job == "astronaut")), 
                length.out = 100)
sal.valM <- seq(min(subset(AlienData$salary, AlienData$job == "manager")), 
                max(subset(AlienData$salary, AlienData$job == "manager")), 
                length.out = 100)

invLogitPred3G <- inv.logit(predict(model3, newdata = data.frame(salary = sal.valG,
                                                       job = 'ground')))
invLogitPred3A <- inv.logit(predict(model3, newdata = data.frame(salary = sal.valA,
                                                       job = 'astronaut')))
invLogitPred3M <- inv.logit(predict(model3, newdata = data.frame(salary = sal.valM,
                                                       job = 'manager')))
pred3G <- predict(model3, newdata = data.frame(salary = sal.valG,
                                               job = 'ground'),
                  se.fit =T)
pred3A <- predict(model3, newdata = data.frame(salary = sal.valA,
                                               job = 'astronaut'),
                  se.fit = T)
pred3M <- predict(model3, newdata = data.frame(salary = sal.valM,
                                               job = 'manager'),
                  se.fit = T)

plot(AlienData$salary, 
     as.numeric(AlienData$infectedBool) -1, 
     xlab = "Salary ($)", 
     ylab = "Infection Status")
lines(sal.valG, invLogitPred3G, col = "green")
lines(sal.valA, invLogitPred3A, col = "blue")
lines(sal.valM, invLogitPred3M, col = "magenta")

lines(sal.valA, inv.logit(pred3A$fit-1.96*pred3A$se.fit), col = "blue", lty = "dotted")
lines(sal.valA, inv.logit(pred3A$fit+1.96*pred3A$se.fit), col = "blue", lty = "dotted")

lines(sal.valG, inv.logit(pred3G$fit-1.96*pred3G$se.fit), col = "green", lty = "dotted")
lines(sal.valG, inv.logit(pred3G$fit+1.96*pred3G$se.fit), col = "green", lty = "dotted")

lines(sal.valM, inv.logit(pred3M$fit-1.96*pred3M$se.fit), col = "magenta", lty = "dotted")
lines(sal.valM, inv.logit(pred3M$fit+1.96*pred3M$se.fit), col = "magenta", lty = "dotted")

legend("topright", 
       c("Astronaut","Ground","Manager"), 
       lwd = 1, 
       col = c("blue", "green", "magenta"),
       cex = 0.6)

# ggplot1 <- ggplot(data = AlienData, aes(x = salary, 
#                                         y = as.numeric(infectedBool), 
#                                         linetype = job,
#                                         shape = job)) +
#   geom_point() +
#   stat_smooth(method = "glm",
#               family = binomial,
#               formula = y~x)


### Week 7

# Question 1

meanParasite   <- mean(AlienData$alienParasites)
modeParasite   <- mode(AlienData$alienParasites)
medianParasite <- median(AlienData$alienParasites)
rangeParasite  <- range(AlienData$alienParasites)
minParasite    <- min(AlienData$alienParasites)
maxParasite    <- max(AlienData$alienParasites)
varParasite    <- var(AlienData$alienParasites)


parasiteSummary <- c(meanParasite,
                     modeParasite,
                     medianParasite,
                    rangeParasite,
                    minParasite,
                    maxParasite,
                    varParasite)

boxplot(AlienData$alienParasites)
hist(AlienData$alienParasites)
densityPlot <- ggplot(data = AlienData, aes(x = alienParasites)) +
  geom_density()

##Does job type affect number of parasites

poModel1 <- glm(data = AlienData,
               family = poisson,
               formula = alienParasites ~ job)

meanJobParasites <- c(mean(subset(AlienData, job == "astronaut")$alienParasites),
                      mean(subset(AlienData, job == "manager")$alienParasites),
                      mean(subset(AlienData, job == "ground")$alienParasites))

varJobParasites <- c(var(subset(AlienData, job == "astronaut")$alienParasites),
                     var(subset(AlienData, job == "manager")$alienParasites),
                     var(subset(AlienData, job == "ground")$alienParasites))

par(mfrow = c(2,2))
plot(poModel)
par(mfrow = c(1,1))

# Yes - job type affects the number of parasites on average.

poModel2 <- glm(data = AlienData,
                family = poisson,
                formula = alienParasites ~ job * salary)

predAstro <- predict(poModel2, data.frame(salary = sal.valA,
                                          job = "astronaut"), se.fit = T)

predManager <- predict(poModel2, data.frame(salary = sal.valM,
                                          job = "manager"), se.fit = T)

predGround <- predict(poModel2, data.frame(salary = sal.valM,
                                            job = "ground"), se.fit = T)

AstroGraph <- ggplot(data = AlienData,
                     aes(x = salary, y = alienParasites, colour = job)) +
  geom_point() +
  geom_line(aes(y=fitted(poModel2))) +
  stat_ellipse() +
  theme_classic()
  
