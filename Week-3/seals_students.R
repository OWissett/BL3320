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

########### Phocine Distemper Virus effects on harbour seal body condition ####################

# The Phocine distemper virus (PDV) is a type of Morbillivirus that is pathogenic to pinniped species. There have been two massive PDV outbreaks causing large-scale mortalities in seal populations in Europe, one in 1988 and another one in 2002. These outbreaks primarily affected harbour seal populations, as grey seals are less sensitive to PDV infection. Clinical manifestation of the PDV infection includes respiratory problems, fever, diarrhoea and inability to dive, as well as an increased likelihood for secondary infections leading to death.

# The dataset we will be examining contains measurements for body length and axial body girth for harbour seals, with information on the sex of the seal and whether it died due to PDV infection or due to other causes. As a seal grows from pup to adult we should see changes in body length as well as in axial girth (i.e. seals not only get longer but also fatter). We will first look at the relationship between axial girth and body length, and test whether this relationship is different for male and female harbour seals. We then hypothesize that such relationship(s) will be affected by a PDV infection, i.e. a PDV infection will ultimately generate an observable change in body condition. 

## Clean your data space
rm(list=ls())

## Import data file
####put data files in a folder called temp on your main drive - 
## C: if on your laptop, H: in the Bute Computer lab
seals<-read.csv("seals.csv", header=T)  ## or read.table ("C:/temp/seals.csv", header=T, sep="," )



## There are 4 variables
# sex: 1 = male; 2 = female
# exposure: PDV = seal died due to the PDV epidemic; Other = seal died of other causes
# girth: axillary girth, measured as a circumference of the body behind the fore-flippers (cm)
# length: nose to tail body length (cm)

## Identify and define factors
str(seals) # this will show you the structure of the dataframe
# define variables as factors as needed

seals$sex <- factor(seals$sex)

######################################################################################
## QUESTION 1 - Does body girth in harbour seals depend on body length? 

# (hint = here we are not interested in any effects from sex or PDV)

# Specify a model to test this hypothesis and illustrate the results with a plot.

#H0 there is no relation between body girth and body length
#H1 there is a relationship between body girth and body length

model1 <- glm(girth ~ length, data = seals, family = "gaussian")
summary(model1)
#
#
#Coefficients:
#               Estimate    Std. Error  t value   Pr(>|t|)    
#  (Intercept)  7.15894      2.20962    3.24      0.00125 ** 
#  length       0.65649      0.01733   37.87      < 2e-16 ***
#
# The is a relationship between the girth and length
#
par(mfrow = c(2,2))
plot(model1)
# Model assumptions are not being violated
#
#

plot1 <- ggplot(data = seals, aes(x = length, y = girth)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, formula = y~x) + 
  labs(title = "Relationshp between girth and length in Harbour Seals",
       x = "Length (cm)",
       y = "Girth (cm)")

plot1 + theme_bw()

# The plot should include a line of the relationship. Remember you can use abline or the parameter estimates (intercept and gradient) from the model output


######################################################################################
## QUESTION 2 - Does the relationship between body girth and body length depend on sex?

model2 <- glm(girth ~ length + sex + length * sex, 
              data = seals,
              family = "gaussian")
summary(model2)

par(mfrow = c(2,2))
plot(model2)

model3 <- lm(girth ~ length + sex,
              data = seals,
              family = "gaussian")
summary(model3)

par(mfrow = c(2,2))
plot(model3)

AIC(model2, model3)
# Model 2 is superior as it has a lower AIC value, 5731.505 compared to 5735.545 (of model 3)


# Fit a model to test this hypothesis and visualize the results. 
# (hint = think if you need to incorporate an interaction term).

# To visualize the results, plot the relationship between girth and length in males and females in the same figure using two different colors (2 lines).
# You can subset your data by sex and then plot each with a different color,e.g. males<-subset(seals, seals$sex==1)
# OR you can add col = sex in your plot command to colour your data by sex

plot2 <- ggplot(data = seals, aes(x = length, y = girth , colour = sex)) +
  geom_point() +
  geom_smooth(se = F, method = "lm") +
  labs(title = "Relation between girth and length for male and female harbour seals",
       x = "Length (cm)",
       y = "Girth (cm)") +
  scale_colour_manual(labels = c("Male", "Female"), values = c("#00CC00","#FF33FF"), name = "Sex")

par(mfrow = c(1,1))
plot2

######################################################################################
## QUESTION 3 - Does PDV affect the relationship between body girth and body length? # 
# (hint: consider the effect of PDV regardless of the sex)

model4 <- glm(girth ~ length + exposure, data = seals)
summary(model4)

model5 <- glm(girth ~ length * exposure, data = seals)
summary(model5)

AIC(model4, model5)
# Model 4 is better than model 5

par(mfrow = c(2, 2))
plot(model4)
plot(model5)

# Fit a model to test this hypothesis and visualize the results. 
# To visualize the results, plot the relationship between girth and length in PDV and other seals in the same figure 
# using two different colors (2 lines).

plot3 <- ggplot(data = seals, aes(x = length, y = girth, colour = exposure)) +
  geom_point() +
  geom_smooth(se = F, method = "lm") +
  scale_colour_manual(labels = c("Other", "PDV"), values = c("#000000","#FF0000"), name = "Cause of Death") +
  theme_classic() +
  labs(tile = "Relationship between length and girth in harbour seals with and without PDV",
       x = "Length (cm)",
       y = "Girth (cm)")
  
# Try to plot the lines from the model output by finding the intercept + gradient for PDV seals and Other seals (hint = two 2 intercepts and 2 gradients (4 terms))
# You can then double check you got it right by plotting the lines with the abline command on subsetted datasets, e.g. PDV<-subset(seals, seals$exposure=="PDV")


#######################################################################################################################

## THIS IS AN INDEPENDENT EXERCISE - IT NEEDS TO BE CHECKED BY DEMONSTRATORS/LECTURER WHEN COMPLETED
## YOU ALSO NEED TO UPLOAD THE RESULTS SECTION ONTO MMS BEFORE YOU LEAVE THE WORKSHOP (SEE DETAILS BELOW)

## QUESTION 4 - Is the effect of PDV on the relationship between body girth and body length different for males and females? 

model6 <- glm(girth ~ length + sex + exposure + length*sex + exposure*sex,
              data = seals,
              family = "gaussian") #Girth is affected by length, sex and exposure. length and sex have a relationship. Exposure and sex have a relationship

model7 <- glm(girth ~ length + sex + exposure + length*sex,
              data = seals,
              family = "gaussian") #Girth is affected by length, sex and exposure. length and sex have a relationship. Exposure and sex have a no relationship

model8 <- glm(girth ~ length*exposure*sex,
              data = seals,
              family = "gaussian")



par(mfrow = c(2,2))
plot(model6)
plot(model7)
plot(model8)
# Model assumptions are not violated in either model

# Fit a model to test this hypothesis 

AIC(model6, model7, model8)
#AICs are within 2 of each other, therefor AIC cannot be used to distinguish between better and worse model.

# Visualize the results of this model with ONE of the following options:
#a) Make two plots that show the relationship between girth and length in PDV animals split by sex, compared to Other seals split by sex
#b) Make two plots that show the relationship between girth and length in males split by PDV or Other, compared to females split by PDV or Other
#c) Make a single plot to show the relationship between girth and length in males and females that died from PDV or Other; i.e. you will need to show 4 lines in this plot (hint: you need 8 terms from the model output, 4 intercepts, 4 gradients).

plot4 <- ggplot(data = seals,
                aes(x = length, 
                    y = girth, 
                    colour = sex,
                    shape = exposure,
                    linetype = exposure)) +
  geom_point(size = 1) +
  geom_smooth(se = F, method = lm) +
  scale_colour_manual(labels = c("Male", "Female"), 
                      values = c("#3300FF", "#999999"),
                      name = "Sex") +
  labs(title = "Relationship between length and girth for Harbour Seals",
       x = "Length (cm)",
       y = "Girth (cm)") +
  scale_linetype_manual(name = "Cause of Death Regression Line",
                        labels = c("Other", "PDV"),
                        values = c("solid", "dotdash")) +
  scale_shape_manual(name = "Cause of Death", 
                     labels = c("Other", "PDV"),
                     values = c(1, 0))

# Compare all the models you have fitted. What is the most supported model? 

AIC(model1 , model2, model5, model6, model8)

# How do you interpret the results from a biological point of view?

## To upload onto MMS: 
# 1) the model output for Q.4
# 2) plot(s) to visualize the results from model fitted in Q.4 (chose one of the options you did above)
# 3) a BRIEF description of the statistical and biological interpretation of the results.

