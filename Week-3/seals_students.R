########### Phocine Distemper Virus effects on harbour seal body condition ####################

# The Phocine distemper virus (PDV) is a type of Morbillivirus that is pathogenic to pinniped species. There have been two massive PDV outbreaks causing large-scale mortalities in seal populations in Europe, one in 1988 and another one in 2002. These outbreaks primarily affected harbour seal populations, as grey seals are less sensitive to PDV infection. Clinical manifestation of the PDV infection includes respiratory problems, fever, diarrhoea and inability to dive, as well as an increased likelihood for secondary infections leading to death.

# The dataset we will be examining contains measurements for body length and axial body girth for harbour seals, with information on the sex of the seal and whether it died due to PDV infection or due to other causes. As a seal grows from pup to adult we should see changes in body length as well as in axial girth (i.e. seals not only get longer but also fatter). We will first look at the relationship between axial girth and body length, and test whether this relationship is different for male and female harbour seals. We then hypothesize that such relationship(s) will be affected by a PDV infection, i.e. a PDV infection will ultimately generate an observable change in body condition. 

## Clean your data space
rm(list=ls())

## Import data file
####put data files in a folder called temp on your main drive - 
## C: if on your laptop, H: in the Bute Computer lab
seals<-read.csv("H:/temp/seals.csv", header=T)  ## or read.table ("C:/temp/seals.csv", header=T, sep="," )

##on Mac
####put data file in a folder called temp in your documents folder
setwd("~/Documents/temp")
seals<-read.csv("seals.csv", header=T) 

## There are 4 variables
# sex: 1 = male; 2 = female
# exposure: PDV = seal died due to the PDV epidemic; Other = seal died of other causes
# girth: axillary girth, measured as a circumference of the body behind the fore-flippers (cm)
# length: nose to tail body length (cm)

## Identify and define factors
str(seals) # this will show you the structure of the dataframe
# define variables as factors as needed


######################################################################################
## QUESTION 1 - Does body girth in harbour seals depend on body length? 

# (hint = here we are not interested in any effects from sex or PDV)

# Specify a model to test this hypothesis and illustrate the results with a plot. 
# The plot should include a line of the relationship. Remember you can use abline or the parameter estimates (intercept and gradient) from the model output


######################################################################################
## QUESTION 2 - Does the relationship between body girth and body length depend on sex?

# Fit a model to test this hypothesis and visualize the results. 
# (hint = think if you need to incorporate an interaction term).

# To visualize the results, plot the relationship between girth and length in males and females in the same figure using two different colors (2 lines).
# You can subset your data by sex and then plot each with a different color,e.g. males<-subset(seals, seals$sex==1)
# OR you can add col = sex in your plot command to colour your data by sex

######################################################################################
## QUESTION 3 - Does PDV affect the relationship between body girth and body length? # 
# (hint: consider the effect of PDV regardless of the sex)

# Fit a model to test this hypothesis and visualize the results. 
# To visualize the results, plot the relationship between girth and length in PDV and other seals in the same figure 
# using two different colors (2 lines).

# Try to plot the lines from the model output by finding the intercept + gradient for PDV seals and Other seals (hint = two 2 intercepts and 2 gradients (4 terms))
# You can then double check you got it right by plotting the lines with the abline command on subsetted datasets, e.g. PDV<-subset(seals, seals$exposure=="PDV")


#######################################################################################################################

## THIS IS AN INDEPENDENT EXERCISE - IT NEEDS TO BE CHECKED BY DEMONSTRATORS/LECTURER WHEN COMPLETED
## YOU ALSO NEED TO UPLOAD THE RESULTS SECTION ONTO MMS BEFORE YOU LEAVE THE WORKSHOP (SEE DETAILS BELOW)

## QUESTION 4 - Is the effect of PDV on the relationship between body girth and body length different for males and females? 

# Fit a model to test this hypothesis 

# Visualize the results of this model with ONE of the following options:
#a) Make two plots that show the relationship between girth and length in PDV animals split by sex, compared to Other seals split by sex
#b) Make two plots that show the relationship between girth and length in males split by PDV or Other, compared to females split by PDV or Other
#c) Make a single plot to show the relationship between girth and length in males and females that died from PDV or Other; i.e. you will need to show 4 lines in this plot (hint: you need 8 terms from the model output, 4 intercepts, 4 gradients).

# Compare all the models you have fitted. What is the most supported model? 

# How do you interpret the results from a biological point of view?

## To upload onto MMS: 
# 1) the model output for Q.4
# 2) plot(s) to visualize the results from model fitted in Q.4 (chose one of the options you did above)
# 3) a BRIEF description of the statistical and biological interpretation of the results.

