###Import data
data1 <- read.table("InitialGLMexample.csv", header=T, sep="," )

##Look at data file
data1

##Defining factors
data1$sex<-factor(data1$sex)

##Plot boxplot for factor
plot(data1$mass~data1$sex)

##GLM
model1 <- lm(mass ~ sex, data=data1)
###Statistical significance = TYPE III ANOVA F statistics
drop1(model1,~.,test="F")
##Biological significance
summary(model1)
##Testing model assumptions
par(mfrow=c(2,2))  ##plots 2 by 2 graphs on page
plot(model1)


####################################################EXERCISE
####Explore relationship between mass and length
##DEFINE MODEL 2
model2 <- lm(mass ~ length, data = data1)
model3 <- lm(mass ~ length + sex, data = data1)
model4 <- lm(mass ~ length * sex, data = data1)

##ANOVA TABLE FOR MODEL 2
drop1(model2, ~., test="F")
drop1(model3, ~., test="F")
drop1(model4, ~., test="F")

##SUMMARY TABLE FOR MODEL 2
summary(model2)
summary(model3)
summary(model4)

##TEST ASSUMPTIONS OF MODEL 2
par(mfrow=c(2,2))
plot(model2)
plot(model3)
plot(model4)

###ILLUSTRATE RESULT
##Plot scatterplot for covariate

ggplot(data = data1, aes(length, mass, shape = sex)) + 
  geom_point() +
  theme_bw() +
  labs(title = "Copper Lizards",
       x = "Length (cm)",
       y = "Mass (g)") +
  geom_abline(intercept = -3.808, slope = 3.877) +
  geom_abline(intercept = , slope = )


  
  males <- subset(data1, sex == 1)
  females <- subset(data1, sex == 2)
  
  modelMale <- lm(mass ~ length, data = males)
  modelFemale <- lm(mass ~ length, data = females)