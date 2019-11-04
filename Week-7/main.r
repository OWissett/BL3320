## Question 1
question_1 <- function()
{
  ducks <- c(80, 32) ## Number of ducks in each pond

  res1 <- chisq.test(ducks, p = c(0.5, 0.5))
  res1$expected
  
  ## Reject null hypothesis and accept H1 (X2 = 20.5; p < 0.001). There is a 
  ## significant difference in duck numbers 
}

## Question 2
question_2 <- function()
{
  ducks <- c(80, 32)
  
  res2 <- chisq.test(ducks, p = c(3/4, 1/4))
  res2
  res2$expected
  ## Test is valid
  ## Accept Null hypothesis (X2 = 0.7619; p = 0.3827). No significant difference 
  ## duck density between pond 1 and pond 2.
}

## Question 3
question_3 <- function()
{
  ## Part A
  duck.dat <- data.frame(pond = factor(c(1, 2, 3, 4)),
                         roosting = c(34, 21, 12, 72),
                         feeding = c(62, 3, 14, 68))
  
  duck.mat <- data.matrix(duck.dat)
  
  testA <- chisq.test(duck.mat[,2], correct = F)
  testA
  testA$expected
  ## Test is significant, reject null hypothesis (X2 = 60.28; p < 0.001).
  ## Roosting across all ponds in not equal. There is a pond preferance.
  ## Pond 4 is prefered.
  
  ## Part B
  testB <- chisq.test(rbind(duck.mat[,2],duck.mat[,3]), correct = F)
  testB$expected
  
  ## Reject the null hypothesis (X2 = 21.7; p < 0.001).
  
  ## Part C
  
  
}



