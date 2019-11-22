######################
## Bacterial growth ##
######################

## Load ggplot2.0
library(ggplot2)

## Load data from CSV file
growth.dat <- read.csv("growth.csv", sep = ',', header = T)

## Plot the data to provide estimates for the NLS starting vals
ggplot(data = growth.dat, aes(x = time, y = cells)) +
  geom_point() +
  geom_hline(yintercept = (max(growth.dat$cells) - min(growth.dat$cells)) / 2) +
  theme_bw()

# midx ~ 14

## Define the formula used in the NLS
growth.formula <- function(L, K, time, midx)
{
  tmp = L / (1+exp(-K*(time-midx)))
  return(tmp)
}
# midx - X-value of the sigmoid midpoint
# L - y-assymptote
# K - curves gradient

## Starting values used in the NLS
start.vals <- list(midx = 14,
                   L = 5701,
                   K = 1)

## Growth model
growth.nls <- nls(cells ~ growth.formula(L, K, time, midx),
                  data = growth.dat,
                  start = start.vals)


x.seq <- c(1:24)
growth.pred <- predict(growth.nls, list(time = x.seq), se.fit = T)

growth.pred.frame <- data.frame(time = x.seq, cells = growth.pred)

## NLS formula final
get.cells <- function(x)
{
  tmp = coef(growth.nls)[[2]] / (1+exp(-coef(growth.nls)[[3]]*(x-coef(growth.nls)[[1]])))
  return(tmp)
}

## Final plot

ggplot(data = growth.dat, aes(x = time, y = cells)) +
  geom_point() +
  geom_line(data = growth.pred.frame, aes(x = time, y = cells)) +
  theme_classic() + 
  labs(title = "Growth of Bacteria over a Day",
       x = "Time (Hours)",
       y = "Cells") +
  lims(x = c(0, 24),
       y = c(0, 6000)) +
  annotate("segment", 
           x = coef(growth.nls)[[1]], 
           xend = coef(growth.nls)[[1]],
           y = 0, 
           yend = get.cells(coef(growth.nls)[[1]]), 
           colour = "red", 
           linetype = 2) +
  annotate("segment", 
           x = 0, 
           xend = coef(growth.nls)[[1]],
           y = get.cells(coef(growth.nls)[[1]]), 
           yend = get.cells(coef(growth.nls)[[1]]), 
           colour = "red", 
           linetype = 2) +
  annotate("text", y = 2000, x = 17, label = "50% Max Cells: ") +
  annotate("text", y = 2000, x = 22, label = "13.83 Hours")








