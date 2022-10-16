### Script to Accompany DAfPS Course 3 - Understanding p-values
### Alton B.H. Worthington

### This script has lots more code for us to run together, but no data to load.

### Loading some packages from tidyverse
library(ggplot2)
library(dplyr)

### Drawing Samples Together

set.seed(12345) # number that gets us all on the same simulations

## Creating an example dataframe
sample <- data.frame(randvar = rnorm(100, # "rnorm" - random draws from the normal distribution, 100 times
                                     mean = 0, # mean of 0
                                     sd = 1)) # standard deviation of 1

hist(sample$randvar) # a histogram of "randvar"

### Here we'll see the other kind of pipe common in R code. The %>% comes from the "magrittr" package
samplemean <- sample %>% summarize(randmean = mean(randvar)) # a dataframe summarizing "sample"

## Creating an example dataframe with more observations
sample2 <- data.frame(randvar = rnorm(1000, # 1000 times
                                     mean = 0, 
                                     sd = 1)) 

hist(sample2$randvar)

sample2mean <- sample2 %>% summarize(randmean = mean(randvar))

## Creating an example dataframe with 5 groups of 100 observations
sample5 <- data.frame(randvar = rnorm(500, # 500 observations total
                                      mean = 0,
                                      sd = 1),
                      draw = rep(c(1:5), # repeat the pattern 1,2,3,4,5
                                 each = 100)) # repeat each element 100 times

sample5mean <- sample5 %>%
  group_by(draw) %>%
  summarize(randmean = mean(randvar)) # summarizing the mean of each draw group

### Creating an example dataframe with 5 groups of 100 observations a different way
sample5b <- data.frame(randvar = c(rnorm(100,
                                         mean = 0,
                                         sd = 1),
                                   rnorm(100,
                                         mean = 0,
                                         sd = 1),
                                   rnorm(100,
                                         mean = 0,
                                         sd = 1),
                                   rnorm(100,
                                         mean = 0,
                                         sd = 1),
                                   rnorm(100,
                                         mean = 0,
                                         sd = 1)),
                       draw = rep(c(1:5), 
                                  each = 100))

sample5bmean <- sample5b %>%
  group_by(draw) %>%
  summarize(randmean = mean(randvar))

### Plotting our simulated samples
ggplot(data = sample, aes(x = randvar)) +
  geom_vline(data = samplemean,
             aes(xintercept = randmean),
             linetype = "dashed",
             size = 1.5) +
  geom_density(size = 2) +
  theme_minimal() +
  labs(title = "One Kernel Density Plot, Normal, Mean 0, SD 1, n = 100")

ggplot(data = sample2, aes(x = randvar)) +
  geom_vline(data = sample2mean,
             aes(xintercept = randmean),
             linetype = "dashed",
             size = 1.5) +
  geom_density(size = 2) +
  theme_minimal() +
  labs(title = "One Kernel Density Plot, Normal, Mean 0, SD 1, n = 1000")

ggplot(data = sample5, aes(x = randvar, group = draw)) +
  geom_vline(data = sample5mean,
             aes(xintercept = randmean),
             linetype = "dashed",
             size = 1) +
  geom_density(size = 2) +
  theme_minimal() +
  labs(title = "Five Kernel Density Plots, Normal, Mean 0, SD 1")

ggplot(data = sample5b, aes(x = randvar, group = draw)) +
  geom_vline(data = sample5mean,
             aes(xintercept = randmean),
             linetype = "dashed",
             size = 1) +
  geom_density(size = 2) +
  theme_minimal() +
  labs(title = "Five Other Kernel Density Plots, Normal, Mean 0, SD 1")

### Simulating differences in random samples, 1000 times

numsims <- 1000 ### Number of simulations
numobs <- 30 ### Number of observations in each simulated sample pair
mean1 <- 0 ### mean of sample 1
mean2 <- 0 ### mean of sample 2
sd1 <- 1 ### standard deviation of sample 1
sd2 <- 1 ### standard deviation of sample 2

meandiffs <- replicate(numsims,
                       mean(rnorm(numobs,
                                  mean1,
                                  sd1)) - 
                       mean(rnorm(numobs,
                                  mean2,
                                  sd2)))

prop.table(table(meandiffs > .3))