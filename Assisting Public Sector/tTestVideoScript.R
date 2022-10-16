### Script to Accompany DAfPS Course 3 - t-tests of difference in means
### Alton B.H. Worthington

### Let's load some objects into memory
load("Video2Workspace.Rdata")

### If the workspace opened properly, there should be 3 data items in the
### environment: DATA1, DATA2, and DATA3.
### We'll load 2 more datasets

unequal_df <- read.csv("unequalttest.csv")
paired_df <- read.csv("pairedttest.csv")

### Examining some of our data
hist(DATA1$Var1) # Base R Graphics historgram
hist(DATA1$Var2) # Base R Graphics historgram

### t.test example with 2 groups
t.test(outcome ~ Group, # model is outcome split by groups
       data = DATA3, # the data are in the DATA3 dataframe
       alternative = "two.sided", # a "two-tailed" test - difference in either direction
       mu = 0, # null hypothesis of no difference
       paired = F, # not paired samples (default)
       var.equal = F, # unequal population variance (default)
       conf.level = 0.95) # 95% confidence intervals on mean difference estimate

t.test(outcome ~ Group, # model is outcome split by groups
       data = DATA3)

mytest <- t.test(outcome ~ Group,
                 data = DATA3)

str(mytest)

### t.test example with paired data and two-vector specification
summary(paired_df)

t.test(paired_df$Pre, # first vector/variable
       paired_df$Post, # second vector/variable
       alternative = "two.sided", # two-tailed test
       mu = 0, # null hypothesis of mean difference of 0
       paired = T) # paired samples ('before-and-after')

t.test(paired_df$Pre, paired_df$Post, paired = T)

### Final notes: Grouping variable must have only 2 levels!
summary(DATA2)

t.test(Var1 ~ Group, # test difference in Var1 by Group (which has 3 levels)
       data = DATA2)
### We get an error!

### Try some more t-tests with the data in the unequal_df dataframe (for two-
### variable specification) or the DATA1 dataframe (for formula specification)