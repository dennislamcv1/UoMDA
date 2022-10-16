### Script to Accompany DAfPS Course 3 - One-Way ANOVA
### Alton B.H. Worthington

ANOVA1_df <- read.csv("Oneway1.csv")
ANOVA2_df <- read.csv("Oneway2.csv")
DATA2 <- read.csv("DATA2export.csv")

### Example from slides: ANOVA1_df data

aov(TestScore ~ Group, # this is our model: TestScore is outcome, Group is group
    data = ANOVA1_df) # the data are in ANOVA1_df
                      # we can rely on the rest of the defaults for now

anova1 <- aov(TestScore ~ Group, # assign it a name
              data = ANOVA1_df)

summary(anova1) # now we check the summary!

### We can try it again with ANOVA2_df together
### NEW CODE GOES BELOW


### One more: revisiting "DATA2" from Video 2
### Let's set up a test where the outcome is Var1 and the grouping variable is Group
anovadata2 <- aov(???) # what would be put here?

summary(anovadata2) # Checking the summary

### Additional analysis: Tukey's Honest Significant Differences

TukeyHSD(anova1,
         ordered = T,
         conf.level = 0.95)

TukeyHSD(anovadata2,
         conf.level = 0.95)

### Retry the DATA2 ANOVA models on Var2 and Var3!
