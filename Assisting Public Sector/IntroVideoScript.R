### This is just a little script used to create objects and demo 
### screenshots for the "Hello" recording of "Introduction to Inferential
### Statistical Analysis in R".
### Alton B.H. Worthington
### 2 August 2022

### Creating Object
example_df <- data.frame(a = rnorm(1000,0,1),
                          b = rbinom(1000, 1, .4),
                          c = c(rnorm(500,0,5),rnorm(500,2,5)))

### Example Tests

### A t-test
myttest <- t.test(x = example_df$a,
                  y = example_df$c)
myttest

## Another way, with "with"
with(example_df, t.test(a, c))

### Opening a help file
?t.test

### Looking at the p-value alone
myttest$p.value

### Importing a csv
imported_df <- read.csv("exampledata.csv")

### Another comment here so that students who pause the video to read this in 
### detail get a special surprise. (And those who checked the code later get
### one, too.)