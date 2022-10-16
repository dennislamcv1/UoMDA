### Script to Accompany DAfPS Course 3 - Chi-Squared Tests of Association
### Alton B.H. Worthington
### 2 August 2022

### Let's start by importing the data.

### The data are a sample of the May 1985 Current Population Survey dataset. The
### sample originally appeared in Berndt, E.R. (1991). The Practice of 
### Econometrics. New York: Addison-Wesley

chisq_df <- read.csv("CPS1985.csv", stringsAsFactor = T)

### Summary of the Dataset

summary(chisq_df) ## Notice the factor variables.

### Making Tables

table(chisq_df$occupation, chisq_df$sector) # our first table

table(chisq_df$occupation, chisq_df$union) # another table

### A shortcut: "with"
with(chisq_df, table(occupation, sector)) # works the same, evaluates within DF

with(chisq_df, table(occupation, union))

### Proportions Tables (for later)
?prop.table # checking the help file: we need a table as input

prop.table(with(chisq_df, table(occupation, sector)), margin = 1) # row props
prop.table(with(chisq_df, table(occupation, sector)), margin = 2) # col props

### Chi-squared tests

### Example from slides
### Setting up dataframe w counts
data_df <- data.frame(pubschool = c(155, 30),
                      chartschool = c(45, 19),
                      row.names = c("No Voucher", "Voucher"))
### Renaming variables
colnames(data_df) <- c("Public School", "Charter School")

### Our first chi-squared test
chisq.test(data_df)

mytest <- chisq.test(data_df) ### performing a chi-squared test
summary(mytest) ### viewing the summary
str(mytest) ### viewing the structure of the object

mytest$observed
mytest$expected

### Together, let's try again, but look for association between occupation and
### union in the chisq_df (CPS) data.
### We'll call vectors of data by name: chisq_df$occupation and chisq_df$union.

chisq.test(???) ### We have to replace the question marks with something!


### One final alternative: xtabs
xtabs(~ occupation + union, data = chisq_df) ### Our occupation vs union table

summary(xtabs(~ occupation + union, data = chisq_df)) ### Summaries show a test

mytable <- xtabs(~ occupation + union, data = chisq_df)
summary(mytable)

str(summary(mytable)) ## Note that you don't get the expected and observed!
