# This exercise illustrates concept of hypothesis testing and error probability (rejecting Ho when it is true)
# Suppose STA123 course grades are Normally distributed with mean 70 and SD of 5
# Estimate the error probability in a t-test of Ho: mu = 70 verses Ha: mu > 70
# when the underlying population is Normal with mu = 70 and sigma = 5
# Take a random sample of n = 25 and test at alpha = 0.05
# R code modification #1: 
# Enter your seed number inside the function set.seed( ) 
# set.seed() function in R will create reproducible results.
set.seed()

# An experiment for hypothesis testing and error probability interpretation:
# Using the STA123 course grades example, we can conduct an experiment using the following steps:
## 1. Generate a set of (10000) STA123 course grades data with 25 students from the population.
## 2. For each of the generated random sample, conduct a t test.
## 3. Store the observed t test statistics and the p.values in matrix 
## 4. Count the number of p.values that are <= 0.05 (alpha-level) to estimate the probability of error
# Initialize a variable count, to count the number of p.values that are <= 0.05 (alpha-level)
count <- 0
nSim <- 10000 # Simulated number
n <- 25 # Size of random sample (e.g., class size)
mu <- 70 # Population mean
sigma <- 5 # Population SD
HT.Array <- array(0, dim=c(nSim, 2)) # A matrix with 10000 rows and 2 columns
for (i in 1:nSim) {
  Grades <- rnorm(n, mean = mu, sd = sigma)
  t.stat <- t.test(Grades, mu = 70, alternative = "greater")$statistic
  p.value <- t.test(Grades, mu = 70, alternative = "greater")$p.value
  HT.Array[i, ] <- c(t.stat, p.value)
  if (p.value <= 0.05){
    count <- count + 1
  } 
}

head(HT.Array)

# Count the number of observed t tests that have p.value <= 0.05
count
# Obtain the estimated error probability. Compare this value with alpha of 0.05
count/nSim 

# Make column names for the matrix
colnames(HT.Array) <- c("t.Stat", "P.value")
head(HT.Array)

# Convert the matrix into a data frame
HT.Array <- as.data.frame(HT.Array)
# Add another column to this data frame that checks whether the p.values <= 0.05
# If p.value <= 0.05, assign 1, otherwise 0
HT.Array$Small.P.value <- ifelse(HT.Array$P.value <= 0.05, 1, 0)

# Attach the data frame
attach(HT.Array)
names(HT.Array)

# Obtain frequency table 
table(Small.P.value)
# Obtain proportion of samll p.values (p.values < = 0.05)
mean(Small.P.value)

# R code modification #2: 
# Make a change to the main (title) of the plot by adding your last name to it.
library(ggplot2)
hist.plot = ggplot(data = HT.Array) 
hist.plot = hist.plot + geom_histogram(aes(x = t.Stat,
                                           y = after_stat(count / sum(count)),
                                           fill = P.value <= 0.05), 
                                       binwidth = 0.5, 
                                       color = 'black')
hist.plot = hist.plot + scale_fill_manual(values=c("lightgrey", "darkorange"), 
                                          name="P.values <= 0.05",
                                          labels=c("No", "Yes"))
hist.plot = hist.plot + labs(x = "t Statistics", 
                             y = "Proportion")
hist.plot = hist.plot + ggtitle('Simulated t Statistics Conducted by You')
hist.plot + theme_bw() + theme(plot.title=element_text(hjust=0.5))