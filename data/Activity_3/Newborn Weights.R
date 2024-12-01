# At-term newborns in Canada vary in weight according to, approximately,
# a Normal model, with mean 3500 grams and standard deviation 500 grams.
# Draw a random sample of size 30 from this population distribution.
# R code modification #1: Pick a seed number so that each time you run your sampling, 
# you obtain the same result.
# Enter your seed number in line 8. You can find your seed number in Quercus > Grade page. 
set.seed( )

# Save the results of your random sampling in a variable name newborn.weights
newborn.weights <- rnorm(n = 30, mean = 3500, sd = 500)
newborn.weights

# Construct plots of data
par(mfrow=c(2,1))
boxplot(newborn.weights, 
        horizontal = TRUE,
        main = "Boxplot of \n Weights (in grams) of New Borns")
qqnorm(newborn.weights, 
       main = "Normal Probability Plot of \n Weights (in grams) of New Borns")
qqline(newborn.weights)

# The following code will remove the plots from the plots pane in RStudio
dev.off()

# Load the library mosaic
library(mosaic)
# Obtain summary statistics using the function favstats 
favstats(newborn.weights)

# Replicate. Draw 100 samples of size n = 30 from the population data.
# Find the mean of each random sample of size 30. 
# Store the results in a variable name Xbar
# Xbar: 100 Sample Means for Weights (in grams) of New Borns
Xbar <- replicate(n = 100, mean(rnorm(n = 30, mean = 3500, sd = 500)))

# R code modification #2: Plot the Sampling Distribution of Sample Means for Weights (in grams) of Newborns
# Make a change to line 40. Include your last name in the main (title) argument.
hist(Xbar, 
     main = "Histogram of Sample Means \n Conducted by You", 
     xlab = "Xbar: Sample Means for Total Weights (in grams) of New Borns", 
     col = "lavender")

# Population Mean (Theoretical Mean) is 3500 
mu = 3500
# Mean of Xbar
mean(Xbar)

# Population SD (Theoretical SD) is 500
SD = 500
# SD of Xbar
sd(Xbar)
# Standard error of Xbar (Theoretical SD)
n = 30 
SD/sqrt(n)

# Try simulations for other sample sizes for n = 2, 5, 15, 30, 100, and 300
# With different replication number, n = 1000
xbar.n.2 <- replicate(n = 1000, mean(rnorm(n = 2, mean = 3500, sd = 500)))
xbar.n.5 <- replicate(n = 1000, mean(rnorm(n = 5, mean = 3500, sd = 500)))
xbar.n.15 <- replicate(n = 1000, mean(rnorm(n = 15, mean = 3500, sd = 500)))
xbar.n.30 <- replicate(n = 1000, mean(rnorm(n = 30, mean = 3500, sd = 500)))
xbar.n.100 <- replicate(n = 1000, mean(rnorm(n = 100, mean = 3500, sd = 500)))
xbar.n.300 <- replicate(n = 1000, mean(rnorm(n = 300, mean = 3500, sd = 500)))

# Plot the Sampling Distributions for n = 2, 5, 15, 30, 100, and 300
# Create a multi-panel with 3 rows and 3 column
par(mfrow=c(2, 3))
hist(xbar.n.2)
hist(xbar.n.5)
hist(xbar.n.15)
hist(xbar.n.30)
hist(xbar.n.100)
hist(xbar.n.300)

# Compare Standard Errors (SE) with different sample sizes n
n <- c(2, 5, 15, 30, 100, 300)
n
# Which sample size gives the smallest SE? 
# Obtain standard error of Xbar for n = 2, 5, 15, 30, 100, 300
# Recall: Population SD is 500
SD = 500
SE.Xbar <- c(SD/sqrt(2), SD/sqrt(5), SD/sqrt(15), SD/sqrt(30), SD/sqrt(100), SD/sqrt(300))
SE.Xbar

# Put the results in a data frame to display more clearly
SE.Xbar.diff.n <- data.frame(n, SE.Xbar)
SE.Xbar.diff.n
