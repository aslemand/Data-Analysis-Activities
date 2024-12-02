# This exercise illustrates obtaining a Confidence Interval (CI).
# Obtaining a CI involves three steps:
# 1. Obtain a point estimate for parameter of interest. Note that point estimate is a function of x or your data.
# 2. Find out the sampling distribution of point estimate
# 3. An equal-tail confidence interval with 95% confidence level can be constructed using the 2.5th and 97.5th percentiles of the sampling distribution.
# The exercise:
# Suppose STA123 course grades are Normally distributed.
# Instead of going out to collect data from students, we will simulate (generate) some data for our example. 
# To simulate data, we need to know the population mean and standard deviation of STA123 course grades. 
# Here we assume the mean STA123 course grades is 70 and SD is 5
# R code modification #1: 
# Enter your seed number inside the function set.seed( ) 
# You can find your seed number in Quercus > Grade page
# set.seed() function in R will create reproducible results.
set.seed()

# Let's generate a sample of data with the sample size 150 with the hypothesized parameter values.
Grades <-rnorm(n = 150, mean = 70, sd = 5) 

# Plot the sampled data
par(mfrow=c(1, 3))
hist(Grades)
boxplot(Grades, main = "Boxplot of Grades")
qqnorm(Grades, main = "Normal Q-Q Plot of Grades")
qqline(Grades, col = "red")

# Turn off the graphics since we need to plot another plot later and need to use the entire margin in the Plots pane in R
dev.off()

# Based on the central limit theorem, if the population variance is known, 
# regardless of the shape of the population distribution, 
# sampling distribution of sample man is approximately normally distributed 
# with mean 70 and standard deviation (standard error of the mean) 5/sqrt(150)
# Thus, the 95% CI for the population mean course grade can be constructed 
# using the 2.5% and 97.5% percentile of the normal distribution
xbar <- mean(Grades)
s.e.xbar <- 5/sqrt(150)
ciValues <- qnorm(c(.025, .975), xbar, s.e.xbar)
ciValues
lower_ci <- xbar - 5/sqrt(150) * (-qnorm(.025))
upper_ci <- xbar + 5/sqrt(150) * qnorm(0.975)
print(c(lower_ci, upper_ci))
CIvalues <- data.frame(CI=c('Lower', 'Upper'), Values=c(lower_ci, upper_ci))
CIvalues 

# An experiment for CI interpretation:
# A CI changes each time with a study. If we repeat the same study again and again, 
# (1-alpha)% of the time the obtained confidence intervals would cover the true population parameter value. 
# This can be shown through a simulation study or experiment. 
# Using the STA123 course grades example, we can conduct an experiment using the following steps:
## 1. Generate a set of STA123 course grades data with 150 students from the population
## 2. Calculate the observed sample mean of STA123 course grades and the standard error of xbar
## 3. Calculate the confidence interval
## 4. Check whether the confidence interval contains the population parameter value
## 5. Repeat (1)-(4) 10000 times and count the total number of times that the confidence intervals contain the population value.
## 6. For a 95% CI, one would expect about 9500 times the CIs contain the population value.
# The R code below carries out the experiment. 
# The output shows that the among the 10000 sets of CIs calculated based on the 10000 sets of simulated data, 
# a number of them (this number varies each time we run this experiment) contain the population mean value of 70
# Initialize a variable count, to count the number of CIs that contain the population mean value of 70
count <- 0
nSim <- 10000 # Simulated number
n <- 150 # Size of random sample (e.g., class size)
mu <- 70 # Population mean
sigma <- 5 # Population SD
# Initialize a matrix ciArray to join vectors of interval bounds (lower bound, upper bound) in it
ciArray <- array(0, dim=c(nSim, 2))
for (i in 1:nSim) {
  
  Grades <- rnorm(n, mean = mu, sd = sigma)
  xbar <- mean(Grades)
  s.e.xbar <- sigma/sqrt(n)
  lower.bound <-qnorm(.025, xbar,  s.e.xbar)
  upper.bound <- qnorm(.975, xbar,  s.e.xbar)
  ciArray[i, ] <- c(lower.bound, upper.bound)
  
  if (lower.bound < mu & upper.bound > mu){
    count <- count + 1
  }  
  
}
head(ciArray)
count
count/nSim

# Draw a plot of the first 100 simulated confidence intervals and 
# indicate those which do not contain the true population mean value of 70
# R code modification #2: 
# Make a change to the main (title) of the plot by adding your last name to it
# Initialize the plot
plot(0, 
     xlim = c(min(ciArray[1:100, 1])-0.05, max(ciArray[1:100, 2])+0.05), 
     ylim = c(1, 100), 
     ylab = "Sample", 
     xlab = expression("Interval Estimates for" ~ mu), 
     main = "Simulated Confidence Intervals Conducted by You")

# Identify confidence intervals not containing the mu value
ID <- which(!(ciArray[1:100, 1] < 70 & ciArray[1:100, 2] > 70))
length(ID)

# set up color vector
colors <- rep(gray(0.7), 100)
colors[ID] <- "red"

# draw reference line at mu=70
abline(v = 70, lty = 2)

# add horizontal bars representing the CIs
for(j in 1:100) {
  
  lines(c(ciArray[j, 1], ciArray[j, 2]), 
        c(j, j), 
        col = colors[j], 
        lwd = 2)
  
}