# Read/Import the data file "Volunteering.csv" into R
# Name the data frame as volunteering (note: R is case-sensitive)
volunteering <- read.csv("Volunteering.csv")

# Attach the data frame in R
attach(volunteering)

# Construct plots of data
par(mfrow=c(2,1))
boxplot(group, 
        horizontal = TRUE,
        main = "Boxplot of \n Total Number of Group Participation")
qqnorm(group, 
       main = "Normal Probability Plot of \n Total Number of Group Participation")
qqline(group)

# The following code will remove the plots from the plots pane in RStudio
dev.off()

# Obtain summary statistics
# Load the library mosaic
library(mosaic)
# Use the function favstats in the mosaic package to obtain summary statistics
favstats(group)

# R code modification #1: Obtain sampling Distribution of Sample Means
# Pick a seed number so that each time you run your sampling,
# you will obtain the same result.
# Enter your seed number in line 30.
set.seed( )

# Let's treat our data as population data.
# Draw 60 samples of size n = 50 from the population data.
rep = 60 # Number of replication
n = 50  # Each sample size in the replication
# Store the results in a variable name Xbar
# Xbar: 60 Sample Means for Total Number of Group Participation
Xbar <- replicate(rep, mean(sample(group, n)))

# R code modification #2: Plot the Sampling Distribution of Sample Means for Total Number of Group Participation
# Make a change to line 43. Include your last name in the main (title) argument.
hist(Xbar, 
     main = "Histogram of Sample Means \n Conducted by You", 
     xlab = "Xbar: Sample Means for Total Number of Group Participation", 
     col = "lavender")

# Obtain summary statistics for the variable Xbar
library(mosaic)
favstats(Xbar)

# Mean of Xbar
mean(Xbar)
# Population Mean (Theoretical Mean)
mean(group)
# SD of Xbar
sd(Xbar)
# Population SD 
sd(group)
# Standard error of Xbar (Theoretical SD)
n = 50
sd(group)/sqrt(n)
