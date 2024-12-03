# Read the data file EduAttain-OECD2017 into R
# Name the data frame EduAttain
EduAttain <- read.csv("EduAttain-OECD2017.csv")
# attach the data file
attach(EduAttain)

# Obtain the scatterplot of the data
# Load the library ggplot
library(ggplot2)
Plot = ggplot(EduAttain, aes(x=Females_Edu, y=Males_Edu))
Plot = Plot + geom_point(shape=19, color="blue")
Plot = Plot + xlab("Percent Females Educational Attaintment") 
Plot = Plot + ylab("Percent Males Educational Attaintment")
Plot = Plot + ggtitle("Scatterplot of Males vs Females Educational Attainment", 
                      subtitle = "Constructed by You")
Plot = Plot + theme(plot.title=element_text(hjust=0.5), 
                    plot.subtitle = element_text(hjust=0.5))  
Plot

# Install the ggExtra package to add histogram of data on the margins of the plot
install.packages("ggExtra")
# Load the library ggExtra
library(ggExtra)
# Add the histograms to the scatterplot
ggMarginal(Plot, type = "histogram",
           xparams = list(binwidth = 5, fill = "pink"), 
           yparams = list(binwidth = 5, fill = "lightblue"))

# Est. Correlation Coefficient r
correlation<-cor(Females_Edu, Males_Edu)
correlation

# Show the line of best fit (regression equation) on the scatterplot
# First: Fit the linear model: E(Y) = B0 + B1X
# lm in R means linear model
Plot = ggplot(EduAttain, aes(x=Females_Edu, y=Males_Edu))
Plot = Plot + geom_point(shape=19, color="blue") 
Plot = Plot + xlab("Percent Females Educational Attaintment") 
Plot = Plot + ylab("Percent Males Educational Attaintment")
Plot = Plot + ggtitle("Scatterplot of Males vs Females Educational Attainment")
Plot = Plot + theme(plot.title=element_text(hjust=0.5))
Plot = Plot + geom_smooth(method = "lm", colour = "red", se = FALSE)
Plot

# First: Fit the linear model: E(Y) = B0 + B1X
# lm in R means linear model
# Use the function lm () to obtain least squares estimates 
# Estimates: y-intercept (Beta_hat_zero), slope (Beta_hat_1)
# These estimated are regression coefficients: bo, b1
linear.model = lm(Males_Edu ~ Females_Edu)

# Obtain summary of lm 
summary(linear.model)

# Summary statistics for percent females' edu attain
summary(Females_Edu)

# Predictions in R
Females.Edu.80 <- data.frame(Females_Edu = 80)
predict(linear.model, newdata = Females.Edu.80)

# Use the pipe operator in dplyr library
library(dplyr)
EduAttain %>% filter(Females_Edu == "80")

# Obtain residual values
resid_values = resid(linear.model)

# Extract standard error of residuals from linear.model
S = sigma(linear.model)
S

# Residuals are expected to be within 3 SD of their mean (zero)
Lower.Bound = -3*S
Upper.Bound = 3*S
expected.residuals <- data.frame(Lower.Bound, Upper.Bound)
expected.residuals

# Range of residual values given the data at hand
Min.Residual = min(resid_values)
Max.Residual = max(resid_values)
observed.residuals <- data.frame(Min.Residual, Max.Residual)
observed.residuals

# Create a 2 by 2 Panel to display residual plots
par(mfrow=c(2,2))
# Histogram of the residuals 
hist(resid_values, xlab = "Residual Values", main = "Histogram of Residuals")
# Boxplot of the residuals
boxplot(resid_values, ylab = "Residual Values", main = "Boxplot of Residuals")
# Normal Probability Plot of Residuals
qqnorm(resid_values)
qqline(resid_values, col = "red")
# Plot of residuals verses their X-values
plot(Females_Edu, resid_values, 
     ylab = "Residual Values", 
     xlab = "Percent Females Education Attaintment", 
     main = "Residuals v.s. x-values")
abline(h = 0, col = "red", pch = 21, cex = 1.1)
