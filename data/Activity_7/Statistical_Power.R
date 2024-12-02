# Suppose it is claimed that the mean STA123 course grade is 70.
# The population SD is 4 (suppose sigma is known). 
# Suppose that the population distribution of STA123 course grades is Normal. 
# Suppose we test Ho: mu = 70 v.s. Ha: mu > 70
# Suppose the true mu value is something else (i.e, 68).
# So Ho (the null hypothesis) is wrong. 
# How likely is that to happen? This is power of the test.
# In R, the function is: power.t.test 
# The argument delta in this function is the difference between null and true mean.
# We also need a sample size. 
# Let's consider various sample sizes and their effect on power.
# We will examine the relationship between power and sample sizes 
# for different mean values (possible true mu values).
# Make a sequence of sample sizes going up by 10
sample.sizes = seq(from = 10, to = 100, by = 10)
# Consider a sequence of mean values going up by 0.5
means = seq(from = 66, to = 68, by = 1)

# Make a combination of true means with the different sample sizes
# Load the library tidyverse
library(tidyverse)
# Use the function crossing in tidyr to make combinations of variables
combos = crossing(mean = means, n = sample.sizes)
# Display combos
combos

# Recall that we test Ho: mu = 70 vs Ha: mu < 70
# Calculate power for different "true" mean values
# Save the results for different combinations of means and sample sizes
results = with(combos, power.t.test(n = n, delta = 70-mean, sd = 4, 
                                    alternative = "one.sided", type = "one.sample"))

# Make a table, indicating, n, mean, and power
my_data = tibble(mean = factor(combos$mean), n = combos$n, power = results$power)
# Display data
my_data

# Plot the power curve for different true mean values as n sample size increases
power.curve = ggplot(my_data, aes(x = n, y = power, colour = mean)) +
  geom_point() + geom_line() +
  geom_hline(yintercept = 1, linetype="dashed") + 
  xlab("sample size") +
  # Make a change to the main (title) of the plot by adding your last name to it
  ggtitle('Relationship Between Power and Sample Sizes for Different Mean Value Constructed by You') +
  theme(plot.title=element_text(hjust=0.5))
power.curve

