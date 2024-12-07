# Install the openintro package
install.packages("openintro")
# Load the library openintro 
library(openintro)
# Open the birth14 data from openintro library
data("births14")
# Attach the data file
attach(births14)

# Obtain a frequency distribution table of mothers' smoking habits
table(habit)

# Load the tidyverse library
library(tidyverse)
# Drop Na values from the variables weight and habit
# Save the changes into a new dataframe babies
babies <- births14 %>% drop_na(weight, habit)

# Load the library mosaic
library(mosaic)
# Obtain summary statistics for the two groups of mothers
favstats(weight ~ habit, data = babies)

# Construct a side-by-side boxplots of weights of babies by mothers' smoking habit.
# Give an appropriate title and x-y labels (modify the code below).
box.plot <- ggplot(babies, aes(x = habit, y = weight, fill = habit)) 
box.plot <- box.plot + geom_boxplot()
box.plot <- box.plot + scale_fill_manual(values = c("lightgrey", "darkorange"), 
                                         name = "Habit")
box.plot <- box.plot + labs(x = "Mothers' Smoking Habit", 
                            y = "Babies' Weights")
# Center and add a title to the side-by-side boxplots.
box.plot <- box.plot + ggtitle("Boxplots Constructed by You") # LINE 33
box.plot <- box.plot + theme_update(plot.title = element_text(hjust = 0.5))
box.plot

# Employ a t test for two independent samples
t.test(weight ~ habit, var.equal = FALSE, data = babies)
