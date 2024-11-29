# Hello :)
# This data set that we will explore is named Home_SCF2013
# It is a csv file.
# However, upon importing it into R, we will rename it as Home
Home <- read.csv("Home_SCF2013.csv")

# Get the variable names in the data frame 
names(Home)

# Get the structure of the variables in the data frame
str(Home)

# Attach the data frame 
# Reason for doing so is to use each variable name as they are named 
attach(Home)

# Open the tidyverse library
## We need the tidyverse package for several reasons.
## We need to Change the levels of each variable from numeric to text format. 
## Why do this?
## We need to know what each numeric value represent in a variable.
## This helps to display results clearly.
library(tidyverse)

# Let's obtain contingency table of Education_Level and Home_Ownership
# Save the the contingency table in "Home.Table"
Home.Table <- table(Education_Level, Home_Ownership)
Home.Table

# Let's make some changes to our data frame.
# We will use the pipe function %>% to apply the changes.
# Let's create a new data frame to store our changes.
# Like this: Home2 <- Home %>% 
# We will use the mutate function to make change to structure of our variables.
# We use the argument as_factor inside the mutate function.
# We are changing our variables' integer formats to factors.
Home2 <- Home %>%
  mutate(Education_Level = as_factor(Education_Level),
         Home_Ownership = as_factor(Home_Ownership))

# We will now recode the levels of our factors for the purpose of clarity.
# We will use the argument fct_recode inside thee mutate function. 
Home2 <- Home2 %>% 
  mutate(Education_Level = fct_recode(Education_Level, 
                                      "No High School" = "1",
                                      "High School" = "2", 
                                      "Some College" = "3", 
                                      "College Degree" = "4"),
         Home_Ownership = fct_recode(Home_Ownership, 
                                     "Yes" = "1", 
                                     "No" = "2"))
# View few lines of Home2 data frame to see the changes made.
head(Home2)

# Let's re-order the levels of our factor levels.
# This is for the purpose of displaying results in order.
# Otherwise, R will display results in alphabetical order.
Home2 <- Home2 %>% 
  mutate(Education_Level = fct_relevel(Education_Level, 
                                       c("No High School",
                                         "High School", 
                                         "Some College", 
                                         "College Degree")), 
         Home_Ownership = fct_relevel(Home_Ownership, 
                                      c("Yes", "No")))
# Detach the data frame Home
detach(Home)

# Obtain contingency table of Education_Level and Home_Ownership
# Save the the contingency table in "Home.Table"
Table <- table(Education_Level, Home_Ownership)
Table

# Attach Home2 data frame
attach(Home2)

# Obtain contingency table of Education_Level and Home_Ownership
# Save the the contingency table in "Table"
Table <- table(Education_Level, Home_Ownership)
Table

# Add Margins to the table
addmargins(Table)

# Calculate Marginal Proportions for Education_Level
Margin.Prop.Edu <- prop.table(margin.table(Table, 1))
Margin.Prop.Edu

# Add margins to the marginal distribution table
addmargins(Margin.Prop.Edu)

# Calculate Marginal Proportions for Home_Ownership
Margin.Prop.Home <- prop.table(margin.table(Table, 2))
Margin.Prop.Home

# Add margins to the marginal distribution table
addmargins(Margin.Prop.Home)

# Calculate Joint Proportions
Joint.Prop <- prop.table(Table)
Joint.Prop

# Add margins to the joint distribution table
addmargins(Joint.Prop)

# Calculate Row Proportions
Row.Prop <- prop.table(Table, 1)
Row.Prop

# Calculate Column Proportions
Col.Prop <- prop.table(Table, 2)
Col.Prop

# To plot the data, we will use the package ggplot2
# ggplot2: grammar of graphics - Data Visualization
# Generally, we need to make sure we have the package installed,
# before we call for its functions.
# Since we have loaded the tidyvers library, 
# ggplot2 library is included in it and it is already loaded.
# That is, we can use the functions in ggplot2 
# But, you can open the ggplot2 library again :)
library(ggplot2)

# Exercise.
# We will construct a side-by-side (clustered) bar chart of the data
# bar.plot is a name where we want to save the plot and its features
# ggplot function will make a canvas, 
# and will make the plot ready using the data set and its variables of interest
bar.plot = ggplot(Home2, aes(x = Education_Level, fill = Home_Ownership))
# We will add the bars to the plot of the data
# As well, we will add the legends and position it to the right-hand side
bar.plot = bar.plot + geom_bar(position = "dodge")
# We will add a label to the x-axis, 
# We will differentiate the bars by filling in the levels of the response variable
# We will add a title and a subtitle to the plot
# And, we will centre the position of both the title and the subtitle
# Modify line 140 with your last-name in the subtitle
bar.plot = bar.plot + labs(xlab = "Education Level", fill = "Home Ownership",
                           title = "Bar Plot of Home Ownership and Education Level", 
                           subtitle = "Constructed by You")
bar.plot = bar.plot + theme(plot.title=element_text(hjust=0.5), 
                            plot.subtitle = element_text(hjust=0.5))
bar.plot

