# Import/Read the "Social.csv" data file into R.
# Name and save the data frame as social (all small letters).
social <- read.csv("Social.csv")

# Attach the social data frame.
attach(social)

# Load the library mosaic
library(mosaic)
# Use the function favstats in the mosaic library,
# to obtain summary statistics for Support Network % by Gender.
favstats(Support_Network ~ Gender)

# Load the library ggplot
# ggplot2: Grammar of Graphic - Data Visualization
library(ggplot2)
# Construct a side-by-side boxplots of Support Network % by Gender.
# Give an appropriate title and x-y labels (modify the code below).
box.plot <- ggplot(social, aes(x = Gender, y = Support_Network, fill = Gender)) 
box.plot <- box.plot + geom_boxplot()
box.plot <- box.plot + labs(fill = "Gender")
box.plot <- box.plot + xlab("Gender")
# Add a title to the side-by-side boxplots.
box.plot <- box.plot + ggtitle("Boxplots of Social Support Network % by Gender")
# Center the title.
box.plot <- box.plot + theme_update(plot.title = element_text(hjust = 0.5))
box.plot

# Filter data by a specific value to obtain characteristic of extreme value(s).
# Load the library tidyverse to access its functions.
library(tidyverse)

# %>% is called pipe, which allows us to apply operation to the existing data.
# We will call our data frame social,
# and use %>% to filter data by specific criteria.
social %>% filter(Support_Network < 80)

# We need to turn our long data format into a wide format 
# in order to compare percentages of perceived social support network 
# between males and females within each OECD country.

# Social is the name of our long data frame format.
# We continue using %>% in library tidyverse to make changes to our data frame.
# We will store our changes into a new data frame and name it wide.data
# spread function will create a wide format of our data.
wide.data = social %>% 
                      spread(Gender, Support_Network)
wide.data

# Detach the data frame social since it is a long data format.
detach(social)

# Attach the wide data format since we want to explore this data.
attach(wide.data)

# Create a variable for storing differences in females' and males' social net support%.
Diff_Support_Network = Female - Male

# Update the wide.data frame by adding the diff_Support_Network variable in it.
wide.data<-data.frame(wide.data, Diff_Support_Network)

# See a few observations in this updated wide data format.
head(wide.data)

# Obtain summary statistics for the variable Diff_Support_Network
# We will use favstats function in the library mosaic
favstats(Diff_Support_Network)

# Construct a histogram of the variable Diff_Support_Network
hist(Diff_Support_Network, 
     main = "Histogram of Differences in Percentages of Perceived Social Network Support",
     xlab = "Differences in Perceived Social Network Support %",
     # You can pick a different colors using the "R colours palette.pdf" file in Quercus     
     col = "mistyrose2", 
     labels = TRUE)

# Construct a boxplot of the variable Diff_Support_Network
box.plot <- ggplot(wide.data, aes(x = Diff_Support_Network))
# You can pick a different colors using the "R colours palette.pdf" file in Quercus
box.plot <- box.plot + geom_boxplot(color = "saddlebrown", fill = "peachpuff3")
# Label the x-axis
box.plot <- box.plot + xlab("Differences in Percentages of Perceived Social Network Support Social Network Support")
# Add a title to your plot and put the title in the center
box.plot <- box.plot + theme_update(plot.title = element_text(hjust = 0.5))
# Modify the title by inserting your lastname
box.plot <- box.plot + labs(title = "Boxplot of Data Constructed by You")
box.plot

# Print out sorted data
sort(Diff_Support_Network)

# Print out data values below Q1: Lower Quartile
sort(Diff_Support_Network[Diff_Support_Network < 0])

# Print out data values above Q3: Upper Quartile
sort(Diff_Support_Network[Diff_Support_Network > 4])

# Filter data by a specific value to obtain characteristic of extreme value(s)
wide.data %>% filter(Diff_Support_Network > 10)




