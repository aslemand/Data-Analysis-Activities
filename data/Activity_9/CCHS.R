# Read CCHS data file into R
CCHS <- read.csv("CCHS.csv")

# Variables in the data file
names(CCHS)

# Structure of the data
str(CCHS) 

# Open the tidyverse library
library(tidyverse)

# Obtain frequency distribution of the data
CCHS %>% 
  count(Mental_Health)

# Convert any variable with the character structure to numeric 
CCHS <- CCHS %>% mutate_if(is.character, as.numeric)

# Obtain frequency table for the perception of mental health
# Note that . are converted to NA
CCHS %>% 
  count(Mental_Health)

# Obtain frequency table for the age variable
## 1 = 12-14, 2 = 15-17, 3 = 18-19, 4 = 20-24, 5 = 25-29, 6 or more are 30 years of age or older 
CCHS %>% 
  count(Age)

# Obtain frequency table for the gender variable
## 1 = Male, 2 = Female
CCHS %>% 
  count(Gender)

# Drop NA values (missing information) from the data file
## mental health variable only had NAs 
CCHS.2 <- CCHS %>% drop_na(Mental_Health)

# For the purpose of our analysis, filter the data file by a given age range
# We will consider youths ages between 15 to 29
## 2 = 15-17, 3 = 18-19, 4 = 20-24, 5 = 25-29
CCHS.3 <- CCHS.2 %>% filter(Age > 1 & Age <= 5)

# Install the janitor package in R
# The function tabyl() in this package produces tabulations and cross-tabulations, 
# which can be adorned or modified with helper functions. 
# It displays percents, proportions, counts, etc.
install.packages("janitor")
# Open the janitor library
library(janitor)

# Obtain contingency table of age and perception of mental health
CCHS.3 %>%                                    # data frame
  tabyl(Age, Mental_Health) %>%               # cross-tabulate counts of two columns
  adorn_totals(where = c("row", "col")) %>%   # add a total row, add a total column
  adorn_percentages(denominator = "row") %>%  # convert to proportions with row denominator
  adorn_pct_formatting() %>%                  # convert proportions to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age",
    col_name = "Mental Health Perception")

# Obtain contingency table of gender and perception of mental health
CCHS.3 %>%                                    # data frame
  tabyl(Gender, Mental_Health) %>%            # cross-tabulate counts of two columns
  adorn_totals(where = c("row", "col")) %>%   # add a total row, add a total column
  adorn_percentages(denominator = "row") %>%  # convert to proportions with row denominator
  adorn_pct_formatting() %>%                  # convert proportions to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Gender",
    col_name = "Mental Health Perception")

# Create a new variable and name it Positive.Mental.Health
# Combine 1, 2, and 3 levels of perception of mental health into one level as "Yes", 
# and combine 4 and 5 levels into one level as "No".
# Store the result into a new data frame CCH.4 
# that includes this new variable and the CCHS.3 variables. 
# CCHS.4 will have 4 variables in it.
CCHS.4 <- CCHS.3 %>%
  mutate(Positive.Mental.Health = case_when(Mental_Health <= 3 ~ "Yes", 
                                            Mental_Health >= 4 ~ "No"))

# Use the mutate function to make change to structure of our variables.
# Use the argument as_factor inside the mutate function.
# Change the format of the variables from e.g., integer to factors
CCHS.5 <- CCHS.4 %>%
  mutate(Age = as_factor(Age),
         Gender = as_factor(Gender),
         Mental_Health = as_factor(Mental_Health), 
         Positive.Mental.Health = as_factor(Positive.Mental.Health))

# recode the levels of the factors for the purpose of clarity.
# Use the argument fct_recode inside thee mutate function. 
CCHS.5 <- CCHS.5 %>% 
  mutate(Age = fct_recode(Age,
                          "15-17" = "2", 
                          "18-19" = "3",
                          "20-24" = "4", 
                          "25-29" = "5"),
          Gender = fct_recode(Gender, 
                              "Male" = "1",
                              "Female" = "2"),
         Mental_Health = fct_recode(Mental_Health, 
                                    "Excellent" = "1", 
                                    "Very Good" = "2",
                                    "Good" = "3",
                                    "Fair" = "4",
                                    "Poor" = "5"))

# View few rows of the CCHS.5 data frame to notice the changes made.
head(CCHS.5)

# re-level the factor levels to display levels in logical order.
# Otherwise, R will display results in alphabetical order.
CCHS.5 <- CCHS.5 %>% 
  mutate(Age = fct_relevel(Age,
                          c("15-17", "18-19", "20-24", "25-29")),
         Mental_Health = fct_relevel(Mental_Health, 
                                    c("Excellent", 
                                      "Very Good", 
                                      "Good", 
                                      "Fair", 
                                      "Poor")),
         Positive.Mental.Health = fct_relevel(Positive.Mental.Health, 
                                              c("Yes", "No")))

# Obtain contingency table of age and perception of mental health
CCHS.5 %>%                                    # data frame
  tabyl(Age, Mental_Health) %>%               # cross-tabulate counts of two columns
  adorn_totals(where = c("row", "col")) %>%   # add a total row, add a total column
  adorn_percentages(denominator = "row") %>%  # convert to proportions with row denominator
  adorn_pct_formatting() %>%                  # convert proportions to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age",
    col_name = "Mental Health Perception")

# Obtain contingency table of age and positive perception of mental health
CCHS.5 %>%                                    # data frame
  tabyl(Age, Positive.Mental.Health) %>%      # cross-tabulate counts of two columns
  adorn_totals(where = c("row", "col")) %>%   # add a total row, add a total column
  adorn_percentages(denominator = "row") %>%  # convert to proportions with row denominator
  adorn_pct_formatting() %>%                  # convert proportions to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age",
    col_name = "Positive Perception of Mental Health")

# Obtain contingency table of gender and perception of mental health
CCHS.5 %>%                                    # data frame
  tabyl(Gender, Mental_Health) %>%            # cross-tabulate counts of two columns
  adorn_totals(where = c("row", "col")) %>%   # add a total row, add a total column
  adorn_percentages(denominator = "row") %>%  # convert to proportions with row denominator
  adorn_pct_formatting() %>%                  # convert proportions to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Gender",
    col_name = "Mental Health Perception")

# Obtain contingency table of gender and positive perception of mental health
CCHS.5 %>%                                    # data frame
  tabyl(Gender, Positive.Mental.Health) %>%      # cross-tabulate counts of two columns
  adorn_totals(where = c("row", "col")) %>%   # add a total row, add a total column
  adorn_percentages(denominator = "row") %>%  # convert to proportions with row denominator
  adorn_pct_formatting() %>%                  # convert proportions to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Gender",
    col_name = "Positive Perception of Mental Health")

attach(CCHS.5)

# Create a contingency table and name it cross.tab (cross tabulation of two variables)
# Obtain conditional percentages 
cross.tab <- CCHS.5 %>% 
  count(Age, Positive.Mental.Health) %>%
  group_by(Age) %>%
  mutate(Percentage = prop.table(n)*100)

# Convert proportion table to data frame
cross.tab <- data.frame(cross.tab)

# Bar Plot of Gender and Positive Mental Health Perception
bar.plot <- ggplot(data = cross.tab, aes(x = Age, y = Percentage, 
                                        fill = Positive.Mental.Health))
bar.plot <- bar.plot + geom_bar(stat = "identity", position = "dodge") 
bar.plot <- bar.plot + scale_fill_manual(values = c("orange", "grey"), 
                                         name = "Positive Mental Health Perception")
bar.plot <- bar.plot + scale_y_continuous(breaks = seq(0, 100, 10))
bar.plot <- bar.plot + labs(y = "Percentages within Age", 
                            title = "Conditional Distribution of Positive Perception of Mental Health on Age")
bar.plot <- bar.plot + theme_bw()
bar.plot <- bar.plot + theme(plot.title=element_text(hjust=0.5))
bar.plot

# Perform chi-square test of independence
# Ho: There is no association between perception of mental health and age
# Ha: There is an association between perception of mental health and age
Table <- table(CCHS.5$Age, CCHS.5$Positive.Mental.Health)
# Name each of the categorical variable in the table
names(dimnames(Table))<-c("Age","Positive Mental Health Perception")
addmargins(Table)
chisq.test(Table, correct = FALSE)

# Obtain expected cell counts from the chi-square test of independence
chisq.test(Table)$expected

# Obtain the adjusted standardized residuals
chisq.test(Table)$stdres

# Create a contingency table and name it crosstab (cross tabulation of two variables)
# Obtain conditional percentages 
crosstab <- CCHS.5 %>% 
  count(Gender, Positive.Mental.Health) %>%
  group_by(Gender) %>%
  mutate(Percentage = prop.table(n)*100)

# Convert proportion table to data frame
crosstab <- data.frame(crosstab)

# Bar Plot of Gender and Positive Mental Health Perception
bar.plot <- ggplot(data = crosstab, aes(x = Gender, y = Percentage, 
                                        fill = Positive.Mental.Health))
bar.plot <- bar.plot + geom_bar(stat = "identity", position = "dodge") 
bar.plot <- bar.plot + scale_fill_manual(values = c("orange", "grey"), 
                                        name = "Positive Mental Health Perception")
bar.plot <- bar.plot + scale_y_continuous(breaks = seq(0, 100, 10))
bar.plot <- bar.plot + labs(y = "Percentages within Gender", 
                            title = "Conditional Distribution of Positive Perception of Mental Health on Gender")
bar.plot <- bar.plot + theme_bw()
bar.plot <- bar.plot + theme(plot.title=element_text(hjust=0.5))
bar.plot

# Perform chi-square test of independence
# Ho: There is no association between perception of mental health and gender
# Ha: There is an association between perception of mental health and gender
Table <- table(CCHS.5$Gender, CCHS.5$Positive.Mental.Health)
# Name each of the categorical variable in the table
names(dimnames(Table))<-c("Gender","Positive Mental Health Perception")
addmargins(Table)
chisq.test(Table, correct = FALSE)

# Obtain expected cell counts from the chi-square test of independence
chisq.test(Table)$expected

# Obtain the adjusted standardized residuals
chisq.test(Table)$stdres

# Find 95% confidence interval for the 
# difference between population proportions of males and females who perceived having positive mental health 
addmargins(Table)
prop.test(c(7907, 7968), c(8566, 9099), correct=FALSE)
names(prop.test(c(7907, 7968), c(8566, 9099), correct=FALSE))
prop.test(c(7907, 7968), c(8566, 9099), correct=FALSE)$statistic
sqrt(prop.test(c(7907, 7968), c(8566, 9099), correct=FALSE)$statistic)

# Sample relative risk
est.p1 = 7907/8566
est.p1
est.p2 = 7968/9099
est.p2
sample.relative.risk = est.p1/est.p2
sample.relative.risk

# Estimated Odds Ratio
est.p1 = 7907/8566
est.p1
est.p2 = 7968/9099
est.p2
est.odd1 = (0.9230679 / (1 - 0.9230679))
est.odd1
est.odd2 = (0.8757006 / (1 - 0.8757006))
est.odd2
est.Odds.Ratio = est.odd1/est.odd2
est.Odds.Ratio

# Obtain contingency table of age and positive perception of mental health by gender
CCHS.5 %>%                                    
  tabyl(Age, Positive.Mental.Health, Gender) %>%            
  adorn_totals(where = c("row", "col")) %>%   
  adorn_percentages(denominator = "row") %>%  
  adorn_pct_formatting() %>%                  
  adorn_ns(position = "front") %>%            
  adorn_title(                               
    row_name = "Age",
    col_name = "Positive Mental Health Perception")

# Create a contingency table and name it crosstab1 
# This is a three-way table (three variables used: Age, Gender, Positive Mental Health)
crosstabl <- CCHS.5 %>% 
  count(Age, Gender, Positive.Mental.Health) 
# Convert crosstabl object into a data frame
crosstabl <- data.frame(crosstabl)
# Convert counts into conditional percentages: Positive Perception of Mental Health on Age by Gender
crosstabl <- mutate(group_by(crosstabl, Age, Gender), Percent = n / sum(n) * 100)

# Bar Plots of Age and Positive Mental Health Perception by Gender
bar.plot <- ggplot(data = crosstabl, aes(x = Age, y = Percent, 
                                         fill = Positive.Mental.Health))
bar.plot <- bar.plot + geom_bar(stat = "identity", position = "dodge") 
bar.plot <- bar.plot + scale_fill_manual(values = c("orange", "grey"), 
                                         name = "Positive Mental Health Perception")
bar.plot <- bar.plot + scale_y_continuous(breaks = seq(0, 100, 10))
bar.plot <- bar.plot + labs(y = "Percentages within Age", 
                            title = "Conditional Distribution of Positive Perception of Mental Health on Age by Gender", 
                            subtitle = "Constructed by You") # LINE 307
bar.plot = bar.plot + facet_wrap(~ Gender, scales = "free_x")
bar.plot <- bar.plot + theme_bw()
bar.plot <- bar.plot + theme(plot.title=element_text(hjust=0.5), 
                             plot.subtitle = element_text(hjust=0.5))
bar.plot



