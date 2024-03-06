# Problem set 3

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com"))

# Install required packages
install.packages("readr")
library(readr)
install.packages("dplyr")
library(dplyr)
install.packages("forcats")
library(forcats)
install.packages("rsample")
library(rsample)

# Continue with the rest of your code...


# Question 1, Training and Testing Datasets

# Load the IMDB_movies dataset.
# Use the mutate function to create variables grossM and budgetM
# that are budget and gross respectively in units of 1 Million.
# (Similar to how we did so in the lab.)
# Also using the mutate function, create two new variables: log_gross and
# log_budget that are equal to the natural log of gross and budget.
# Store the new data frame as the object movies_clean

setwd("/Users/raneemrahman/Desktop/MGSC_310/datasets")
movies <- read_csv("IMDB_movies.csv") 

movies_clean <- movies %>% 
  mutate(grossM = gross / 1000000,
         budgetM = budget / 1000000,
         log_gross = log(gross),
         log_budget = log(budget))

# Use the function fct_lump_n in the forcats package to create a new variable called
# rating_simple, using content_rating as its source, that explicitly lists the four most
# common factor levels and the rest are given the category “Other”. Add this new variable to
# the movies_clean dataset.

movies_clean <- movies_clean %>% 
  mutate(rating_simple = fct_lump_n(content_rating, n = 4, other_level = "Other"))

# Use the table function to create a two way frequency table of content_rating against
# rating_simple to confirm the mutate function was successful.

table <- table(movies_clean$content_rating, movies_clean$rating_simple)
print(table)

# In your own words, what is a factor variable and why is it used?
# a factor variable can only take on a predefined set of values used to represent data 


# Use the function set.seed() to set your favorite seed using any number of your choice.
# Use the function training() and testing() in the rsample package to split the movies_clean
# dataset into training and testing sets of 75% and 25% each respectively.
# Call these objects movies_train and movies_test

set.seed(32)

split_obj <- initial_split(movies_clean, prop = 0.75)
movies_train <- training(split_obj)
movies_test <- testing(split_obj)

# Explain, in your own words, why we split our dataset into training and testing sets
# it helps to make sure that all data is seen and organized well, it is laso easier to compare different data this way

# In your own words, explain how we will use the training dataset
# the training data is used to make predictions on the new data

# Explain, in your own words, how we will use the testing dataset.
# the testing data set is used to accurately make predictions




# Question 2, Predicting Movie Gross

# Estimate a linear regression model where grossM is the dependent variable and imdb_score
# is the independent variable. Store this object as mod1 and run the summary command against
# this model.
mod1 <- lm(grossM ~ imdb_score, data = movies_train)
summary(mod1)

# Interpret the coefficient for imdb_score, being specific about
# the magnitude of the impact of the variable on gross, and the sign
# (positive or negative).
# the coefficient for imdb_score is 14.63
# the coefficient represents a change in grossM for a one-unit increase in imdb_score
# for every one unit increase in imdb_score, there is a $14.63 million increase in grossM
# positive relationship


# In your own words, what does a p-value in a linear regression model measure?
# p value shows the statistical significance of coefficients end the model
# lower p value is better

# What is the p-value for the coefficient for imdb_score.
# What does this p-value imply about the relationship between imdb_score and gross?
# p-value for imdb_score is <2e-16 
# this is a low p-value, so this shows that there is a statistically significant relationship between IMDb score and gross revenue



