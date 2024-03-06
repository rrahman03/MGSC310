# Raneem Rahman
# Problem Set 4

#Question 1, Training and Testing Datasets

# a
# Load the IMDB_movies dataset. Run the code below to generate movies_clean which has some transformed variables.
library("readr")
library("dplyr")
library("forcats")

setwd("/Users/raneemrahman/Desktop/MGSC_310/datasets")
movies <- read_csv("IMDB_movies.csv") 

movies_clean <- movies %>%
  mutate(budgetM = budget/1e+06, grossM = gross/1e+06, log_gross = log(gross),
         log_budget = log(budget), log_imdb_score = log(imdb_score)) %>%
  mutate(rating_factor = fct_lump_n(content_rating, n = 4),
         country_factor = fct_lump_n(country, n = 6))

# b
# Use the function set.seed() to set your favorite seed using any number of your choice.
# Use the function training() and testing() in the rsample package to split the movies_clean dataset
# into training and testing sets of 75% and 25% each respectively.
#Call these objects movies_train and movies_test.

library(rsample)

set.seed(42)

split_data <- initial_split(movies_clean, prop = 0.75)
movies_train <- training(split_data)
movies_test <- testing(split_data)

# c
# Execute the code below and show its output.
# The function model.matrix() outputs the matrix used in the regression model.
# If there are five factor levels for ratings, why do only four “one-hot” encoded columns appear?

mod_mat <- model.matrix(grossM ~ rating_factor, 
                        data = movies_train,
                        contrasts.arg = list(rating_factor = contr.treatment(levels(movies_train$rating_factor))))

levels(movies_train$rating_factor)
head(mod_mat, n = 10)

# one of the factor levels is the dummy variable so that is why there is only four
# NOTE:
# I had to change this code because it was giving me this error: 
  # Warning message:
  # In model.matrix.default(grossM ~ rating_factor, data = movies_train,  :
                            # non-list contrasts argument ignored
# I put the error in chat GPT and it showed me how to change it so the rest of my code would work
# I'm not sure why I was having that error

# d
# Estimate a regression model with grossM as the dependent variable imdb_score, rating_factor,
# country_factor, title_year, and budgetM as the independent variables using the training dataset.
# Store this model as mod2.

mod2 <- lm(grossM ~ imdb_score + rating_factor + country_factor + title_year + budgetM,
           data = movies_train)

# e
# Run the summary function against mod2 to show the output from the regression.
summary(mod2)

# f
# Interpret the coefficent on rating_factorR being sure to communicate its magnitude specifically.
# The coefficient on rating_factorR is -48.39099
# This number represents the change for a one-unit increase
# But, since it is negative, there is an estimated decrease of 48.39099

# g
# Estimate a linear model predicting log_gross using the variables log_budget,
# country_factor, title_year and log_imdb_score.
# Store this as mod3 and run the summary command over that fitted model.

mod3 <- lm(log_gross ~ log_budget + country_factor + title_year + log_imdb_score,
           data = movies_train)
summary(mod3)

# h
# Interpret the coefficient on log_imdb_score. Be sure to use the elasticity formula to interpret the coefficient.
# The coefficient is 1.488954
# This shows that there is a positive relationship between log_imdb_score and log_gross
# The elasticity formula makes the value 1.49 which means a 1% increase in log_imdb_score
# is connected to a 1.49% increase in log_gross

# i 
# Return to mod3 you estimated.
# Use this model to generate predictions in the test and training sets.
# Use the sketch of the code below (you will need to alter it)to generate
# a results data frame that holds the predicted and true values from mod3 you estimated.

results_train <- 
  tibble(
    `preds` = exp(predict(mod3, newdata = movies_train))/1000000,
    `true` = movies_train$log_gross,
    `type` = "train"
  )

results_test <- 
  tibble(
    `preds` = exp(predict(mod3, newdata = movies_test))/1000000,
    `true` = movies_test$log_gross,
    `type` = "test"
  )

results_df <- 
  bind_rows(results_train, results_test)

# j 
# Generate two predicted/true scatter plots, one each for the test and train datasets.
# For these, plot the predictions on the y axis and the true values on the x axes.
# Be sure to label your axes and set a range for the axes that shows detail for the most important data
# (e.g. 0 to 200 million)
# (Note, you can set the options fig.width = 8, fig.height = 6 in the top of the code chunk to change the plot size displayed.)
library(ggplot2)

plot_predictions <- function(data, title) {
  ggplot(data, aes(x = true, y = preds)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "blue") +
    labs(title = title,
         x = "True Values (in millions)",
         y = "Predicted Values (in millions)") +
    xlim(0, 200) +
    ylim(0, 200)
}

options(fig.width = 8, fig.height = 6)

plot_train <- plot_predictions(results_df %>% filter(type == "train"), "Predicted vs True Values (Training Set)")

plot_test <- plot_predictions(results_df %>% filter(type == "test"), "Predicted vs True Values (Test Set)")

print(plot_train)
print(plot_test)

# k
# Comment on the plots. How well does your model predict movie gross?
# The points do not follow the blue line closely which means the predicted values are not close to the true values in the training set

# Load ggplot2 library
library(ggplot2)

# Create a scatter plot with regression line
ggplot(data_combined, aes(x = `Happiness score`, y = `Healthy life expectancy`)) +
  geom_point() +  # Add scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  facet_wrap(~Country) +  # Separate plots for each country
  labs(title = "Linear Regression: Healthy Life Expectancy vs. Happiness Score",
       x = "Happiness Score",
       y = "Healthy Life Expectancy")