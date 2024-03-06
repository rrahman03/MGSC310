# Problem Set 5

# Question 1, Logistic Regression
# Run the code below to clean the movie data set and split the data into testing and training sets.
# (Note, you will have to install the package purrr to run the code)

library("dplyr")
library("forcats")
library("readr")
library("purrr")
movies <- read_csv("/Users/raneemrahman/Desktop/MGSC_310/datasets/IMDB_movies.csv")

movies_clean <- movies %>%
  mutate(budgetM = budget/1000000, grossM = gross/1000000,
         profitM = grossM - budgetM, blockbuster = ifelse(profitM >
                                                            100, 1, 0), genre_main = as.factor(unlist(map(strsplit(as.character(movies$genres),
                                                                                                                   "\\|"), 1))) %>%
           fct_lump(10), rating_simple = fct_lump(content_rating,
                                                  n = 4)) %>%
  filter(budget < 400000000, content_rating != "", content_rating !=
           "Not Rated") %>%
  mutate(rating_simple = rating_simple %>%
           fct_drop()) %>%
  distinct()

library("rsample")
movies_split <- initial_split(movies_clean)
movies_train <- training(movies_split)
movies_test <- testing(movies_split)


# The variable “blockbuster” in the movies dataframe equals 1 if a movie earns more than $100M USD in profit, and 0 otherwise. 
# Fit a logistic regression model to predict whether a movie is a blockbuster using imdb_score, budgetM, and genre_main as predictors.
# Store this logistic model as movies_logit1 and run the summary command over the fitted logistic model object.
movies_logit1 <- glm(blockbuster ~ imdb_score + budgetM + genre_main,
                     data = movies_train,
                     family = "binomial")
summary(movies_logit1)


# Exponentiate the fitted coefficient vector and print the results.
odds_ratios <- exp(coef(movies_logit1))
print(odds_ratios)


# Interpret the coefficient on genre_mainAnimation using the odds ratio interpretation formula.
coef_animation <- coef(movies_logit1)["genre_mainAnimation"]
odds_ratio_animation <- exp(coef_animation)
print(paste("Odds Ratio for genre_mainAnimation:", round(odds_ratio_animation, 3)))

# Interpret the coefficient on imdb_score using the same odds ratio interpretation formula.
# Is it true that blockbusters get worse IMDB scores?

coef_imdb_score <- coef(movies_logit1)["imdb_score"]
odds_ratio_imdb_score <- exp(coef_imdb_score)
print(paste("Odds Ratio for imdb_score:", round(odds_ratio_imdb_score, 3)))
# No, it is not true that blockbusters get worse imdb scores
# The odds ratio shows a positive relationship between IMDB score and a movie being a blockbuster


# Using the movies_logit1 model generate predicted probabilities for the test and training sets.
# (e.g. use the fitted model to ‘score’ the dataset.)
# Print the top of each of these score vectors using the head() command.
train_pred_probs <- predict(movies_logit1, newdata = movies_train, type = "response")
print("Top of Training Set Predicted Probabilities:")
head(train_pred_probs)

test_pred_probs <- predict(movies_logit1, newdata = movies_train, type = "response")
print("Top of Test Set Predicted Probabilities:")
head(test_pred_probs)


# Create two dataframes, results_train and results_test that each contain the following variables:
# true_class (holding the true blockbuster status), and prob_event (the probability that the film is a blockbuster according to your model).
# Note you will need to change blockbuster to a numeric
movies_train$blockbuster <- as.numeric(movies_train$blockbuster)
movies_test$blockbuster <- as.numeric(movies_test$blockbuster)

results_train <- data.frame(
  true_class = movies_train$blockbuster,
  prob_event = predict(movies_logit1, newdata = movies_train,
                       type = "response"))

results_test <- data.frame(
  true_class = movies_test$blockbuster,
  prob_event = predict(movies_logit1, newdata = movies_test,
                       type = "response"))

# Use the plotROC package to generate two ROC plots,one each for the test and training sets.
# Be sure to label the cutoff probabilities along the ROC lines using the cutoffs.at
# = c(0.9,0.8,0.7,0.5,0.3,0.2,0.1) option as shown in the lecture slides.

library("ggplot2")
library("plotROC")

roc_plot <- ggplot(results_test,
                   aes(m = prob_event, d = true_class)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(0.9,0.8,0.7,0.5,0.3,0.2,0.1)) +
  theme_minimal(base_size = 16) + labs(title = "ROC Plot - Testing Set")

roc_plot <- ggplot(results_train,
                   aes(m = prob_event, d = true_class)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(0.9,0.8,0.7,0.5,0.3,0.2,0.1)) +
  theme_minimal(base_size = 16) + labs(title = "ROC Plot - Training Set")

# Calculate the AUC for the test and training sets using the function calc_auc.
# Explain what AUC measures in your own words.
calc_auc(roc_plot)
