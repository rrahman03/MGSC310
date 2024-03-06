#--------------------------------------------------------
# Testing and Training Sets
#--------------------------------------------------------
# Seed is a unique number that the compiler uses in any random
# number generator. Setting the seed lets us reproduce
# any random process on our machine, but not across machines

set.seed(1818)
library('rsample')
library('ggplot2')
library('dplyr')
library('forcats')

data(mpg)

# clean mpg a little
mpg_clean <- 
  mpg %>% 
  mutate(cyl = as.factor(cyl)) %>% 
  mutate(class_factor = fct_lump_n(class, n = 4),
         cyl_factor = fct_lump_n(cyl, n = 3))

# create a split with 75% of the data in the training set
mpg_split <- initial_split(mpg_clean, prop = 0.75)
mpg_train <- training(mpg_split)
mpg_test <- testing(mpg_split)

dim(mpg_train)
dim(mpg_test)


#--------------------------------------------------------
# Predicted-True Plots
#--------------------------------------------------------

# estimate a model
mod1 <- lm(hwy ~ displ + year + class_factor, 
           data = mpg_train)
summary(mod1)


preds_train <- predict(mod1, newdata = mpg_train)
preds_test <- predict(mod1, newdata = mpg_test)

results_train <- 
  tibble(
    `preds` = preds_train,
    `true` = mpg_train$hwy,
    `type` = "train"
    )

results_test <- 
  tibble(
    `preds` = preds_test,
    `true` = mpg_test$hwy,
    `type` = "test"
  )

results_df <- 
  bind_rows(results_train, results_test)

ggplot(results_df, aes(x = true, y = preds)) +
  geom_point(aes(color = type)) + 
  geom_abline(color = "red") +
  facet_wrap(~ type) +
  xlim(10,40) + ylim(10,40) +
  theme_minimal(base_size = 16) + 
  theme(legend.position="bottom")


#--------------------------------------------------------
# Median and Mean Absolute Error
#--------------------------------------------------------
get_medae <- function(true, predictions){
  median(abs(true - predictions))
}

get_medae(results_test$true, results_test$preds)
get_medae(results_train$true, results_train$preds)

get_mae <- function(true, predictions){
  mean(abs(true - predictions))
}

get_mae(results_test$true, results_test$preds)
get_mae(results_train$true, results_train$preds)

#--------------------------------------------------------
# Lab 
#--------------------------------------------------------

# 1. Estimate a model of city mpg on displacement, year, and cyl_factor 
mod2 <- lm(hwy ~ displ + year + cyl_factor, 
           data = mpg_train)
summary(mod2)

# 2. Generate prediction/true plots 
preds_train <- predict(mod2, newdata = mpg_train)
preds_test <- predict(mod2, newdata = mpg_test)

# 3. Calculate MAE in the test and training sets
get_mae <- function(true, predictions){
  mean(abs(true - predictions))
}

get_mae(results_test$true, results_test$preds)
get_mae(results_train$true, results_train$preds)

# 4. What do the pred/true plots and MAE suggest about the model being over 
#    or underfit?

underfit


#--------------------------------------------------------
# Log-Log Regressions
#--------------------------------------------------------
mod2 <- lm(log(hwy) ~ log(displ) + year, 
           data = mpg_train)
summary(mod2)

# we interpret displ here as a 1% change in engine displacement  
# decreases highway mpg by 0.59%. 

  
    
#--------------------------------------------------------
# Feature Engineering - Polynomial Terms 
#--------------------------------------------------------
# true relationship between displacement and hwy mpg is 
# nonlinear. We can approximate this with a squared term or a
# 2nd order polynomial
ggplot(mpg_train, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal(base_size = 16)

# second order polynomial effect 
ggplot(mpg_train, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2)) +
  theme_minimal(base_size = 16)

# third order polynomial effect 
ggplot(mpg_train, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,3)) +
  theme_minimal(base_size = 16)



# Engineer squared feature in displacement 
mpg <-
  mpg %>% 
  mutate(displ_sq = displ * displ)

# have to re-split training and testing
mpg_split <- initial_split(mpg, prop = 0.75)
mpg_train <- training(mpg_split)
mpg_test <- testing(mpg_split)

# estimate new model with squared displacement term 
mod3 <- lm(hwy ~ displ + displ_sq, 
           data = mpg_train)

summary(mod3)

# or can model using ~ poly(xvar, #) in lm equation
mod4 <- lm(hwy ~ poly(displ,2), 
           data = mpg_train)

summary(mod4)

# use plot_model to plot model estimated predictions 
library('sjPlot')
plot_model(mod4, type = "pred", 
           terms = "displ", show.data = TRUE)

#--------------------------------------------------------
# For practice on your own (do not need to complete for the lab)
#--------------------------------------------------------

# 6. Estimate a model predicting hwy as a function of cty mpg and 
#    city mpg squared
mod3 <- lm(hwy ~ displ + displ_sq, 
           data = mpg_train)

summary(mod3)

# 7. Use plot_model to show the estimated nonlinear impact on mpg
library('sjPlot')
plot_model(mod4, type = "pred", 
           terms = "displ", show.data = TRUE)

# 8. Is the impact linear? Increasing at a decreasing rate? 
#    Or increasing at an increasing rate?

Yes the impact is linear, increasing at a decreasing rate



