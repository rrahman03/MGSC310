
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

# change cyl to a factor
mpg_clean <- 
  mpg %>% 
  mutate(cyl_factor = as_factor(cyl),
         class = as_factor(class)) %>% 
  mutate(class_simple = fct_lump(class, n = 5))

# create a split with 75% of the data in the training set
mpg_split <- initial_split(mpg_clean, prop = 0.75)
mpg_train <- training(mpg_split)
mpg_test <- testing(mpg_split)

# check the dimensions of these objects
dim(mpg_train)
dim(mpg_test)

#--------------------------------------------------------
# Generating Predictions in R
#--------------------------------------------------------
# note, can also create factors in the formula equation
mod2 <- lm(hwy ~ displ + factor(year), 
           data = mpg)

preds_train <- predict(mod2, newdata = mpg_train)

preds_test <- predict(mod2, newdata = mpg_test)


# calculate root mean squared error (RMSE)
get_rmse <- function(true, predictions){
  sqrt(mean((true - predictions)^2))
}

# calcualte RMSE in the testing and training sets
get_rmse(mpg_train$hwy, preds_train)
get_rmse(mpg_test$hwy, preds_test)

#-------------------------------------------------------------------------------
# Lab 1
#------------------------------------------------------------------------------
# 1. Estimate a linear model on the training set to predict hwy mile per
#    gallon as a function of cylinders (as a factor variable, e.g. cyl_factor), 
#    and displacement. 
mod4 <- lm(hwy ~ cyl_factor + displ, data = mpg_train )

# 2. What is the p-value for displacement? 
summary(mod4)

# 3. Communicate in your own words, what the p-value for displacement means 
#    for the model you've built

# 4. Generate predictions in the test and training sets
preds_train <- predict(mod4, newdata = mpg_train)
preds_test <- predict(mod4, newdata = mpg_test)

# 5. Use the function get_rmse to calculate RMSE in the test and training sets
get_rmse(mpg_train$hwy, preds_train)
get_rmse(mpg_test$hwy, preds_test)


#--------------------------------------------------------
# Working with Binary/Discrete Variables
#--------------------------------------------------------
summary(lm(cty ~ year + class_simple, 
           data = mpg_train))

# to see the base level of the factor run the levels command
# the base level is the first item listed in the vector
levels(mpg_train$class_simple)

# you can "relevel" factors to reset the base level
summary(lm(cty ~ year + 
             relevel(class_simple, ref = "suv"), 
           data = mpg_train))


#--------------------------------------------------------
# Lab 2
#--------------------------------------------------------
# 6. Interpret the coefficient on cyl_factor in the model you built above

summary(lm(cty ~ year + cyl_factor, data = mpg_train))

# 7. What does the p-value of cyl imply about the model? 
#    (Note this one is tricky! Think about it if you can)
# the p-value measures the coeffecient and the base level

# 8. Use the 'relevel' command to change the base level of the factor cyl
#     to another level and re-estimate the model. 
#     Interpret one of the new coefficients. 
summary(lm(cty ~ year + relevel(cyl_factor, ref = "6"), data = mpg_train))
        
        
        
        
