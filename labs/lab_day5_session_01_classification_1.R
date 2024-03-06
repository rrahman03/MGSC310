
#--------------------------------------------------------
# Why not linear models to estimate classification problems? 
#--------------------------------------------------------
library('ISLR')
library('dplyr')
library('ggplot2')


# load data which has credit card default behavior
data(Default)
head(Default)

# convert whether an individual defaults to binary 0 (no default) or 1 (default)
Default <- 
  Default %>% 
  mutate(default_binary = ifelse(default == "Yes",1,0))

# estimate a linear model using average CC balance to predict default
mod1 <- lm(default_binary ~ balance, 
           data = Default)

summary(mod1)


#--------------------------------------------------------
# Estimating Logistic Regression in R
#--------------------------------------------------------
# make sure to use glm() function! 
# set family = binomial to set logistic function
logit_fit1 <- glm(default ~ student,
                  family = binomial,
                  data = Default)

# generalized linear models

# summary over the model to see the model estimates
summary(logit_fit1)


#--------------------------------------------------------
# Lab 1
#--------------------------------------------------------
# 1. Estimate a logistic regression model predicting 
#    default as a function of student, balance, and income
#    and store this as 'logit_mod2'
logit_mod2 <- glm(default ~ student + balance + income,
                  data = Default,
                  family = "binomial")

# 2. Run the summary command over this logistic model
summary(logit_mod2)

# 3. What does the coefficient on income tell us about the impact of income
#    on the likelihood of default? 

# 4. What does the p-value on income tell us about the coefficient income? 






#--------------------------------------------------------
# Converting logit coefficients to odds ratio
#--------------------------------------------------------

options(scipen = 9)
summary(logit_fit1)

# exponentiate the coefficient vector -> can interpret coefficient 
# as its impact on the odds ratio
exp(logit_fit1$coefficients)



#--------------------------------------------------------
# Lab 2
#--------------------------------------------------------

# 5. Exponentiate the coefficient vector of logit_mod2. 
exp(logit_mod2$coefficients)

# 6. Interpret the impact of being a student on the probability of default using 
#    the odds ratio interpretation. 



