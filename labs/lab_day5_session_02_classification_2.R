
#--------------------------------------------------------
# Estimating Logistic Regression in R
#--------------------------------------------------------
library('ISLR')
library('tidyverse')
library('rsample')
# load data which has credit card default behavior
data(Default)
head(Default)

default_split <- initial_split(Default, prop = 0.75)
default_train <- training(default_split)
default_test <- testing(default_split)

#--------------------------------------------------------
# Generating Logistic Predictions
#--------------------------------------------------------

logit_fit3 <-  glm(default ~ balance,
                   family = binomial,
                   data = default_train)

summary(logit_fit3)

# probability of default with balance of $1000
top <- exp(logit_fit3$coefficients[1] + 
             logit_fit3$coefficients[2] * 1000)
p_hat_1000 <- top / (1 + top)
p_hat_1000


# probability of default with balance of $1000
top <- exp(logit_fit3$coefficients[1] + 
             logit_fit3$coefficients[2] * 2000)
p_hat_2000 <- top / (1 + top)
p_hat_2000

scores <- predict(logit_fit3,
                  type = "response", 
                  data = default_train)

df <- tibble(
  default_train,
  `pred_prob` = scores,
  `pred_class` = as.factor(ifelse(scores > 0.4,
                                  "Yes","No"))
)
head(df)

#--------------------------------------------------------
# Generating Logistic Predictions
#--------------------------------------------------------
library('yardstick')


results_train <- tibble(
  `true_class` = as.factor(default_train$default),
  `prob_event` =  scores,
  `prob_not_event` = 1 - scores,
  `pred_class` = as.factor(ifelse(scores > 0.4,
                                 "Yes","No"))
)

cm <- conf_mat(results_train, 
               truth = true_class,
               estimate = pred_class)

print(cm)
library(ggplot2)
autoplot(cm, "heatmap")


#--------------------------------------------------------
# Lab 1 
#--------------------------------------------------------
# 1. Estimate a logistic model of default using the dependent 
#    variables income, student, and balance. Store this as logit_mod4
logfit_mod4 <- glm(default ~ income + student + balance,
                   family = binomial,
                   data = Default)

# 2. Generate predicted probabilities (score the model) for the training data
scores <- predict(logfit_mod4,
                       type = "binomial",
                       data = default_train)

# 3. Create a results data frame that holds the true class, the predicted class, 
#    the predicted probability of the event 

results_mod4 <- tibble(
  'probs' = scores,
  'pred_class' = if_else(scores > 0.4, 1,0),
  'true' = default_train,
  'true' = default_train$default,
)

# 4. Generate a confusion matrix using the results data frame 
#    using 0.3 as the cutoff
cm <- conf_mat(results, train,
               truth = TRUE,
               estimate = pred_class)

# 5. How many false positives did your model receive? 
#    (Remember, FP = we predict it to be positive but it's actually negative)


#--------------------------------------------------------
# ROC plots
#--------------------------------------------------------
library('ggplot2')
library('plotROC')

p <- ggplot(results_train, 
            aes(m = prob_event, d = true_class)) + 
  geom_roc(labelsize = 3.5, 
           cutoffs.at = 
             c(0.99,0.9,0.7,0.5,0.3,0.1,0)) +
  theme_minimal(base_size = 16)
print(p)
calc_auc(p)

#--------------------------------------------------------
# Lab 2
#--------------------------------------------------------

# 6. Generate an ROC plot using your logit_fit4 model
roc_plot <- ggplot(results_fit4,
                   aes(m=prob_event, d = true_class)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = 
             c(0.99,0.9,0.7,0.5,0.3,0.1,0)) +
  theme_minimal(base_size = 16)
print(roc_plot)

# 7. Calculate the AUC for this model 

calc_auc(roc_plot)


