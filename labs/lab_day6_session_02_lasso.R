# ----------------------------------------------------------
# Setup
# ----------------------------------------------------------
set.seed(1818)
options(scipen = 9)

library('tidyverse')
library('rsample')
library('glmnet')
library('glmnetUtils')
library('forcats')

# clean data a bit 
data(mpg)
mpg_clean <- mpg %>% mutate(manufacturer = fct_lump(manufacturer,5),
                            trans = fct_lump(trans,3),
                            class = fct_lump(class,5),
                            displ_sq = displ * displ,
                            displ_cube = displ^3,
                            year_sq = year *  year) %>% 
  select(-model)


mpg_split <- initial_split(mpg_clean, prop = .85)
mpg_train <- training(mpg_split)
mpg_test <- testing(mpg_split)


# ----------------------------------------------------------
# Estimating Lasso Models in R 
# ----------------------------------------------------------

# note cv.glmnet automatically performs 
# k-fold cross-validation 
lasso_mod <- cv.glmnet(hwy ~ .,
                       data = mpg_train,
                       # note alpha = 1 sets Lasso!
                       alpha = 1)


# Note that lasso estimates a series of models, one for 
# every value of lambda -- the amount of shrinkage

# print the two model suggested values of lambda:
print(lasso_mod$lambda.min)
# 
print(lasso_mod$lambda.1se)

# plot how the MSE varies as we vary lambda
plot(lasso_mod)

# to examine the coefficients we must say what value of 
# lambda we want to use. 

# coefficients using lambda.1se
coef(lasso_mod, 
     s = lasso_mod$lambda.1se) %>% 
  round(3)

# coefficients using lambda that minimizes cross-validated error
coef(lasso_mod, 
     s = lasso_mod$lambda.min) %>% 
  round(3)

# put into coefficient vector
lasso_coefs <- tibble(
  `varnames` = rownames(coef(lasso_mod, s = lasso_mod$lambda.1se)),
  `lasso_min` = coef(lasso_mod, s = lasso_mod$lambda.min) %>% 
    round(3) %>% as.matrix() %>% as.data.frame(),
  `lasso_1se` = coef(lasso_mod, s = lasso_mod$lambda.1se) %>% 
    round(3) %>% as.matrix() %>% as.data.frame()
) 
print(lasso_coefs, n = 32)

# install.packages('devtools')
devtools::install_github("jaredlander/coefplot")
library('coefplot')
coefpath(lasso_mod)

# ----------------------------------------------------------
#  Lasso Lab  
# ----------------------------------------------------------
# 1. Estimate a cross-validated lasso regression model predicting city mpg
#    as a function of all the other variables in the dataset EXCEPT highway. 
#    Store this as lasso_mod2
lasso_mod2 <- cv.glmnet(cty ~ .,
                       data = mpg_train,
                       alpha = 1)

# 2. Call the plot function against the lasso_mod2 and describe the plot
plot(lasso_mod2)

# 3. How many variables are selected (e.g. non-zero) when we use lambda.1se? 
print(lasso_mod2$lambda.1se)

# 4. How many variables are selected (e.g. non-zero) when we use lambda.min? 
print(lasso_mod2$lambda.min)

# 5. Run the function coefpath over the lasso model and examine how the  
#    variables shrink as we increase the lambda penalty term 

library('coefplot')
coefpath(lasso_mod2)

# ----------------------------------------------------------
# ElasticNet Model
# ----------------------------------------------------------
enet_mod <- cva.glmnet(hwy ~ .,
                       data = mpg_clean,
                       alpha = seq(0,1, by = 0.05))

plot(enet_mod)

# now enet_mod holds a list with all of the sub models, 
# each with alpha = whatever sequence the model was estimated with

minlossplot(enet_mod, 
            cv.type = "min")


str(enet_mod$modlist)

# Use this function to find the best alpha
get_alpha <- function(fit) {
  alpha <- fit$alpha
  error <- sapply(fit$modlist, 
                  function(mod) {min(mod$cvm)})
  alpha[which.min(error)]
}

# Get all parameters.
get_model_params <- function(fit) {
  alpha <- fit$alpha
  lambdaMin <- sapply(fit$modlist, `[[`, "lambda.min")
  lambdaSE <- sapply(fit$modlist, `[[`, "lambda.1se")
  error <- sapply(fit$modlist, function(mod) {min(mod$cvm)})
  best <- which.min(error)
  data.frame(alpha = alpha[best], lambdaMin = lambdaMin[best],
             lambdaSE = lambdaSE[best], eror = error[best])
}

# extract the best alpha value and model parameters
best_alpha <- get_alpha(enet_mod)
print(best_alpha)
get_model_params(enet_mod)

# extract the best model object
best_mod <- enet_mod$modlist[[which(enet_mod$alpha == best_alpha)]]

# ----------------------------------------------------------
# Lab 2 (time permitting)
# ----------------------------------------------------------

# 6. Estimate an elasticnet regression model predicting city mpg
#    as a function of all the other variables in the dataset EXCEPT highway. 
#    Store this as enet_mod2
enet_mod2 <- cva.glmnet(cty ~ ,.
                        data = mpg_clean,
                        alpha = seq)
# 7. Run the code below to show the cross-validated loss for various levels of 
#    Alpha. What is the optimal alpha? Does that make it more a Lasso or Ridge
#    problem? 

minlossplot(enet_mod2, 
            cv.type = "min")


