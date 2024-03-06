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


mpg_split <- initial_split(mpg_clean, prop = .8)
mpg_train <- training(mpg_split)
mpg_test <- testing(mpg_split)

# ----------------------------------------------------------
# Estimating Ridge Models in R 
# ----------------------------------------------------------

# estimate a Ridge model using glmnet
# note if you get an error make sure you 
#  have loaded glmnetUtils
ridge_mod <- cv.glmnet(hwy ~ .,
                       data = mpg_train %>% select(-cty),
                       # note alpha = 0 sets ridge!  
                       alpha = 0)

# print the two model sugegsted values of lambda:

print(ridge_mod$lambda.min)
#
print(ridge_mod$lambda.1se)

# print coefficient using lambda.min
coef(ridge_mod, s = ridge_mod$lambda.min) %>% 
  round(3)

# print coefficient using lambda.1se
coef(ridge_mod, s = ridge_mod$lambda.1se) %>% 
  round(3)

# put into coefficient vector
ridge_coefs <- tibble(
  `varnames` = rownames(coef(ridge_mod, s = ridge_mod$lambda.1se)),
  `ridge_min` = coef(ridge_mod, s = ridge_mod$lambda.min) %>% 
    round(3) %>% as.matrix() %>% as.data.frame(),
  `ridge_1se` = coef(ridge_mod, s = ridge_mod$lambda.1se) %>% 
    round(3) %>% as.matrix() %>% as.data.frame()
) 

print(ridge_coefs, n = 31)

# use the plot function to see the MSE
# path as we vary lambda (the amount of penalization)
plot(ridge_mod)

### examine coefficient shrinkage path
# note may need to install devtools first
# install.packages('devtools')
devtools::install_github("jaredlander/coefplot")
library('coefplot')
coefpath(ridge_mod)

# ----------------------------------------------------------
# Ridge Lab 
# ----------------------------------------------------------

# 1. What is the difference between the functions cv.glmnet and glmnet?
# both estimate a lasso or ridge model
# cv.glment automatically performs cross-validation to select optimal lamda

# 2. Estimate a cross-validated ridge regression model predicting city mpg
#    as a function of all the other variables in the dataset EXCEPT highway. 
#    Store this as ridge_mod2
ridge_mod2 <- cv.glmnet(cty ~ .,
                        data = mpg_train %>% select(-hwy),
                        alpha = 0)

# 3. Call the plot function against the estimated model and describe the plot
plot(ridge_mod2)

# 4. What do the two vertical dashed lines represent? 
# lambda.min line 
# lambda.1se line

# 5. Call the print function against the ridge coefficient vector specifying lambda.1se
print(coef(ridge_mod2, s = ridge_mod$lambda.1se) %>% 
        round(3))

# 6. Call the print function against the ridge coefficient vector specifying lambda.min
print(coef(ridge_mod2, s = ridge_mod$lambda.min) %>% 
        round(3))

# 7. Why do we have different coefficients for lambda.1se and labda.min? 
# the two lamdba values are based on different critera

# 8. (time permitting) 
#    Install the package coefplot and call the function coefpath 
#    against the ridge model. Describe the plot
library('ceofplot')

