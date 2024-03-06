
#--------------------------------------------------------
# Basic Linear Regression
#--------------------------------------------------------
# remove all existing objects in memory
rm(list = ls())

# load these libraries
library('dplyr')
library('ggplot2')


#--------------------------------------------------------
# Formulas in R 
#--------------------------------------------------------
# formulas in R start with the dependent variable on the 
# left hand side (LHS), then by a "~" tilde, following by 
# all the dependent variables you wish to estimate on the
# right hand side (RHS)

# e.g. y ~ x1 + x2

data(mpg)
hwy ~ year + displ + cyl  

mod1 <- lm(hwy ~ year + cyl + displ, 
           data = mpg)

summary(mod1)


#--------------------------------------------------------
# Linear Models using lm()
#--------------------------------------------------------

# estimate a linear model with displacement, and 
# cycl on the RHS, and hwy as the 
# development variable (LHS)
# Use the 'mpg' dataframe to estimate the model
# and store the regression equation as 'mod1'
mod2 <- lm(hwy ~ displ + cyl, 
           data = mpg)

# print out a summary of the linear model
summary(mod2)

# or just view the whole "list" object of 
# the model results
str(mod2)

#--------------------------------------------------------
# Exercises
#--------------------------------------------------------
# 1. Suppose you want to estimate a regression model of 
#    cty mpg on year, displacement and engine cylinders. 
#    What is the R formula equation you would use? 

#formula: cty ~ year + displ + cyl
mod1 <- lm(cty ~ year + displ + cyl, data = mpg)

# 2. Estimate a regression model of city mpg on year, 
#    displacement, and engine cylinders and store this as 'mod3'
mod3 <- lm(cty ~ year + displ + cyl, data = mpg)

# 3. What is the outcome (Y) of this model? 
#city

# 4. What are in inputs (Xs) of this model? 
#year displ cyl


#--------------------------------------------------------
# estimating "prettier" regression output
#--------------------------------------------------------
#install.packages('sjPlot')
library('sjPlot')
#install.packages('sjPlot')
library('tidymodels')
# output a prettier table of results 
# looks very nice in RMarkdown! 
tab_model(mod1)

# output a plot of regression coefficients
plot_model(mod1)

# output a table of coefficients and their p-values, t-stats
tidy(mod1)



#--------------------------------------------------------
# Exercises
#--------------------------------------------------------
# 5. Return to mod3 you estimated. Interpret in words the coefficient for year 
# for each additional year,, city miles per gallon is expected to increase by 0.0712

# 6. Interpret in words the coefficient for displacement
# for each additional unit of displacement, the city miles per gallon are expected to decrease by 1.26

# 7. Interpret in words the coefficient for engine cylinders 
# for each additional cylinder, the city miles per gallon are expected to decreased by 1.21

# 8. What is qualitatively different about engine cylinders that may impact
#    our interpretation of the coefficient for this variable? 
# more cylinders are associated with lower city miles per gallon

# 9. If you finish and still have time, try using 'plot_model()'
#    'tab_model' and 'tidy' on 'mod3' (may need to load/install 
#     the packages tidymodels and sjPlot)









