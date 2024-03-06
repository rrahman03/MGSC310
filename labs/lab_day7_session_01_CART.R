#------------------------------------------------------------
# Load data and packages
#------------------------------------------------------------
options(scipen = 10)
set.seed(1818)

# please install these if running the first time 
# using install.packages()
library(partykit)
library(tidyverse)
library(titanic)
library(PerformanceAnalytics)
library(rpart)       
library(rpart.plot)  



#------------------------------------------------------------
# Regression Trees
#------------------------------------------------------------
help(titanic)
data(titanic_train)

# see fancy correlation chart of titanic dataset
chart.Correlation(titanic_train %>% select(-Name) %>% 
                    select_if(is.numeric), 
                  pch = 20,
                  histogram = TRUE)


# view the top of the titanic data frame
head(titanic_train)

# clean data by creating a binary variable of "survived" vs "did not survive"
# create factors for sex and class of cabin
titanic_df <- titanic_train %>% as_tibble() %>% 
  mutate(Survived = as.factor(Survived),
         Sex = as.factor(Sex),
         Pclass = as.factor(Pclass)) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-Name, -Ticket,-Cabin,-Embarked, -PassengerId)



# Use the function ctree in rparty to estimate a 
# single regression tree classification model 
titanic_tree <- ctree(Survived ~ Sex + Pclass, 
                      data = titanic_df)

# print the fitted model object 
print(titanic_tree)

# Viewing the fitted model is easier 
plot(titanic_tree)



# use the ctree_control parameters to adjust stopping criteria
titanic_tree2 <- ctree(Survived ~ Sex + Pclass + 
                         SibSp + Parch + Fare, 
                       data = titanic_df,
                   control = partykit::ctree_control(alpha=0.1, 
                                                     minbucket = 50))
plot(titanic_tree2)


#------------------------------------------------------------
# Cross-Validating to Select Optimal Tree Depth
#------------------------------------------------------------
# cross validate to get optimal tree depth
# must use rpart package here

# rpart function to select optimal depth of tree
# read the help() file for rpart.control to learn about 
#  the different function options
# max depth  ensures the final tree only has this many splits
# min split means minimum observations in a node before 
#  a split can be attempted
# cp is the complexity parameter, overall Rsq must 
#  increase by cp at each step
library('rpart')
titanic_rpart <- rpart(Survived ~ Sex + Pclass + 
                      SibSp + Parch + Fare, 
                    data = titanic_df,
                    method = "class",control = list(cp = 0, 
                                                    minsplit = 10,
                                                    maxdepth = 10))
titanic_rpart$cptable

# plot the relationship between tree complexity (depth and cp)
# and CV error
plotcp(titanic_rpart)

#------------------------------------------------------------
# Regression Tree Exercises
#------------------------------------------------------------
# 1. Estimate a regression tree model predicting survival as a function 
#    of Sex, Pclass, Age, SibSp and Fare paid using the ctree package but 
#    don't use the ctree_control function to create a stopping condition. 
#    Plot the tree. What do you notice about the fitted tree?
tree_model <- c_tree(Survived ~ Sex + Pclass + Age + Sibsp + Fare, data = titanic_train)
plot(tree_model)

# 2. Now use the ctree_control function to set various levels of alpha and 
#    minbucket. Try alpha levels of 0.05 and 0.2 and minbuckets of 10 and 100. 
#    What do you notice about these trees?

tree_model2 <- ctree(Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare, 
                       data = titanic_df,
                       control = partykit::ctree_control(alpha = 0.5, 
                                                         minbucket = 10))
plot(tree_model2)

# 3. Interpret the trees and communicate who has the best chance of survival. 

# 4. If you finish the above, try the code below to create an interactive
#    CART tree

# install.packages('visNetwork')
# install.packages('sparkline')
visNetwork::visTree(titanic_rpart,
                    nodesPopSize = TRUE,
                    edgesFontSize = 18, 
                    nodesFontSize = 20, 
                    width = "100%",
                    height = "1200px")


