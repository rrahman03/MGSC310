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
library('randomForest')

# we will also use these packages later in the lab
# install.packages('visNetwork')
# install.packages('sparkline')

#------------------------------------------------------------
# bagging - bootstrapp aggregation
#------------------------------------------------------------
data(titanic_train)

# clean data by creating a binary variable of "survived" vs "did not survive"
# create factors for sex and class of cabin
titanic_df <- titanic_train %>% as_tibble() %>% 
  mutate(Survived = as.factor(Survived),
         Sex = as.factor(Sex),
         Pclass = as.factor(Pclass)) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-Name, -Ticket,-Cabin,-Embarked, -PassengerId)

# store rownames as columns
titanic_boot_preds <- titanic_df %>% rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))

B <- 100      # number of bootstrap samples
num_b <- 500  # sample size of each bootstrap
boot_mods <- list() # store our bagging models
for(i in 1:B){
  boot_idx <- sample(1:nrow(titanic_df), 
                     size = num_b,
                     replace = FALSE)
  # fit a tree on each bootstrap sample
  boot_tree <- ctree(Survived ~ Pclass + Sex + Age + SibSp + Fare, 
                     data = titanic_df %>% 
                       slice(boot_idx)) 
  # store bootstraped model
  boot_mods[[i]] <- boot_tree
  # generate predictions for that bootstrap model
  preds_boot <- data.frame(
    preds_boot = predict(boot_tree),
    rowname = boot_idx 
  )  
  # rename prediction to indicate which boot iteration it came from
  names(preds_boot)[1] <- paste("preds_boot",i,sep = "")
  # merge predictions to dataset
  titanic_boot_preds <- left_join(x = titanic_boot_preds, y = preds_boot,
                                  by = "rowname")
}

## examine some of the individual models
plot(boot_mods[[1]], gp = gpar(fontsize = 8))

plot(boot_mods[[10]], gp = gpar(fontsize = 6))

plot(boot_mods[[50]], gp = gpar(fontsize = 6))

# must convert factor into numeric, note that class "0" = 1, 
# and class "1" = 2, so we need to subtract 1 from every column
titanic_boot_preds %<>% mutate_if(is.factor, as.numeric) %>% 
  mutate_all(function(x){x - 1})

# calculate mean over all the bootstrap predictions
titanic_boot_preds <- titanic_boot_preds %>% 
  mutate(preds_bag = 
           select(., preds_boot1:preds_boot100) %>% 
           rowMeans(na.rm = TRUE))

# congratulations! You have bagged your first model!
ggplot(titanic_boot_preds, aes(x = preds_bag)) + 
  geom_histogram()


#---------------------------------------------------------------
# Random Forest
#---------------------------------------------------------------
library('randomForest')

rf_fit <- randomForest(Survived ~ 
                         Pclass + Sex + Age + 
                         SibSp + Fare, 
                       data = titanic_df,
                       type = classification,
                       mtry = 3,
                       na.action = na.roughfix,
                       ntree = 400, 
                       importance = TRUE)

print(rf_fit)

plot(rf_fit)

# Tuning Random Forests To Determine
#  Optimal Parameters (mtry)
#---------------------------------------------------------------
rf_mods <- list()
oob_err <- NULL
test_err <- NULL
for(mtry in 1:9){
  rf_fit <- randomForest(Survived ~ 
                           Pclass + Sex + Age + SibSp + Fare, 
                         data = titanic_df,
                         mtry = mtry,
                         na.action = na.roughfix,
                         ntree = 600)
  oob_err[mtry] <- rf_fit$err.rate[600]
  
  cat(mtry," ")
}

results_DF <- data.frame(mtry = 1:9, oob_err)
ggplot(results_DF, aes(x = mtry, y = oob_err)) + geom_point() + theme_minimal()


#---------------------------------------------------------------
# Random Forest Exercises
#---------------------------------------------------------------
# 1. Estimate a random forest model predicting survival using the predictors 
#    Pclass, Sex, Age, SibSp and Fare. First set mtry = 5, and select ntree = 400. 
#    Call this model rf_fit
rf_fit <- randomForest(Survived ~ 
                         Pclass + Sex + Age + 
                         SibSp + Fare, 
                       data = titanic_df,
                       type = classification,
                       mtry = 5,
                       na.action = na.roughfix,
                       ntree = 400, 
                       importance = TRUE)

  
# 2. Call the print function against rf_fit
print(rf_fit)

# 3. call the plot() function against the fitted model and describe the plot. 
plot(rf_fit)

# 4. How many trees should we use to estimate the model?
# We should use around 100 trees to estimate this model

# 5. (Time permitting) Estimate a bagging model to predict survival.  





