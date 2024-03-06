#------------------------------------------------------------
# Load data and packages
#------------------------------------------------------------
options(scipen = 10)
set.seed(1818)

# please install these if running the first time 
# using install.packages()
library('tidyverse')
library('titanic')
library('randomForest')
library('rsample')
library('randomForestExplainer')


#---------------------------------------------------------------
# load and clean titanic dataset 
#---------------------------------------------------------------
data(titanic_train)
data(titanic_test)
# clean data by creating a binary variable of "survived" vs "did not survive"
# create factors for sex and class of cabin
titanic_df <- titanic_train %>% as_tibble() %>% 
  mutate(Survived = as.factor(Survived),
         Sex = as.factor(Sex),
         Pclass = as.factor(Pclass)) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-Name, -Ticket,-Cabin,-Embarked, -PassengerId) %>% 
  drop_na()

titanic_test <- titanic_test %>% as_tibble() %>% 
  mutate(
    Sex = as.factor(Sex),
    Pclass = as.factor(Pclass)) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-Name, -Ticket,-Cabin,-Embarked, -PassengerId) %>% 
  drop_na()


#---------------------------------------------------------------
# Random Forest
#---------------------------------------------------------------

rf_fit <- randomForest(Survived ~ 
                         Pclass + Sex + Age + SibSp + Fare, 
                       data = titanic_df,
                       type = classification,
                       mtry = 3,
                       na.action = na.roughfix,
                       ntree = 300,
                       importance = TRUE,
                       keep.inbag = TRUE)

print(rf_fit)

#---------------------------------------------------------------
# Global Interpretability - Permutation 
#---------------------------------------------------------------

# importance of each feature
importance(rf_fit, type = 1, scale = FALSE)

# plot the importance of each variable 
varImpPlot(rf_fit, type = 1, scale = FALSE)


# partial dependence plots
library('pdp')
pdp::partial(rf_fit, 
             pred.var = "Age", 
             plot = TRUE,
             rug = TRUE,
             plot.engine = "ggplot2",
             smooth = TRUE,
             trim.outliers = FALSE,
             prob = TRUE,
             center = TRUE, 
             type = "classification", 
             which.class = 2)



 # PDP prediction interaction
library('randomForestExplainer')
plot_predict_interaction(rf_fit, 
                         titanic_df,
                         "Age",
                         "Fare", 
                         grid = 9)


#---------------------------------------------------------------
# Global Interpretability - Meta Model Information
#---------------------------------------------------------------
# min depth distribution
plot_min_depth_distribution(rf_fit)

# 
plot_multi_way_importance(rf_fit, 
                          size_measure = "no_of_nodes")


# fully explain forest 
explain_forest(rf_fit, 
               interactions = TRUE, 
               data = titanic_df)

#---------------------------------------------------------------
# Local Interpretability - SHAPELEY
#---------------------------------------------------------------
library('DALEX')
explain_rf <- DALEX::explain(model = rf_fit,  
                             data = titanic_df[, -1],
                             y = titanic_df$Survived == 1, 
                             label = "Random Forest")

predict(explain_rf, titanic_test[1,])

shap_test1 <- predict_parts(explainer = explain_rf, 
                            new_observation = titanic_test[1,], 
                            type = "break_down",
                            B = 25)
plot(shap_test1, show_boxplots = TRUE) 


#---------------------------------------------------------------
# Lab Interpreting Random Forests
#---------------------------------------------------------------
# 1. Estimate the random forest model below

rf_fit2 <- randomForest(Survived ~ 
                          Sex + Age + SibSp + Fare, 
                        data = titanic_df %>% drop_na(),
                        type = classification,
                        mtry = 2,
                        ntree = 500,
                        importance = TRUE,
                        keep.inbag = TRUE)

# 2. Calculate the variable importance using the function importance
#    What does the output indicate is the most important variable is?
# The most important variable is sex

importance_rf <- importance(rf_fit2)
print(importance_rf)

# 3. Produce a variable importance plot and explain what it means
varImpPlot(rf_fit2)

# 4. Produce a partial dependence plot for "Age" using parital. 
#    Explain in words how Age affects the probability of survival

partial_age = partical(rf_fit2, pred.var = "Age")

ggplot(partial_age, aes(x = Age, y = yhat)) +
  geom_line() +
  labs(title = "Partial Dependent Plot for Age",
       x = "Age",
       y = "Predicted Probability of Survival")

# 5. Interact Age with SibSp and explain the plot. 
#    For what age and number of siblings is the probability of survival quite high?
plot_predict_interaction(rf_fit2,
                         titanic_df,
                         "Age",
                         "SibSp",
                         grid = 9)
# 6. Run explain_forest(rf_fit2) and look at the output
explain_forest(rf_fit2)
