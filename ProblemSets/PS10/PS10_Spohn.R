# GIVEN STARTER CODE IMPORTED

library(tidyverse)
library(tidymodels)
library(magrittr)
library(modelsummary)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(kernlab)
library(knitr)
library(kableExtra)
library(xtable)

set.seed(100)

income <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", col_names = FALSE)
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income %<>% select(-native.country, -fnlwgt, education.num)
# Make sure continuous variables are formatted as numeric
income %<>% mutate(across(c(age,hours,education.num,capital.gain,capital.loss), as.numeric))
# Make sure discrete variables are formatted as factors
income %<>% mutate(across(c(high.earner,education,marital.status,race,workclass,occupation,relationship,sex), as.factor))
# Combine levels of factor variables that currently have too many levels
income %<>% mutate(education = fct_collapse(education,
                                            Advanced    = c("Masters","Doctorate","Prof-school"), 
                                            Bachelors   = c("Bachelors"), 
                                            SomeCollege = c("Some-college","Assoc-acdm","Assoc-voc"),
                                            HSgrad      = c("HS-grad","12th"),
                                            HSdrop      = c("11th","9th","7th-8th","1st-4th","10th","5th-6th","Preschool") 
                                           ),
                   marital.status = fct_collapse(marital.status,
                                            Married      = c("Married-civ-spouse","Married-spouse-absent","Married-AF-spouse"), 
                                            Divorced     = c("Divorced","Separated"), 
                                            Widowed      = c("Widowed"), 
                                            NeverMarried = c("Never-married")
                                           ), 
                   race = fct_collapse(race,
                                            White = c("White"), 
                                            Black = c("Black"), 
                                            Asian = c("Asian-Pac-Islander"), 
                                            Other = c("Other","Amer-Indian-Eskimo")
                                           ), 
                   workclass = fct_collapse(workclass,
                                            Private = c("Private"), 
                                            SelfEmp = c("Self-emp-not-inc","Self-emp-inc"), 
                                            Gov     = c("Federal-gov","Local-gov","State-gov"), 
                                            Other   = c("Without-pay","Never-worked","?")
                                           ), 
                   occupation = fct_collapse(occupation,
                                            BlueCollar  = c("?","Craft-repair","Farming-fishing","Handlers-cleaners","Machine-op-inspct","Transport-moving"), 
                                            WhiteCollar = c("Adm-clerical","Exec-managerial","Prof-specialty","Sales","Tech-support"), 
                                            Services    = c("Armed-Forces","Other-service","Priv-house-serv","Protective-serv")
                                           )
                  )


######################
# tidymodels time!
######################
income_split <- initial_split(income, prop = 0.8)
income_train <- training(income_split)
income_test  <- testing(income_split)




#####################
# logistic regression
#####################
print('Starting LOGIT')
# set up the task and the engine
tune_logit_spec <- logistic_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("classification")

# define a grid over which to try different values of the regularization parameter lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 3-fold cross-validation
rec_folds <- vfold_cv(income_train, v = 3)

# Workflow
rec_wf <- workflow() %>%
  add_model(tune_logit_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# what is the best value of lambda?
top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")
final_logit_lasso <- finalize_workflow(rec_wf,
                                 best_acc
                                )
print('*********** LOGISTIC REGRESSION **************')
logit_test <- last_fit(final_logit_lasso,income_split) %>%
         collect_metrics()

logit_test %>% print(n = 1)
top_acc %>% print(n = 1)

# combine results into a nice tibble (for later use)
logit_ans <- top_acc %>% slice(1)
logit_ans %<>% left_join(logit_test %>% slice(1),by=c(".metric",".estimator")) %>%
               mutate(alg = "logit") %>% select(-starts_with(".config"))


asdfafd

#####################
# tree model
#####################
print('Starting TREE')
# set up the task and the engine
tune_tree_spec <- decision_tree(
  min_n = tune(), # tuning parameter
  tree_depth = tune(), # tuning parameter
  cost_complexity = tune(), # tuning parameter
) %>% 
  set_engine("rpart") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (complexity, depth, etc.)
tree_parm_df1 <- tibble(cost_complexity = seq(.001,.2,by=.05))
tree_parm_df2 <- tibble(min_n = seq(10,100,by=10))
tree_parm_df3 <- tibble(tree_depth = seq(5,20,by=5))
tree_parm_df  <- full_join(tree_parm_df1,tree_parm_df2,by=character()) %>% full_join(.,tree_parm_df3,by=character())

# YOU FILL IN THE REST

# Create a workflow with the model and formula
tree_wf <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tune the model using tune_grid with the parameter grid and resampling strategy
tree_res <- tree_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = tree_parm_df,
    metrics = metric_set(accuracy, roc_auc) # Choose the metrics you want to evaluate
  )

# Select the best model based on the desired metric (e.g., accuracy)
best_tree <- select_best(tree_res, metric = "accuracy")

# Finalize the workflow with the best hyperparameters
final_tree <- finalize_workflow(tree_wf, best_tree)

# Test the final model on the test set and collect metrics
tree_test <- last_fit(final_tree, income_split) %>%
  collect_metrics()

# Print the results of the final model's performance
print(tree_test)


#####################
# neural net
#####################
print('Starting NNET')
# set up the task and the engine
tune_nnet_spec <- mlp(
  hidden_units = tune(), # tuning parameter
  penalty = tune()
) %>% 
  set_engine("nnet") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
nnet_parm_df1 <- tibble(hidden_units = seq(1,10))
lambda_grid   <- grid_regular(penalty(), levels = 10)
nnet_parm_df  <- full_join(nnet_parm_df1,lambda_grid,by=character())

# YOU FILL IN THE REST

# Create a workflow with the model and formula
nnet_wf <- workflow() %>%
  add_model(tune_nnet_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tune the model using tune_grid with the parameter grid and resampling strategy
nnet_res <- nnet_wf %>%
  tune_grid(
    resamples = rec_folds,  # You need to provide a resampling strategy such as `vfold_cv` 
    grid = nnet_parm_df,
    metrics = metric_set(accuracy, roc_auc)  # Choose the metrics you want to evaluate
  )

# Select the best model based on the desired metric (e.g., accuracy)
best_nnet <- select_best(nnet_res, metric = "accuracy")

# Finalize the workflow with the best hyperparameters
final_nnet <- finalize_workflow(nnet_wf, best_nnet)

# Test the final model on the test set and collect metrics
nnet_test <- last_fit(final_nnet, income_split) %>%
  collect_metrics()

# Print the results of the final model's performance
print(nnet_test)

# Predictions and actual outcomes from the test set
predictions <- last_fit(final_nnet, income_split) %>%
  collect_predictions()

# Examine the predictions data frame
print(predictions)



#####################
# knn
#####################
print('Starting KNN')
# set up the task and the engine
tune_knn_spec <- nearest_neighbor(
  neighbors = tune() # tuning parameter
) %>% 
  set_engine("kknn") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
knn_parm_df <- tibble(neighbors = seq(1,30))

# YOU FILL IN THE REST

# Create a workflow with the model and formula
knn_wf <- workflow() %>%
  add_model(tune_knn_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tune the model using tune_grid with the parameter grid and resampling strategy
knn_res <- knn_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = knn_parm_df,
    metrics = metric_set(accuracy, roc_auc) # Choose the metrics you want to evaluate
  )

# Select the best model based on the desired metric (e.g., accuracy)
best_knn <- select_best(knn_res, metric = "accuracy")

# Finalize the workflow with the best hyperparameters
final_knn <- finalize_workflow(knn_wf, best_knn)

# Test the final model on the test set and collect predictions
knn_fit_results <- last_fit(final_knn, income_split)

# Extract predictions from the test set
predictions <- knn_fit_results %>%
  collect_predictions()

# Print predictions
print(predictions)

# Assess the performance of the model on the test set
knn_test <- knn_fit_results %>%
  collect_metrics()

# Print the results of the final model's performance
print(knn_test)

#####################
# SVM
#####################
print('Starting SVM')
# set up the task and the engine
tune_svm_spec <- svm_rbf(
  cost = tune(), 
  rbf_sigma = tune()
) %>% 
  set_engine("kernlab") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
svm_parm_df1 <- tibble(cost = c(2^(-2), 2^(-1), 2^0, 2^1, 2^2, 2^10))
svm_parm_df2 <- tibble(rbf_sigma = c(2^(-2), 2^(-1), 2^0, 2^1, 2^2, 2^10))
svm_parm_df <- full_join(svm_parm_df1, svm_parm_df2, by = character())

# YOU FILL IN THE REST

# Create a workflow with the model and formula
svm_wf <- workflow() %>%
  add_model(tune_svm_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tune the model using tune_grid with the parameter grid and resampling strategy
svm_res <- svm_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = svm_parm_df,
    metrics = metric_set(accuracy, roc_auc) # Choose the metrics you want to evaluate
  )

# Select the best model based on the desired metric (e.g., accuracy)
best_svm <- select_best(svm_res, metric = "accuracy")

# Finalize the workflow with the best hyperparameters
final_svm <- finalize_workflow(svm_wf, best_svm)

# Test the final model on the test set and collect predictions
svm_fit_results <- last_fit(final_svm, income_split)

# Extract predictions from the test set
predictions <- svm_fit_results %>%
  collect_predictions()

# Print predictions
print(predictions)

# Assess the performance of the model on the test set
svm_test <- svm_fit_results %>%
  collect_metrics()

# Print the results of the final model's performance
print(svm_test)


#####################
# combine answers
#####################
# List of data frames
dfs <- list(logit_ans, best_tree, best_nnet, best_knn)

# List of file names
file_names <- c("logit_ans", "best_tree", "best_nnet", "best_knn")

# Loop through the data frames and create a LaTeX file for each
for (i in seq_along(dfs)) {
  # Convert the data frame to a LaTeX table
  latex_table <- xtable(dfs[[i]], type = "latex", digits = 2)
  
  # Save the LaTeX code to a file in the Downloads folder
  downloads_path <- path.expand("~/Downloads")
  sink(file.path(downloads_path, paste0(file_names[i], ".tex")))
  cat("\\begin{longtable}{", paste(rep("r", ncol(dfs[[i]])), collapse = ""), "}\n")
  print(latex_table, include.rownames = FALSE, sanitize.text.function = function(x) x)
  cat("\n\\end{longtable}")
  sink()
}

latex_table <- xtable(best_knn, type = "latex", digits = 2)

# Save the LaTeX code to a file in the Downloads folder
downloads_path <- path.expand("~/Downloads")
sink(file.path(downloads_path, "best_knn.tex"))
cat("\\begin{longtable}{", paste(rep("r", ncol(best_knn)), collapse = ""), "}\n")
print(latex_table, include.rownames = FALSE, sanitize.text.function = function(x) x)
cat("\n\\end{longtable}")
sink()