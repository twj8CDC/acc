# Load necessary libraries
install.packages("tidymodels")
install.packages("twopartm")
install.packages("Metrics")
install.packages("randomForest") 
chooseCRANmirror(72)
install.packages("xgboost")

library(tidymodels)
library(twopartm)
library(Metrics)
library(randomForest)
library(xgboost)


# Load the data
data(meps, package = "twopartm")
data <- meps
rm(meps)

# Create a binary variable indicating positive costs
data$positive_cost <- as.integer(data$exp_tot > 0)
data %>% glimpse

# Split the data into training and testing sets
set.seed(2024)  
data_split <- initial_split(data, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)



# GLM Model Specification
model_spec_glm_part1 <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")
model_spec_glm_part2 <- linear_reg() %>% 
  set_engine("glm", family = Gamma(link = "log")) %>% 
  set_mode("regression")

# Fit the GLM Models
model_fit_glm_part1 <- model_spec_glm_part1 %>% 
  fit(as.factor(positive_cost) ~ female + age + lninc + ins_unins, data = train_data)
train_data_positive <- train_data[train_data$exp_tot > 0, ]
model_fit_glm_part2 <- model_spec_glm_part2 %>% 
  fit(exp_tot ~ female + age + lninc + ins_unins, data = train_data_positive)

tidy(model_fit_glm_part1)
tidy(model_fit_glm_part2)


############################################################################
# Random Forest with Hyperparameter Tuning
rf_grid <- expand.grid(mtry = c(2, 3, 4), min_n = c(1, 3, 5))
model_spec_rf <- rand_forest(mtry = tune(), min_n = tune()) %>% 
  set_engine("randomForest") %>% 
  set_mode("classification")
rf_wf <- workflow() %>% 
  add_formula(as.factor(positive_cost) ~ female + age + lninc + ins_unins) %>% 
  add_model(model_spec_rf)
rf_res <- rf_wf %>% 
  tune_grid(resamples = vfold_cv(train_data, v = 5), grid = rf_grid)
# Select the best parameters for Random Forest
best_rf_params <- select_best(rf_res, metric = "accuracy")

# Fit the Random Forest Models
model_spec_rf_final <- rand_forest(mtry = best_rf_params$mtry, min_n = best_rf_params$min_n) %>% 
  set_engine("randomForest") %>% 
  set_mode("classification")
model_fit_rf_part1 <- model_spec_rf_final %>% 
  fit(as.factor(positive_cost) ~ female + age + lninc + ins_unins, data = train_data)

model_spec_rf_part2 <- rand_forest(mtry = 3, min_n = 1) %>%  # no cross val?
  set_engine("randomForest") %>% 
  set_mode("regression")
model_fit_rf_part2 <- model_spec_rf_part2 %>% 
  fit(exp_tot ~ female + age + lninc + ins_unins, data = train_data_positive)



####################################################################################
# XGBoost Model Specification and Tuning
xgb_grid <- expand.grid(trees = c(50, 100),
                        tree_depth = c(3, 4, 5),
                        learn_rate = c(0.01, 0.1, 0.3))
model_spec_xgb <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  min_n = 5  
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_wf <- workflow() %>% 
  add_formula(as.factor(positive_cost) ~ female + age + lninc + ins_unins) %>%
  add_model(model_spec_xgb)

xgb_res <- xgb_wf %>% 
  tune_grid(resamples = vfold_cv(train_data, v = 5), grid = xgb_grid)

# Select the best parameters for XGBoost
best_xgb_params <- select_best(xgb_res, metric = "accuracy")

model_spec_xgb_final <- boost_tree(
  trees = best_xgb_params$trees,
  tree_depth = best_xgb_params$tree_depth,
  learn_rate = best_xgb_params$learn_rate,
  min_n = 5
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# Fit the XGBoost Models
model_fit_xgb_part1 <- model_spec_xgb_final %>% 
  fit(as.factor(positive_cost) ~ female + age + lninc + ins_unins, data = train_data)

model_spec_xgb_part2 <- boost_tree(
  trees = 100,
  tree_depth = 5,  
  learn_rate = 0.1,
  min_n = 5
) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

model_fit_xgb_part2 <- model_spec_xgb_part2 %>% 
  fit(exp_tot ~ female + age + lninc + ins_unins, data = train_data_positive)

##########################################################
# Prediction Functions
two_part_pred = function(model1, model2, test_data, idf){
  # predict class
  p1_nm = paste0("pred_prt1_",idf)
  test_data[p1_nm] <- predict(model1, new_data = test_data)$.pred_class
  # select only those predicted positive class
  test_pos <- test_data[test_data[[p1_nm]] == "1", ]
  # predic cost
  p2_nm = paste0("pred_prt2_",idf)
  test_pos[p2_nm] <- predict(model2, new_data = test_pos)
  # create new empty column
  test_data[p2_nm] = 0
  # set the new column exp_tot_pred_glm to the predicted result 
  test_data[test_data[[p1_nm]] == "1", p2_nm] <- test_pos[p2_nm] 
  # get cost as the class * cost
  p3_nm = paste0("pred_", idf)
  test_data[p3_nm] = as.numeric(as.character(test_data[[p1_nm]])) * test_data[[p2_nm]]
  return (test_data)
}

# test_data[["dupersid"]][1:100]
# rlang::last_trace()

glm_td = two_part_pred(model_fit_glm_part1, model_fit_glm_part2, test_data, "glm")
glm_td %>% glimpse

rf_td = two_part_pred(model_fit_rf_part1, model_fit_rf_part2, test_data, "rf")
rf_td %>% glimpse

xg_td = two_part_pred(model_fit_xgb_part1, model_fit_xgb_part2, test_data, "xg")
xg_td %>% glimpse



# test_data$positive_cost_pred_glm <- predict(model_fit_glm_part1, new_data = test_data)$.pred_class
# test_data_positive_glm <- test_data[test_data$positive_cost_pred_glm == "1", ]
# test_data_positive_glm$exp_tot_pred_glm <- predict(model_fit_glm_part2, new_data = test_data_positive_glm)
# test_data$exp_tot_pred_glm <- 0
# test_data[test_data$positive_cost_pred_glm == "1", "exp_tot_pred_glm"] <- test_data_positive_glm$exp_tot_pred_glm
# test_data$glm_combined_pred <- as.numeric(as.character(test_data$positive_cost_pred_glm)) * test_data$exp_tot_pred_glm
# test_data %>% glimpse

# test_data$positive_cost_pred_rf <- predict(model_fit_rf_part1, new_data = test_data)$.pred_class
# test_data_positive_rf <- test_data[test_data$positive_cost_pred_rf == "1", ]
# test_data_positive_rf$exp_tot_pred_rf <- predict(model_fit_rf_part2, new_data = test_data_positive_rf)
# test_data$exp_tot_pred_rf <- 0
# test_data[test_data$positive_cost_pred_rf == "1", "exp_tot_pred_rf"] <- test_data_positive_rf$exp_tot_pred_rf
# test_data$rf_combined_pred <- as.numeric(as.character(test_data$positive_cost_pred_rf)) * test_data$exp_tot_pred_rf

# test_data$positive_cost_pred_xgb <- predict(model_fit_xgb_part1, new_data = test_data)$.pred_class
# test_data_positive_xgb <- test_data[test_data$positive_cost_pred_xgb == "1", ]
# test_data_positive_xgb$exp_tot_pred_xgb <- predict(model_fit_xgb_part2, new_data = test_data_positive_xgb)
# test_data$exp_tot_pred_xgb <- 0
# test_data[test_data$positive_cost_pred_xgb == "1", "exp_tot_pred_xgb"] <- test_data_positive_xgb$exp_tot_pred_xgb
# test_data$xgb_combined_pred <- as.numeric(as.character(test_data$positive_cost_pred_xgb)) * test_data$exp_tot_pred_xgb


# Incremental Cost Calculation
test_data_insured <- test_data
test_data_uninsured <- test_data

test_data_insured$ins_unins <- 0
test_data_uninsured$ins_unins <- 1

test_data_insured$ins_unins <- as.factor(test_data_insured$ins_unins)
test_data_uninsured$ins_unins <- as.factor(test_data_uninsured$ins_unins)

# GLM Incremental Cost
test_data_insured$positive_cost_pred_glm <- predict(model_fit_glm_part1, new_data = test_data_insured)$.pred_class
test_data_insured_positive_glm <- test_data_insured[test_data_insured$positive_cost_pred_glm == "1", ]
test_data_insured_positive_glm$exp_tot_pred_glm <- predict(model_fit_glm_part2, new_data = test_data_insured_positive_glm)
test_data_insured$exp_tot_pred_glm <- 0
test_data_insured[test_data_insured$positive_cost_pred_glm == "1", "exp_tot_pred_glm"] <- test_data_insured_positive_glm$exp_tot_pred_glm
test_data_insured$glm_combined_pred <- as.numeric(as.character(test_data_insured$positive_cost_pred_glm)) * test_data_insured$exp_tot_pred_glm

test_data_uninsured$positive_cost_pred_glm <- predict(model_fit_glm_part1, new_data = test_data_uninsured)$.pred_class
test_data_uninsured_positive_glm <- test_data_uninsured[test_data_uninsured$positive_cost_pred_glm == "1", ]
test_data_uninsured_positive_glm$exp_tot_pred_glm <- predict(model_fit_glm_part2, new_data = test_data_uninsured_positive_glm)
test_data_uninsured$exp_tot_pred_glm <- 0
test_data_uninsured[test_data_uninsured$positive_cost_pred_glm == "1", "exp_tot_pred_glm"] <- test_data_uninsured_positive_glm$exp_tot_pred_glm
test_data_uninsured$glm_combined_pred <- as.numeric(as.character(test_data_uninsured$positive_cost_pred_glm)) * test_data_uninsured$exp_tot_pred_glm

incremental_cost_glm <- mean(test_data_uninsured$glm_combined_pred - test_data_insured$glm_combined_pred)

# Random Forest Incremental Cost
test_data_insured$positive_cost_pred_rf <- predict(model_fit_rf_part1, new_data = test_data_insured)$.pred_class
test_data_insured_positive_rf <- test_data_insured[test_data_insured$positive_cost_pred_rf == "1", ]
test_data_insured_positive_rf$exp_tot_pred_rf <- predict(model_fit_rf_part2, new_data = test_data_insured_positive_rf)
test_data_insured$exp_tot_pred_rf <- 0
test_data_insured[test_data_insured$positive_cost_pred_rf == "1", "exp_tot_pred_rf"] <- test_data_insured_positive_rf$exp_tot_pred_rf
test_data_insured$rf_combined_pred <- as.numeric(as.character(test_data_insured$positive_cost_pred_rf)) * test_data_insured$exp_tot_pred_rf

test_data_uninsured$positive_cost_pred_rf <- predict(model_fit_rf_part1, new_data = test_data_uninsured)$.pred_class
test_data_uninsured_positive_rf <- test_data_uninsured[test_data_uninsured$positive_cost_pred_rf == "1", ]
test_data_uninsured_positive_rf$exp_tot_pred_rf <- predict(model_fit_rf_part2, new_data = test_data_uninsured_positive_rf)
test_data_uninsured$exp_tot_pred_rf <- 0
test_data_uninsured[test_data_uninsured$positive_cost_pred_rf == "1", "exp_tot_pred_rf"] <- test_data_uninsured_positive_rf$exp_tot_pred_rf
test_data_uninsured$rf_combined_pred <- as.numeric(as.character(test_data_uninsured$positive_cost_pred_rf)) * test_data_uninsured$exp_tot_pred_rf

incremental_cost_rf <- mean(test_data_uninsured$rf_combined_pred - test_data_insured$rf_combined_pred)

# XGBoost Incremental Cost
test_data_insured$positive_cost_pred_xgb <- predict(model_fit_xgb_part1, new_data = test_data_insured)$.pred_class
test_data_insured_positive_xgb <- test_data_insured[test_data_insured$positive_cost_pred_xgb == "1", ]
test_data_insured_positive_xgb$exp_tot_pred_xgb <- predict(model_fit_xgb_part2, new_data = test_data_insured_positive_xgb)
test_data_insured$exp_tot_pred_xgb <- 0
test_data_insured[test_data_insured$positive_cost_pred_xgb == "1", "exp_tot_pred_xgb"] <- test_data_insured_positive_xgb$exp_tot_pred_xgb
test_data_insured$xgb_combined_pred <- as.numeric(as.character(test_data_insured$positive_cost_pred_xgb)) * test_data_insured$exp_tot_pred_xgb

test_data_uninsured$positive_cost_pred_xgb <- predict(model_fit_xgb_part1, new_data = test_data_uninsured)$.pred_class
test_data_uninsured_positive_xgb <- test_data_uninsured[test_data_uninsured$positive_cost_pred_xgb == "1", ]
test_data_uninsured_positive_xgb$exp_tot_pred_xgb <- predict(model_fit_xgb_part2, new_data = test_data_uninsured_positive_xgb)
test_data_uninsured$exp_tot_pred_xgb <- 0
test_data_uninsured[test_data_uninsured$positive_cost_pred_xgb == "1", "exp_tot_pred_xgb"] <- test_data_uninsured_positive_xgb$exp_tot_pred_xgb
test_data_uninsured$xgb_combined_pred <- as.numeric(as.character(test_data_uninsured$positive_cost_pred_xgb)) * test_data_uninsured$exp_tot_pred_xgb

incremental_cost_xgb <- mean(test_data_uninsured$xgb_combined_pred - test_data_insured$xgb_combined_pred)

# RMSE Calculation
rmse_glm <- rmse(test_data$exp_tot, test_data$glm_combined_pred)
rmse_rf <- rmse(test_data$exp_tot, test_data$rf_combined_pred)
rmse_xgb <- rmse(test_data$exp_tot, test_data$xgb_combined_pred)

# Print Incremental Costs and RMSEs
cat("Incremental Cost (GLM):", incremental_cost_glm, "\n")
cat("Incremental Cost (RF):", incremental_cost_rf, "\n")
cat("Incremental Cost (XGBoost):", incremental_cost_xgb, "\n")
cat("RMSE (GLM):", rmse_glm, "\n")
cat("RMSE (RF):", rmse_rf, "\n")
cat("RMSE (XGBoost):", rmse_xgb, "\n")

