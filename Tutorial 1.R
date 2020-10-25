#> ADD DIRECTORY FOR INSTALLING PACKAGES ====

# myPaths <- .libPaths()
# myPaths <- c(myPaths, ‘C:/CustomR’)
# myPaths <- c(myPaths[2], myPaths[1])  # switch them
# .libPaths(myPaths)  # reassign them


#> LOAD LIBRARIES ====
library(caret)
library(rattle)
library(party)
library(tidyverse)


#> LOAD DATA ====
df_NIDS <- readRDS("df_NIDS.rds")

#> PREPROCESS DATA ====
# Make factor variables into dummies
dummies <- dummyVars(log_income_pc ~ ., data = df_NIDS)
df_NIDS_dummies <- data.frame(
  log_income_pc = df_NIDS$log_income_pc, 
  predict(dummies, newdata = df_NIDS)
  ) 

#> SPLIT SAMPLE ====
# Set seed so that results are replicable
set.seed(7483831)
# Randomly sample (based on stratification of outcome) 25% of data for test set
train_index = createDataPartition(
  y = df_NIDS_dummies$log_income_pc, 
  p = 0.75, 
  list = F
  )
# Create train subset data frame
df_train = df_NIDS_dummies[train_index,]
# Create test subset data frame
df_test = df_NIDS_dummies[-train_index,]

#> OLS REGRESSION ====
# Estimate OLS model on train subset
model_lm <- lm(data = df_train, formula = log_income_pc ~ .)

# Print output
summary(model_lm)

# Predict outcomes for test subset using model estimated on train set
df_predict_lm <- model_lm %>% 
  predict(df_test)

# Calculate model fit metrics (RMSE and R-squared) for test subset
fit_lm <- data.frame(
  RMSE = RMSE(df_predict_lm, df_test$log_income_pc),
  Rsquare = R2(df_predict_lm, df_test$log_income_pc)
)

# Print model fit metrics 
print(fit_lm)
# RMSE: 0.8051955


#> RPART REGRESSION ====
# Estimte simple rpart model with default hyperparameter values ----
model_rpart_simple <- rpart::rpart(
  data = df_train,
  formula = log_income_pc ~ ., 
  method = "anova"
  )

# Print model output (splits, predicted values, and sample sizes)
model_rpart_simple

# Print default parameter values
model_rpart_simple$control

# Print graph of regression tree 
fancyRpartPlot(model_rpart_simple, main = "Regression Tree for NIDS Income")

# Predict outcomes for test subset using model estimated on train set
df_predict_rpart_simple <- model_rpart_simple %>% 
  predict(df_test)

# Calculate model fit metrics (RMSE and R-squared) for test subset
fit_rpart_simple <- data.frame(
  RMSE = RMSE(df_predict_rpart_simple, df_test$log_income_pc),
  Rsquare = R2(df_predict_rpart_simple, df_test$log_income_pc)
)
print(fit_rpart_simple)
# RMSE: 0.8746037

# Estimate more complex rpart model with cp value of 0.003 ----
model_rpart_complex <- rpart::rpart(
  data = df_train,
  formula = log_income_pc ~ ., 
  method = "anova",
  control = rpart::rpart.control(
    cp = 0.001
    )
  )

# Print model output
model_rpart_complex

# Print graph of regression tree 
fancyRpartPlot(model_rpart_complex, main = "Regression Tree for NIDS Income")

# Predict outcomes for test subset using model estimated on train set
df_predict_rpart_complex <- model_rpart_complex %>% 
  predict(df_test)

# Calculate model fit metrics (RMSE and R-squared) for test subset
fit_rpart_complex <- data.frame(
  RMSE = RMSE(df_predict_rpart_complex, df_test$log_income_pc),
  Rsquare = R2(df_predict_rpart_complex, df_test$log_income_pc)
)

# Print model fit metrics 
print(fit_rpart_complex)
# RMSE: 0.8500053

# Empirically tune cp value for rpart model ----
# Set seed
set.seed(802740)

# Set control function to use 10 fold cross-validation and to print output while
# tuning (verboseIter = TRUE)
fitControl <- trainControl(
  method = "cv", 
  number = 10, 
  verboseIter = TRUE
  )

# Use caret's train function to tune cp value of rpart model using 10 grid points
# chosen by function
model_rpart_caret <- caret::train(
  data = df_train,
  log_income_pc ~ ., 
  method = 'rpart', 
  trControl = fitControl, 
  tuneLength = 10
  )

# Print results from cross-validation
model_rpart_caret
# Plot results from cross-validation
plot(model_rpart_caret)

# The plot shows that the chosen grid points did not capture the minimum of the
# U-shaped out of sample RMSE. We therefore have to specify our own grid points
# and repeat the empirical tuning.

# Choose 100 grid points between 10^-3 and 10^-1.8. (These were values I found
# after playing around with various other values)
cp_grid <- 10^seq(-3, -1.8, length = 100)

# Retune cp hyperparater using user-defined grid points
model_rpart_caret <- caret::train(
  data = df_train,
  log_income_pc ~ ., 
  method = 'rpart', 
  trControl = fitControl, 
  tuneGrid = expand.grid(cp = cp_grid)
)

# Print results from cross-validation
model_rpart_caret
# Plot results from cross-validation
plot(model_rpart_caret)
# Print results of finalModel: the model estimated using the tuned
# hyperparameter value
model_rpart_caret$finalModel

fancyRpartPlot(model_rpart_caret$finalModel, main = "Regression Tree for NIDS Income")

# Predict outcomes for test subset using model estimated on train set
df_predict_rpart_caret <- model_rpart_caret$finalModel %>% 
  predict(df_test)

# Calculate model fit metrics (RMSE and R-squared) for test subset
fit_rpart_caret <- data.frame(
  RMSE = RMSE(df_predict_rpart_caret, df_test$log_income_pc),
  Rsquare = R2(df_predict_rpart_caret, df_test$log_income_pc)
)

# Print model fit metrics 
print(fit_rpart_caret)
# RMSE: 0.8464256

#> SHRINKAGE METHODS ====

# Preprocess data for shrinkage methods ----

# Create alternative versions of data in which variables have been normalised.
# This is necessary for shrinkage methods
pre_proc_values <- preProcess(df_train, method = c("center", "scale"))
df_train_pre_proc <- predict(pre_proc_values, df_train)
df_test_pre_proc <- predict(pre_proc_values, df_test)

# Estimate ridge regression model ----

#Set seed
set.seed(41094)

#Set grid points for lambda (shrinkage penalty hyperparameter)
lambda <- 10^seq(-4, 0, length = 100)

# Emprically tune lambda using caret's train function with "glmnet" learner
# (which uses elastic net model generally, and ridge regressions when specifying
# alpha = 0) 
model_ridge <- train(
  log_income_pc ~ ., 
  data = df_train_pre_proc, 
  method = "glmnet",
  trControl = fitControl,
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)

# Print RMSE values from tuning
model_ridge

# Plot RMSE values 
plot(model_ridge)

# Print model coefficients for final model
coef(model_ridge$finalModel, model_ridge$bestTune$lambda)

# Predict outcomes for test subset using model estimated on train set
df_predict_ridge <- model_ridge %>% 
  predict(df_test_pre_proc)

# Calculate model fit metrics (RMSE and R-squared) for test subset
fit_ridge <- data.frame(
  RMSE = RMSE(df_predict_ridge, df_test_pre_proc$log_income_pc),
  Rsquare = R2(df_predict_ridge, df_test_pre_proc$log_income_pc)
)

# Print model fit metrics 
print(fit_ridge)
# RMSE: 0.7827756

# Estimate LASSO regression model ----

#Set seed
set.seed(064285)

# Emprically tune lambda using while setting alpha = 1 to indicate use of LASSO
# model
model_lasso <- train(
  log_income_pc ~ .,
  data = df_train_pre_proc,
  method = "glmnet",
  trControl = fitControl,
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)

# Print RMSE values from tuning
model_lasso

# Plot RMSE values 
plot(model_lasso)

# Print model coefficients for final model
coef(model_lasso$finalModel, model_lasso$bestTune$lambda)

# Predict outcomes for test subset using model estimated on train set
df_predict_lasso <- model_lasso %>% 
  predict(df_test_pre_proc)

# Calculate model fit metrics (RMSE and R-squared) for test subset
fit_lasso <- data.frame(
  RMSE = RMSE(df_predict_lasso, df_test_pre_proc$log_income_pc),
  Rsquare = R2(df_predict_lasso, df_test_pre_proc$log_income_pc)
)

# Print model fit metrics 
print(fit_lasso)
# RMSE: 0.772734


# Estimate elastic net model ----

#Set seed
set.seed(86920320)

# Emprically tune lambda and alpha hyperparameters
model_elas_net <- train(
  log_income_pc ~ .,
  data = df_train_pre_proc,
  method = "glmnet",
  trControl = fitControl,
  tuneLength = 10
)

# Print RMSE values from tuning
model_elas_net

# Plot RMSE values 
plot(model_elas_net)

# Print model coefficients for final model
coef(model_elas_net$finalModel, model_elas_net$bestTune$lambda)

# Predict outcomes for test subset using model estimated on train set
df_predict_elas_net <- model_elas_net %>% 
  predict(df_test_pre_proc)

# Calculate model fit metrics (RMSE and R-squared) for test subset
fit_elas_net <- data.frame(
  RMSE = RMSE(df_predict_elas_net, df_test_pre_proc$log_income_pc),
  Rsquare = R2(df_predict_elas_net, df_test_pre_proc$log_income_pc)
)

# Print model fit metrics 
print(fit_elas_net)
# RMSE: 0.7724225
