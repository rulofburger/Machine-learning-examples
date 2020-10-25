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

# Create alternative versions of data in which variables have been normalised.
# This is necessary for shrinkage methods
pre_proc_values <- preProcess(df_train, method = c("center", "scale"))
df_train_pre_proc <- predict(pre_proc_values, df_train)
df_test_pre_proc <- predict(pre_proc_values, df_test)


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


#> BAGGED TREE  ====
# Simple rpart model ----

# Empirically tuned rpart model ----
set.seed(802740)

fitControl <- trainControl(
  method = "cv", 
  number = 10, 
  verboseIter = TRUE
)

model_treebag <- caret::train(
  data = df_train,
  log_income_pc ~ ., 
  method = 'treebag', 
  trControl = fitControl, 
  tuneLength = 10
)

model_treebag
varImp(model_treebag)
partial (model_treebag)

# Predict outcomes for test subset using model estimated on train set
df_predict_treebag <- model_treebag$finalModel %>% 
  predict(df_test)

# Calculate model fit metrics (RMSE and R-squared) for test subset
fit_treebag <- data.frame(
  RMSE = RMSE(df_predict_treebag, df_test$log_income_pc),
  Rsquare = R2(df_predict_treebag, df_test$log_income_pc)
)

# Print model fit metrics 
print(fit_treebag)
# RMSE 0.8370484


#> RANDOM FOREST  ====
# Simple rpart model ----

# Empirically tuned rpart model ----
set.seed(802740)

fitControl <- trainControl(
  method = "cv", 
  number = 10, 
  verboseIter = TRUE
)

model_ranger <- caret::train(
  data = df_train,
  log_income_pc ~ ., 
  method = 'ranger', 
  trControl = fitControl, 
  tuneLength = 10
)

plot(model_ranger)

# Predict outcomes for test subset using model estimated on train set
df_predict_ranger <- model_ranger$finalModel %>% 
  predict(df_test)

# Calculate model fit metrics (RMSE and R-squared) for test subset
fit_ranger <- data.frame(
  RMSE = RMSE(df_predict_ranger$predictions, df_test$log_income_pc),
  Rsquare = R2(df_predict_ranger$predictions, df_test$log_income_pc)
)

# Print model fit metrics 
print(fit_ranger)
# RMSE 0.7655455


#> BOOSTED TREE  ====
# Simple rpart model ----

# Empirically tuned rpart model ----
set.seed(802740)

fitControl <- trainControl(
  method = "cv", 
  number = 10, 
  verboseIter = TRUE
)

model_bstTree <- caret::train(
  data = df_train,
  log_income_pc ~ ., 
  # method = 'blackboost', 
  method = 'bstTree',
  trControl = fitControl, 
  tuneLength = 10
)

model_bstTree
plot(model_bstTree)
varImp(model_bstTree)

# Predict outcomes for test subset using model estimated on train set
df_predict_bstTree <- model_bstTree$finalModel %>% 
  predict(df_test)

# Calculate model fit metrics (RMSE and R-squared) for test subset
fit_bstTree <- data.frame(
  RMSE = RMSE(df_predict_bstTree, df_test$log_income_pc),
  Rsquare = R2(df_predict_bstTree, df_test$log_income_pc)
)

# Print model fit metrics 
print(fit_bstTree)
# RMSE 0.7615002
# RMSE 0.7689561 (blackboost)

#> XGBoost  ====
# Simple rpart model ----

model_xgbTree <- caret::train(
  data = df_train,
  log_income_pc ~ ., 
  # method = 'blackboost', 
  method = 'xgbTree',
  trControl = fitControl, 
  tuneLength = 10
)

model_xgbTree
plot(model_xgbTree)
varImp(model_xgbTree)

# Predict outcomes for test subset using model estimated on train set
df_predict_xgbTree <- model_xgbTree %>% 
  predict(df_test)

# Calculate model fit metrics (RMSE and R-squared) for test subset
fit_xgbTree <- data.frame(
  RMSE = RMSE(df_predict_xgbTree, df_test$log_income_pc),
  Rsquare = R2(df_predict_xgbTree, df_test$log_income_pc)
)

# Print model fit metrics 
print(fit_xgbTree)
# RMSE 0.7621086


#> LightGBM  ====
# Simple rpart model ----


valid_index = createDataPartition(
  y = df_train$log_income_pc, 
  p = 0.25, 
  list = F
)

mat_train <- lightgbm::lgb.Dataset(data  = as.matrix(df_train[-valid_index, ] %>% filter() %>% select(-log_income_pc)), 
                      label = df_train$log_income_pc[-valid_index])

mat_valid <- lightgbm::lgb.Dataset(data  = as.matrix(df_train[valid_index, ] %>% filter() %>% select(-log_income_pc)), 
                                   label = df_train$log_income_pc[valid_index])

grid_search <- expand.grid(
  num_leaves        = c(5,7,9,255),
  max_depth         = c(4,6,8,48,64),
  subsample         = c(0.7,0.9,1),
  colsample_bytree  = c(0.7,0.9,1),
  min_child_weight  = c(0,0.01,0.1),
  scale_pos_weight  = c(100,200,300,400)
)

lgb_params <- list(objective = "regression", 
                   boosting  = "gbdt", 
                   metric = "RMSE",
                        # learning_rate = params$learning_rate,
                        # num_leaves = params$num_leaves,
                        # colsample_bytree = params$colsample_bytree,
                        # max_depth = params$max_depth,
                        # lambda = params$lambda,
                    nthread   = 20
                    )
              

model_lightGBM <- lightgbm::lgb.train(
                    params = lgb_params, 
                    data = mat_train,
                    verbose = -1,
                    nrounds = 200)

model <- list()
perf <- numeric(nrow(grid_search))

for (i in 1:nrow(grid_search)) {
  cat("Model ***", i , "*** of ", nrow(grid_search), "\n")
  model[[i]] <- lightgbm::lgb.train(
    list(objective         = "regression",
         boosting          = "gbdt", 
         metric            = "RMSE",
         learning_rate     = 0.1,
         min_child_samples = 100,
         max_bin           = 100,
         subsample_freq    = 1,
         num_leaves        = grid_search[i, "num_leaves"],
         max_depth         = grid_search[i, "max_depth"],
         subsample         = grid_search[i, "subsample"],
         colsample_bytree  = grid_search[i, "colsample_bytree"],
         min_child_weight  = grid_search[i, "min_child_weight"],
         scale_pos_weight  = grid_search[i, "scale_pos_weight"]),
    mat_train,
    valids = list(validation = mat_valid),
    nthread = 4, 
    nrounds = 5, # increase/ decrease rounds
    verbose= 1, 
    early_stopping_rounds = 2
  )
  perf[i] <- max(unlist(model[[i]]$record_evals[["validation"]][["rmse"]][["eval"]]))
  invisible(gc()) # free up memory after each model run
}

# grid_search result
cat("Model ", which.min(perf), " is min RMSE: ", min(perf), sep = "","\n")
best_params = grid_search[which.min(perf), ]
data.table::fwrite(best_params,"best_params_for_sample_data.txt")

cat("Best params within chosen grid search: ", "\n")
t(best_params)

# Predict outcomes for test subset using model estimated on train set
df_predict_lightGBM <- model_lightGBM %>% 
  predict(data.matrix(df_test %>% select(-log_income_pc)))

# Calculate model fit metrics (RMSE and R-squared) for test subset
fit_lightGBM <- data.frame(
  RMSE = RMSE(df_predict_lightGBM, df_test$log_income_pc),
  Rsquare = R2(df_predict_lightGBM, df_test$log_income_pc)
)

# Print model fit metrics 
print(fit_lightGBM)
# RMSE 0.7834848



#> Neural Net  ====
# Simple rpart model ----


library(neuralnet)
n <- names(df_train)
f <- as.formula(paste("log_income_pc ~", paste(n[!n %in% "log_income_pc"], collapse = " + ")))
nn <- neuralnet(f, data = df_train, hidden = c(5,3), linear.output = T)


model_nnet <- nnet::nnet(
  formula = log_income_pc ~ .,
  data = df_train,
  size = 2,
  rang = 0.1,
  decay = 5e-4, 
  maxit = 200
)

df_predict_nnet <- model_nnet %>% 
  predict(df_test)

fit_nnet <- data.frame(
  RMSE = RMSE(df_predict_nnet, df_test$log_income_pc),
  Rsquare = R2(df_predict_nnet, df_test$log_income_pc)
)

fitControl <- trainControl(
  method = "cv", 
  number = 10, 
  verboseIter = TRUE,
  search = "random",
)

model_nnet_caret <- caret::train(
  log_income_pc ~ rooms + members + head_educ + prov, 
  data = df_train,
  method = 'nnet', 
  tuneLength = 10,
  trControl = fitControl,
  # preProc = c("center", "scale"),
  # maxit = 2000,
  # ## and the number of parameters used by the model
  MaxNWts = 10000
)
model_nnet_caret

