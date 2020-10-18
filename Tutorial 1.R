#> LOAD LIBRARIES ====
library(caret)
library(rattle)
library(party)
library(tidyverse)

#> LOAD DATA ====
df_NIDS <- readRDS("df_NIDS.rds")

#> PREPROCESS DATA ====

dummies <- dummyVars(log_income_pc ~ ., data = df_NIDS)

df_NIDS_dummies <- data.frame(
  log_income_pc = df_NIDS$log_income_pc, 
  predict(dummies, newdata = df_NIDS)
  ) 
  # mutate_at(vars(6:ncol(.)), as.logical)

#> SPLIT SAMPLE ====
set.seed(7483831)

train_index = createDataPartition(
  y = df_NIDS_dummies$log_income_pc, 
  p = 0.75, 
  list = F
  )

df_train = df_NIDS_dummies[train_index,]
df_test = df_NIDS_dummies[-train_index,]

pre_proc_values <- preProcess(df_train, method = c("center", "scale"))

df_train_pre_proc <- predict(pre_proc_values, df_train)
df_test_pre_proc <- predict(pre_proc_values, df_test)


#> OLS REGRESSION ====
# Estimate OLS model on train subset
model_lm <- lm(data = df_train, formula = log_income_pc ~ .)

# Print output
summary(model_lm)

# Predict OLS model in test subset
df_predict_lm <- model_lm %>% 
  predict(df_test)

# Evaluate model fit in test subset
fit_lm <- data.frame(
  RMSE = RMSE(df_predict_lm, df_test$log_income_pc),
  Rsquare = R2(df_predict_lm, df_test$log_income_pc)
)
print(fit_lm)


#> RPART REGRESSION ====
# Simple rpart model ----
model_rpart_simple <- rpart::rpart(
  data = df_train,
  formula = log_income_pc ~ ., 
  method = "anova"
  )
model_rpart_simple
fancyRpartPlot(model_rpart_simple, main = "Regression Tree for NIDS Income")

df_predict_rpart_simple <- model_rpart_simple %>% 
  predict(df_test)

fit_rpart_simple <- data.frame(
  RMSE = RMSE(df_predict_rpart_simple, df_test$log_income_pc),
  Rsquare = R2(df_predict_rpart_simple, df_test$log_income_pc)
)
print(fit_rpart_simple)

# Complex rpart model ----
model_rpart_complex <- rpart::rpart(
  data = df_train,
  formula = log_income_pc ~ ., 
  method = "anova",
  control = rpart::rpart.control(
    cp = 0.003
    )
  )

model_rpart_complex
fancyRpartPlot(model_rpart_complex, main = "Regression Tree for NIDS Income")

df_predict_rpart_complex <- model_rpart_complex %>% 
  predict(df_test)

fit_rpart_complex <- data.frame(
  RMSE = RMSE(df_predict_rpart_complex, df_test$log_income_pc),
  Rsquare = R2(df_predict_rpart_complex, df_test$log_income_pc)
)
print(fit_rpart_complex)

# Empirically tuned rpart model ----
set.seed(802740)

fitControl <- trainControl(
  method = "cv", 
  number = 10, 
  verboseIter = TRUE
  )

model_rpart_caret <- caret::train(
  data = df_train,
  log_income_pc ~ ., 
  method = 'rpart', 
  trControl = fitControl, 
  tuneLength = 10
  )

model_rpart_caret


cp_grid <- 10^seq(-3, 0, length = 100)
model_rpart_caret <- caret::train(
  data = df_train,
  log_income_pc ~ ., 
  method = 'rpart', 
  trControl = fitControl, 
  tuneGrid = expand.grid(cp = cp_grid)
)

model_rpart_caret
model_rpart_caret$finalModel

fancyRpartPlot(model_rpart_caret$finalModel, main = "Regression Tree for NIDS Income")

df_predict_rpart_caret <- model_rpart_caret$finalModel %>% 
  predict(df_test)

fit_rpart_caret <- data.frame(
  RMSE = RMSE(df_predict_rpart_caret, df_test$log_income_pc),
  Rsquare = R2(df_predict_rpart_caret, df_test$log_income_pc)
)
print(fit_rpart_caret)

#> ELASTIC NET REGRESSION ====


# preProcValues <- preProcess(training)
# trainTransformed <- predict(preProcValues, training)
# glimpse(trainTransformed)

lambda <- 10^seq(-4, 0, length = 100)

set.seed(41094)
model_ridge <- train(
  log_income_pc ~ ., 
  data = df_train_pre_proc, 
  method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)
# Model coefficients
coef(model_ridge$finalModel, model_ridge$bestTune$lambda)
# Make predictions
predictions <- model_ridge %>% predict(df_test_pre_proc)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, df_test_pre_proc$log_income_pc),
  Rsquare = R2(predictions, df_test_pre_proc$log_income_pc)
)

set.seed(064285)
model_lasso <- train(
  log_income_pc ~ .,
  data = df_train_pre_proc,
  method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)
# Model coefficients

# Make predictions
predictions <- model_lasso %>% predict(df_test_pre_proc)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, df_test_pre_proc$log_income_pc),
  Rsquare = R2(predictions, df_test_pre_proc$log_income_pc)
)
