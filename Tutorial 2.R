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
) %>% 
  mutate_at(vars(6:ncol(.)), as.logical) %>% 
  
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


model_rpart_caret <- caret::train(
  log_income_pc ~ ., 
  data = df_train,
  method = 'rpart', 
  tuneLength = 10,
  control = rpart::rpart.control(minsplit = 5, minbucket = 5),
  trControl = fitControl
)
model_rpart_caret

model_rpart_caret$finalModel

df_predict_rpart_caret <- model_rpart_caret$finalModel %>% 
  predict(df_test)

fit_rpart_caret <- data.frame(
  RMSE = RMSE(df_predict_rpart_caret, df_test$log_income_pc),
  Rsquare = R2(df_predict_rpart_caret, df_test$log_income_pc)
)
print(fit_rpart_caret)


