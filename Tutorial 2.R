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

#> SPLIT SAMPLE ====
set.seed(7483831)

train_index = createDataPartition(
  y = df_NIDS_dummies$log_income_pc, 
  p = 0.75, 
  list = F
  )

df_train = df_NIDS_dummies[train_index,]
df_test = df_NIDS_dummies[-train_index,]


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

plot(model_rpart_simple, uniform=TRUE, 
     main="Regression Tree for NIDS Income")
text(model_rpart_simple, use.n=TRUE, all=TRUE, cex=.8)

rpart(income ~ ., method="anova", data=training,control=rpart.control(minsplit = 20, minbucket = round(20/3), cp = 0.0008, 
                                                                      maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                                                                      surrogatestyle = 0, maxdepth = 30))

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
  log_income_pc ~ ., 
  data = df_train,
  method = 'rpart', 
  trControl = fitControl,
  tuneLength = 10
  )

model_rpart_caret

cp_grid <- 10^seq(-4,-1, length = 30)

model_rpart_caret <- caret::train(
  log_income_pc ~ ., 
  data = df_train,
  method = 'rpart', 
  trControl = fitControl, 
  tuneGrid = expand.grid(cp = cp_grid)
)

model_rpart_caret

fitControl <- trainControl(
  method = "cv", 
  number = 10, 
  verboseIter = TRUE,
  search = "random",
  minsplit = 10, 
)
model_rpart_caret <- caret::train(
  log_income_pc ~ ., 
  data = df_train,
  method = 'rpart', 
  tuneLength = 100,
  trControl = fitControl
)
model_rpart_caret


model_rpart_caret <- caret::train(
  log_income_pc ~ ., 
  data = df_train,
  method = 'rpart', 
  tuneLength = 100,
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

#> ELASTIC NET REGRESSION ====

# preProcValues <- preProcess(training)
# trainTransformed <- predict(preProcValues, training)
# glimpse(trainTransformed)

lambda <- 10^seq(-3, 3, length = 100)

set.seed(123)
ridge <- train(
  formula = income ~ ., data = training, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)
# Model coefficients
coef(ridge$finalModel, ridge$bestTune$lambda)
# Make predictions
predictions <- ridge %>% predict(testing)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, testing$income),
  Rsquare = R2(predictions, testing$income)
)

set.seed(123)
lasso <- train(
  formula = log_income_pc ~ .,
  data = training, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)
# Model coefficients
coef(lasso$finalModel, lasso$bestTune$lambda)
# Make predictions
predictions <- lasso %>% predict(testing)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, testing$income),
  Rsquare = R2(predictions, testing$income)
)
