#> LOAD LIBRARIES ====

library(haven)
library(tidyverse)
library(rattle)


#> LOAD AND WRANGLE DATA ==== 

# Load NIDS wave 1 household data, change variable types and
# create log per household income
df_NIDSsmall <- read_dta("NIDSsmall.dta") %>% 
  as_tibble %>% 
  filter(year == 2008) %>% 
  select(pid, age, age2, educ, educ2, mother_educ, dc, race, urban, female, marital, status, occupation, industry, union, numeracy, tenure, earnings, wages, hours) %>% 
  mutate_at(vars(1:6), as.integer) %>% 
  mutate_at(vars(7:15), as_factor) %>% 
  mutate_at(vars(16:ncol(.)), as.double) %>% 
  mutate(
    log_wage = log(wages),
    log_earnings = log(earnings)
  ) %>% 
  select(-c('wages', 'earnings'))

# Rename som

df_regr <- df_NIDSsmall %>% 
  filter(!is.na(log_wage)) %>% 
  filter(tenure > 0) %>% 
  filter(hours > 0) %>% 
  select(-c('status', 'occupation', 'industry', 'dc', 'female', 'marital', 'race')) %>% 
  select(-c('pid', 'tenure', 'educ2', 'age2', 'log_wage')) %>% 
  mutate_at(vars(1:ncol(.)), as.double) %>% 
  mutate(union = if_else(union == 6, 0, 1)) %>% 
  filter(complete.cases(.))

lm_fit <- lm(data = df_regr, formula = log_earnings ~ .)
summary(lm_fit)

ridge_model <- glmnet::glmnet(y = df_regr$log_earnings, x = df_regr %>% select(-log_earnings) %>% as.matrix, alpha = 0)

tmp <- as.data.frame(as.matrix(coef(ridge_model)))
tmp$coef <- row.names(tmp)
tmp <- reshape::melt(tmp, id = "coef")
tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
tmp$lambda <- ridge_model$lambda[tmp$variable + 1]
ggplot(tmp[tmp$coef != "(Intercept)", ], aes(lambda, value, color = coef, linetype = coef)) + 
  geom_line() +
  scale_x_log10() + 
  guides(color = guide_legend(title = ""),
         linetype = guide_legend(title = "")) + 
  theme_bw() + 
  theme(legend.key.width = unit(3, "lines"))


lasso_model <- glmnet::glmnet(y = df_regr$log_earnings, x = df_regr %>% select(-log_earnings) %>% as.matrix, alpha = 1)

tmp <- as.data.frame(as.matrix(coef(lasso_model)))
tmp$coef <- row.names(tmp)
tmp <- reshape::melt(tmp, id = "coef")
tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
tmp$lambda <- lasso_model$lambda[tmp$variable + 1]
ggplot(tmp[tmp$coef != "(Intercept)", ], aes(lambda, value, color = coef, linetype = coef)) + 
  geom_line() +
  scale_x_log10() + 
  guides(color = guide_legend(title = ""),
         linetype = guide_legend(title = "")) + 
  theme_bw() + 
  theme(legend.key.width = unit(3, "lines"))

model_rpart_simple <- rpart::rpart(
  data = df_regr,
  formula = log_earnings ~ ., 
  method = "anova",
  control = rpart::rpart.control(cp = 0.015)
)

model_rpart_simple
fancyRpartPlot(model_rpart_simple, main = "Regression Tree for NIDS Income")


model_knn <- knnreg(y = df_regr$log_earnings, x = df_regr %>% select(-log_earnings) %>% as.matrix, k = 5)

plot(df_regr$log_earnings, predict(model_knn, df_regr  %>% select(-log_earnings) %>% as.matrix))

