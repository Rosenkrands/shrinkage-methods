library(tidyverse)
library(glmnet)
set.seed(123)

train.ids <- sample(1:nrow(mtcars), round(nrow(mtcars)*.8), replace = FALSE) 

train <- mtcars[train.ids,]
test <- mtcars[-train.ids,]

linear.model <- lm(mpg ~ ., data = train)
linear.pred <- list(
  train = predict(linear.model, train),
  test = predict(linear.model, test)
)

tune.lambda <- function(alpha, lambda_seq){
  mae_seq <- c()
  for (lambda in lambda_seq){
    model <- glmnet(as.matrix(select(train,-mpg)), as.matrix(select(train,mpg)), alpha = alpha, lambda = lambda)
    pred <- predict(model, as.matrix(select(train, -mpg)))
    mae <- tibble(real = train$mpg, pred = pred) %>%
      summarise(mae = mean(abs(real - pred)))
    mae_seq <- c(mae_seq, mae$mae[1])
  }
  return(tibble(lambda = lambda_seq, mae = mae_seq, alpha = alpha))
}

ridge.tune <- tune.lambda(alpha = 0, lambda_seq = seq(0, .25, .01))
lasso.tune <- tune.lambda(alpha = 1, lambda_seq = seq(0, .25, .01))  

tune.result <- bind_rows(ridge.tune, lasso.tune) 
tune.result %>%
  mutate(alpha = factor(alpha)) %>%
  ggplot(aes(x = lambda, y = mae)) +
    geom_line(aes(color = alpha)) +
    theme_bw()

lambda.vals <- tune.result %>%
  group_by(alpha) %>%
  filter(mae == min(mae))

ridge.model <- glmnet(as.matrix(select(train,-mpg)), as.matrix(select(train,mpg)), alpha = 0, lambda = .1)
ridge.pred <- list(
  train = predict(ridge.model, as.matrix(select(train, -mpg))),
  test = predict(ridge.model, as.matrix(select(test, -mpg)))
)

lasso.model <- glmnet(as.matrix(select(train,-mpg)), as.matrix(select(train,mpg)), alpha = 1, lambda = .1)
lasso.pred <- list(
  train = predict(lasso.model, as.matrix(select(train, -mpg))),
  test = predict(lasso.model, as.matrix(select(test, -mpg)))
)

pred.data <- tibble(real = test$mpg, pred = linear.pred$test, model = 'linear', data.set = 'test') %>%
  bind_rows(tibble(real = test$mpg, pred = ridge.pred$test, model = 'ridge', data.set = 'test')) %>%
  bind_rows(tibble(real = test$mpg, pred = lasso.pred$test, model = 'lasso', data.set = 'test')) %>%
  bind_rows(tibble(real = train$mpg, pred = linear.pred$train, model = 'linear', data.set = 'train')) %>%
  bind_rows(tibble(real = train$mpg, pred = ridge.pred$train, model = 'ridge', data.set = 'train')) %>%
  bind_rows(tibble(real = train$mpg, pred = lasso.pred$train, model = 'lasso', data.set = 'train')) %>%
  mutate(model = factor(model, levels = c('linear', 'ridge', 'lasso'))) %>%
  mutate(data.set = factor(data.set, levels = c('train', 'test')))

pred.performance <- pred.data %>%
  group_by(model, data.set) %>%
  summarise(mae = mean(abs(real - pred)))

pred.performance %>%
  ggplot(aes(x = model, y = mae)) +
    geom_point(aes(color = data.set)) +
    theme_bw() +
    labs(x = 'Model', y='Mean Absolute Error')

pred.data %>%  
  ggplot(aes(x = real, y = pred)) +
    geom_abline(size = 1) +
    geom_point(aes(color = model, alpha = data.set)) +
    theme_bw() +
    labs(x = 'Observed values', y = 'Predicted values')

