# Chapter 2 - Modeling Process
library(tidyverse) # helper packages

# Modeling process packages
library(rsample)   # for resampling procedures
library(caret)     # for resampling and model training
library(h2o)       # for resampling and model training

# h2o set-up 
h2o.no_progress()  # turn off h2o progress bars
h2o.init()         # launch h2o

# Ames housing data
ames <- AmesHousing::make_ames()
ames.h2o <- as.h2o(ames)

# Job attrition data
churn <- rsample::attrition %>% 
  mutate_if(is.ordered, .funs = factor, ordered = FALSE)
churn.h2o <- as.h2o(churn)

# Using base R
set.seed(123)  # for reproducibility
index_1 <- sample(1:nrow(ames), round(nrow(ames) * 0.7))
train_1 <- ames[index_1, ]
test_1  <- ames[-index_1, ]

# Using caret package
set.seed(123)  # for reproducibility
index_2 <- createDataPartition(ames$Sale_Price, p = 0.7, 
                               list = FALSE)
train_2 <- ames[index_2, ]
test_2  <- ames[-index_2, ]

# Using rsample package
set.seed(123)  # for reproducibility
split_1  <- initial_split(ames, prop = 0.7)
train_3  <- training(split_1)
test_3   <- testing(split_1)

# Using h2o package
split_2 <- h2o.splitFrame(ames.h2o, ratios = 0.7, 
                          seed = 123)
train_4 <- split_2[[1]]
test_4  <- split_2[[2]]

# orginal response distribution
table(churn$Attrition) %>% prop.table()
## 
##        No       Yes 
## 0.8387755 

# stratified sampling with the rsample package
set.seed(123)
split_strat  <- initial_split(churn, prop = 0.7, 
                              strata = "Attrition") #
train_strat  <- training(split_strat)
test_strat   <- testing(split_strat)
# consistent response ratio between train & test
table(train_strat$Attrition) %>% prop.table()
## 
##       No      Yes 
## 0.838835 0.161165
table(test_strat$Attrition) %>% prop.table()
## 
##        No       Yes 
## 0.8386364 0.1613636
# 
# meta Engines
# lm_caret <- train(Sale_Price ~ ., data = ames, method = "lm")
# Example using h2o
h2o.cv <- h2o.glm(
  x = x, 
  y = y, 
  training_frame = ames.h2o,
  nfolds = 10  # perform 10-fold CV
)
# A bootstrap sample is a random sample of the data taken with 
# replacement 

bootstraps(ames, times = 10)

# Stratified sampling with the rsample package
set.seed(123)
split <- initial_split(ames, prop = 0.7, 
                       strata = "Sale_Price")
ames_train  <- training(split)
ames_test   <- testing(split)

# Specify resampling strategy
cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 5
)
# Create grid of hyperparameter values
hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

# Tune a knn model using grid search
knn_fit <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "RMSE"
)
ggplot(knn_fit, highlight = T)
