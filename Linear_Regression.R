# Linear Regression 
# Prerequisites 
library(tidyverse) # helper package for data manipulation and visualization

# Modeling packages
library(caret)    # for cross-validation, etc.

# Model interpretability packages
library(vip)      # variable importance
options(digits = 3)


#=============================================================================
# In order for a health insurance company to make money, it needs to collect 
# more in yearly premiuns than it spends on medical care to its beneficiares.
# Consequently, insurers invest a great deal of time an money to develop models
# that accurately forecast medical expenses for the insured population.
# Medical expenses are difficult to estimate because the costliest conditions
# are rare and seemingly random.
# Predicting medical expenses using linear regression
# The goal of this analysis is to use patient data to forecast the
# average medical care expenses for such population segments. These estimates 
# could be used to create actuarial tables that set the price of yearly premiums
# higher or lower according to the expected treatment costs.
# 
# outcome variable : expenses - measures the medical costs each person charged 
# to the insurance plan for a year.
#
## Example: Predicting Medical Expenses ----
## Step 1: Exploring and preparing the data ----

insurance <- read.csv("./data/insurance.csv", stringsAsFactors = T) # dataset
View(insurance)
glimpse(insurance)

insurance %>% ggplot(aes(x = expenses, y = ..count..)) +
              geom_density(stat = "density", position = "identity", 
                           color = 'red', size = 1.5) + 
              scale_x_continuous(trans = "log10") +
              theme_bw()

insurance %>% select(age, bmi, children, expenses) %>%
             GGally::ggpairs(., aes(colour = insurance$sex),
                             title = "Correlogram with ggpairs")
library(corrgram)
correlation <- cor(insurance[c("age", "bmi", "children", "expenses")])
pairs(insurance[c("age", "bmi", "children", "expenses")])

corrgram(correlation, order = T, lower.panel = panel.shade, upper.panel = panel.pie, 
         text.panel = panel.txt, main = "Correlação entre as variáveis numéricas")

ggcorrplot::ggcorrplot(correlation, method = "circle", hc.order = T,
                       outline.color = "black", type = 'lower', lab = T,
                       ggtheme = ggplot2::theme_bw())

table(insurance$region)

# Spliting data using caret package
y <- insurance$expenses
set.seed(123, sample.kind = "Rounding")
index <- createDataPartition(y, times = 1, p = 0.7, list = F)
training <- insurance[index,]
test <- insurance[-index,]

# Train a model using 10-fold cross-validation
set.seed(123)

(lm_model1 <- train(expenses ~ age + children + bmi + sex + region, 
              data = training, method = "lm",
              trControl = trainControl(method = "cv", number = 10)))

summary(lm_model1) %>%
        broom::tidy() 
# Model evaluation : Regression models 
# MSE: Mean square error is the average of the square error - objective:minimize
# RMSE: Root mean squared error. objetive: minimize
# Deviance: short for mean residual deviance. objective: minimize
# MAE: Mean absolute error.objective: minimize
# R^2: popular metric that represents the proportion of the variance in the de-
# pendent variable that is predictable from the independent variable(s).
# 
summary(resamples(list(
    model1 = lm_model1,
    model2 = lm_model2,
)))


df1 <- broom::augment(lm_model1$finalModel, data = training)
p1 <- ggplot(df1, aes(.fitted, .resid)) +
  geom_point(size = 1, alpha = 0.4) +
  xlab("Predicted values") + 
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "expenses ~ all predictors")

# Improving model performance
# add a age square to the model, because as we now as long someone gets old the
# medical expenses increase non linear
# 1- formula = expenses ~ age + age2
     training <- training %>% mutate(age2 = age^2)
     test <- test %>% mutate(age2 = age^2)
# 2 - add a new feature conect to obesity indicator
     training <- training %>%
               mutate(bmi30 = factor(ifelse(bmi >= 30, 1, 0)))
     test <- test %>% mutate(bmi30 = factor(ifelse(bmi >= 30, 1, 0)))
# 3- adding interation effects
     bmi30*smoker
     
# Second model  
     
(lm_model2 <- train(expenses ~ age + age2 + children + bmi + sex +
                      bmi30*smoker + region, 
              data = training, method = "lm",
              trControl = trainControl(method = "cv", number = 10)))
summary(lm_model2) %>%
       broom::tidy() 

df2 <- broom::augment(lm_model2$finalModel, data = training)
p2 <- ggplot(df2, aes(.fitted, .resid)) +
  geom_point(size = 1, alpha = 0.4) +
  xlab("Predicted values") + 
  ylab("Residuals") +
  ggtitle("Model 2", subtitle = "expenses ~ age2 + bmi30*smoker")


gridExtra::grid.arrange(p1, p2, nrow = 1)

df1 <- mutate(df1, id = row_number())
df2 <- mutate(df2, id = row_number())
p1 <- ggplot(df1, aes(id, .resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Row ID") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "Correlated residuals.")

p2 <- ggplot(df2, aes(id, .resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Row ID") +
  ylab("Residuals") +
  ggtitle("Model 2", subtitle = "Uncorrelated residuals.")

gridExtra::grid.arrange(p1, p2, nrow = 1)

summary(lm_model2) %>%
  broom::tidy() 

y_hat <- predict(lm_model2, newdata = test)
cor(y_hat, test$expenses)
plot(y_hat, test$expenses)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)
