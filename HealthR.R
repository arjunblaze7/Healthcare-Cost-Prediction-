#Setting up environment
library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(randomForest)
library(Hmisc)
library(cowplot)

#Importing data
Data <- read.csv('C:/Users/Arjun/Desktop/Healthcare/insurance.csv',stringsAsFactors = F)
str(Data)

#Correlations Scatter plot
a <- ggplot(Data, aes(age, charges)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

b <- ggplot(Data, aes(bmi, charges)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

c <- ggplot(Data, aes(sex, charges)) +
  geom_jitter(color = "black", alpha = 0.5) +
  theme_light()

d <- ggplot(Data, aes(children, charges)) +
  geom_jitter(color = "red", alpha = 0.5) +
  theme_light()

e <- ggplot(Data, aes(smoker, charges)) +
  geom_jitter(color = "yellow", alpha = 0.5) +
  theme_light()

f <- ggplot(Data, aes(region, charges)) +
  geom_jitter(color = "purple", alpha = 0.5) +
  theme_light()

p <- plot_grid(a,b,c,d,e,f) 
title <- ggdraw() + draw_label(" Correlations ", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


#Regression model
n_train <- round(0.8 * nrow(Data))
train_indices <- sample(1:nrow(Data), n_train)
Data_train <- Data[train_indices, ]
Data_test <- Data[-train_indices, ]
formula_0 <- as.formula("charges ~ age + sex + bmi + children + smoker + region")
model_0 <- lm(formula_0, data = Data_train)
summary(model_0)
#Saving R-squared
r_sq_0 <- summary(model_0)$r.squared
#predict data on test set
prediction_0 <- predict(model_0, newdata = Data_test)
#calculating the residuals
residuals_0 <- Data_test$charges - prediction_0
#calculating Root Mean Squared Error
rmse_0 <- sqrt(mean(residuals_0^2))
formula_1 <- as.formula("charges ~ age + bmi + children + smoker + region")
model_1 <- lm(formula_1, data = Data_train)
summary(model_1)
r_sq_1 <- summary(model_1)$r.squared
prediction_1 <- predict(model_1, newdata = Data_test)
residuals_1 <- Data_test$charges - prediction_1
rmse_1 <- sqrt(mean(residuals_1^2))

#Model comparision
Data_test$prediction <- predict(model_1, newdata = Data_test)
ggplot(Data_test, aes(x = prediction, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")

# Applying on new data
Bob <- data.frame(age = 19,
                  bmi = 27.9,
                  children = 0,
                  smoker = "yes",
                  region = "northwest")
print(paste0("Health care charges for Bob: ", round(predict(model_1, Bob), 2)))

