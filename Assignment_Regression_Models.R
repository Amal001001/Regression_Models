# Multiple Linear Regression

install.packages("tidyverse")
library(tidyr)  #for data tidying.
library(dplyr)  #for data manipulation.

# Importing the dataset

# The 1st step is setting your working directory "location of your dataset in your device"
dataset = read.csv('Boston_Housing_Dataset.csv')

View(dataset)


# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$target, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


View(training_set)
View(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = target ~ .,
               data = training_set)

# In the summary focus on the coefficients section
# Note: the more the stars the more each coefficient is statistically significant 
# Another indication of statistical significance if the p-value is less than 5%

# Note: Significance level interpretation "we have 5 categories:
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
summary(regressor)

# the features 'RM','LSTAT','PTRATIO', 'DIS' and 'RAD' were the most important by R.

# To get the coefficients 
regressor$coefficients

# Predicting the Test set results
y_pred <- predict(regressor, newdata = test_set)
y_pred
y_actual <- test_set$target

error <- y_pred - test_set$target 
percent_error <- abs(error)/y_pred

percent_error <- round(percent_error,2)

df_multi <- data.frame(y_pred, y_actual, error, percent_error)
View(df_multi)


# Lets try building our model with columns 'RM' 'LSTAT' 'PTRATIO' 'DIS' 'RAD'

target <- dataset$target
new_dataset <- data.frame(research_spend, target)

View(new_dataset)

set.seed(123)

split = sample.split(new_dataset$target, SplitRatio = 0.8)
simple_training_set = subset(new_dataset, split == TRUE)
simple_test_set = subset(new_dataset, split == FALSE)

View(simple_training_set)
View(simple_test_set)

simple_regressor = lm(formula = target ~ RM +LSTAT +PTRATIO +DIS +RAD,
                      data = simple_training_set)

summary(simple_regressor)

simple_y_pred <- predict(simple_regressor, newdata = simple_test_set)
simple_y_pred

simple_error <- simple_y_pred - simple_test_set$target
simple_percent_error <- abs(simple_error)/simple_y_pred

simple_percent_error <- round(simple_percent_error,2)

comparison_df <- data.frame(df_multi, simple_y_pred, simple_percent_error)
View(comparison_df)

#------------------------------------------------------------------------------------------------------------
# Visualizing
install.packages('ggplot2')
library(ggplot2)

# LSTAT

# Visualizing the Training set results
ggplot() +
  geom_point(aes(x = simple_training_set$LSTAT, y = simple_training_set$target),
             colour = 'red') +
  geom_line(aes(x = simple_training_set$LSTAT, y = predict(simple_regressor, newdata = simple_training_set)),
            colour = 'blue') +
  ggtitle('LSTAT vs target (Training set)') +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('target') +
  ylab('LSTAT')


# Visualizing the Test set results
ggplot() +
  geom_point(aes(x = simple_test_set$LSTAT, y = simple_test_set$target),
             colour = 'red') +
  geom_line(aes(x = simple_training_set$LSTAT, y = predict(simple_regressor, newdata = simple_training_set)),
            colour = 'blue') +
  ggtitle('LSTAT vs target (Test set)') + 
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('target') +
  ylab('LSTAT')

#----------------
# 'RM'

# Visualizing the Training set results
ggplot() +
  geom_point(aes(x = simple_training_set$RM, y = simple_training_set$target),
             colour = 'red') +
  geom_line(aes(x = simple_training_set$RM, y = predict(simple_regressor, newdata = simple_training_set)),
            colour = 'blue') +
  ggtitle('RM vs target (Training set)') +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('target') +
  ylab('RM')


# Visualizing the Test set results
ggplot() +
  geom_point(aes(x = simple_test_set$RM, y = simple_test_set$target),
             colour = 'red') +
  geom_line(aes(x = simple_training_set$RM, y = predict(simple_regressor, newdata = simple_training_set)),
            colour = 'blue') +
  ggtitle('RM vs target (Test set)') + 
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('target') +
  ylab('RM')

#----------------
# 'PTRATIO'

# Visualizing the Training set results
ggplot() +
  geom_point(aes(x = simple_training_set$PTRATIO, y = simple_training_set$target),
             colour = 'red') +
  geom_line(aes(x = simple_training_set$PTRATIO, y = predict(simple_regressor, newdata = simple_training_set)),
            colour = 'blue') +
  ggtitle('PTRATIO vs target (Training set)') +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('target') +
  ylab('PTRATIO')


# Visualizing the Test set results
ggplot() +
  geom_point(aes(x = simple_test_set$PTRATIO, y = simple_test_set$target),
             colour = 'red') +
  geom_line(aes(x = simple_training_set$PTRATIO, y = predict(simple_regressor, newdata = simple_training_set)),
            colour = 'blue') +
  ggtitle('PTRATIO vs target (Test set)') + 
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('target') +
  ylab('PTRATIO')

#----------------
# 'DIS' 'RAD'

# Visualizing the Training set results
ggplot() +
  geom_point(aes(x = simple_training_set$DIS, y = simple_training_set$target),
             colour = 'red') +
  geom_line(aes(x = simple_training_set$DIS, y = predict(simple_regressor, newdata = simple_training_set)),
            colour = 'blue') +
  ggtitle('DIS vs target (Training set)') +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('target') +
  ylab('DIS')


# Visualizing the Test set results
ggplot() +
  geom_point(aes(x = simple_test_set$DIS, y = simple_test_set$target),
             colour = 'red') +
  geom_line(aes(x = simple_training_set$DIS, y = predict(simple_regressor, newdata = simple_training_set)),
            colour = 'blue') +
  ggtitle('DIS vs target (Test set)') + 
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('target') +
  ylab('DIS')

#----------------
# 'RAD'

# Visualizing the Training set results
ggplot() +
  geom_point(aes(x = simple_training_set$RAD, y = simple_training_set$target),
             colour = 'red') +
  geom_line(aes(x = simple_training_set$RAD, y = predict(simple_regressor, newdata = simple_training_set)),
            colour = 'blue') +
  ggtitle('RAD vs target (Training set)') +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('target') +
  ylab('RAD')


# Visualizing the Test set results
ggplot() +
  geom_point(aes(x = simple_test_set$RAD, y = simple_test_set$target),
             colour = 'red') +
  geom_line(aes(x = simple_training_set$RAD, y = predict(simple_regressor, newdata = simple_training_set)),
            colour = 'blue') +
  ggtitle('RAD vs target (Test set)') + 
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('target') +
  ylab('RAD')

#-------------------------------------------------------------------------------------------------------------
# 3. Try to improve your scores as much as possible and report which features were deemed most important by R

summary(simple_regressor)$r.squared

# my score is: 0.7077524

# the features 'RM','LSTAT','PTRATIO' and 'DIS' were the most important by R.

