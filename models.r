# Problem: Predict the quality of red wine based on sugar, acidity, and other factors. 

# Importing the dataset
dataset = read.csv('winequality.csv')

dataset$quality = as.factor(ifelse(dataset$quality >= 6, 1, 0))

# Splitting the dataset into the Training set and Test set
library(caTools)
split = sample.split(dataset$quality, SplitRatio = 3/4)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling 
training_scaled_cols = scale(training_set[, 1:11])
training_set[, 1:11] = training_scaled_cols
test_set[, 1:11] = scale(test_set[, 1:11],
                         center = attr(training_scaled_cols, 'scaled:center'),
                         scale = attr(training_scaled_cols, 'scaled:scale'))

# MODELS

# KNN
library(kknn)

# Fitting k-NN to the Training set and Predicting the Test set results
classifier = kknn(formula = quality ~ ., train = training_set, test = test_set,
                  k = 7, distance = 2)
y_pred = classifier$fitted.values

# Showing the Confusion Matrix and Accuracy
library(caret)
cm = confusionMatrix(y_pred, test_set$quality)
print(cm$table)
print(cm$overall['Accuracy'])


# LOGISTIC REGRESSION


# Fitting Logistic Regression to the Training set
classifier = glm(formula = quality ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set)
y_pred = as.factor(ifelse(prob_pred > 0.5, 1, 0))

# Showing the Confusion Matrix and Accuracy
library(caret)
cm = confusionMatrix(y_pred, test_set$quality)
print(cm$table)
print(cm$overall['Accuracy'])



