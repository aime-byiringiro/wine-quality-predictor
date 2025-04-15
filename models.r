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

# Removing non-important independent variables
classifier = glm(formula = quality ~ .,
                 family = binomial,
                 data = training_set)
summary(classifier)


#remove density
classifier = glm(formula = quality ~ fixed.acidity + volatile.acidity +  citric.acid + residual.sugar + 
                   chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol,
                 family = binomial,
                 data = training_set)
summary(classifier)

#remove fixed acidity 

classifier = glm(formula = quality ~  volatile.acidity +  citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + 
                   total.sulfur.dioxide +  pH + sulphates + alcohol,
                 family = binomial,
                 data = training_set)
summary(classifier)


#remove citric acid
classifier = glm(formula = quality ~ volatile.acidity + residual.sugar + chlorides + free.sulfur.dioxide + 
                   total.sulfur.dioxide + pH + sulphates + alcohol,
                 family = binomial,
                 data = training_set)
summary(classifier)

#remove residual sugar
classifier = glm(formula = quality ~  volatile.acidity +  chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
                   pH + sulphates + alcohol,
                 family = binomial,
                 data = training_set)
summary(classifier)


#remove ph
classifier = glm(formula = quality ~  volatile.acidity +  chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
                   sulphates + alcohol,
                 family = binomial,
                 data = training_set)
summary(classifier)


# The final features are: volatile.acidity, chlorides, free.sulfur.dioxide, total.sulfur.dioxide, sulphates, alcohol


# MODELS

### LOGISTIC REGRESSION ###
# Fitting Logistic Regression to the Training set

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set)
y_pred = as.factor(ifelse(prob_pred > 0.5, 1, 0))

# Showing the Confusion Matrix and Accuracy
library(caret)
cm = confusionMatrix(y_pred, test_set$quality)
print(cm$table)
print(cm$overall['Accuracy'])


### KNN ###
library(kknn)

# Fitting k-NN to the Training set and Predicting the Test set results
classifier = kknn(formula = quality ~ volatile.acidity +  chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
                    sulphates + alcohol, train = training_set, test = test_set,
                  k = 34, distance = 2)
y_pred = classifier$fitted.values

# Showing the Confusion Matrix and Accuracy
library(caret)
cm = confusionMatrix(y_pred, test_set$quality)
print(cm$table)
print(cm$overall['Accuracy'])

# Visualizing the Training set results
set = training_set
X1 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
X2 = seq(min(set[, 11]) - 1, max(set[, 11]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('volatile.acidity', 'alcohol')
classifier = kknn(formula = quality ~ volatile.acidity + alcohol, train = training_set, test = grid_set,
                  k = 17, distance = 2)
y_grid = classifier$fitted.values
plot(NULL,
     main = 'k-NN (Training set)',
     xlab = 'Alcohol', ylab = 'Quality (Scaled)',
     xlim = range(X1), ylim = range(X2))
points(grid_set, pch = 20, col = c('tomato', 'springgreen3')[y_grid])
points(set, pch = 21, bg = c('red3', 'green4')[set$quality])

# Visualizing the Test set results
set = test_set
X1 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
X2 = seq(min(set[, 11]) - 1, max(set[, 11]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('volatile.acidity', 'alcohol')
classifier = kknn(formula = quality ~ volatile.acidity + alcohol, train = training_set, test = grid_set,
                  k = 17, distance = 2)
y_grid = classifier$fitted.values
plot(NULL,
     main = 'k-NN (Test set)',
     xlab = 'Alcohol', ylab = 'Quality (Scaled)',
     xlim = range(X1), ylim = range(X2))
points(grid_set, pch = 20, col = c('tomato', 'springgreen3')[y_grid])
points(set, pch = 21, bg = c('red3', 'green4')[set$quality])


### SVM ###
library(e1071)
classifier = svm(formula = quality ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set)

# Showing the Confusion Matrix and Accuracy
library(caret)
cm = confusionMatrix(y_pred, test_set$quality)
print(cm$table)
print(cm$overall['Accuracy'])
