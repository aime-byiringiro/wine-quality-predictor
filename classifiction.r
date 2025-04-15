# Importing the dataset
dataset = read.csv('winequality.csv')

# Splitting the dataset into the Training set and Test set
library(caTools)
split = sample.split(dataset$quality, SplitRatio = 3/4)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Random Forest
library(randomForest)
regressor = randomForest(formula = quality ~ .,
                         data = training_set,
                         ntree = 500)

# Predict y
y_pred = predict(regressor, newdata = test_set)

# Calculate R-squared
ssr = sum((test_set$quality - y_pred) ^ 2)
sst = sum((test_set$quality - mean(test_set$quality)) ^ 2)
r2 = 1 - (ssr/sst)
print(r2)
r2_adjusted = 1 - (1 - r2) * (length(test_set$quality) - 1) / (length(test_set$quality) - 11 - 1)
print(r2_adjusted)



######################################################################################
        #POLYNOMIAL
#####################################################################################



dataset = read.csv('winequality.csv')

#free sulfur dioxide
#ph 
# the above variables increase as the degree increase 

regressor = lm(formula = quality ~ .,
               data = dataset)

summary(regressor)

dataset$quality = dataset$quality^2
regressor = lm(formula = quality ~ ., 
               data = dataset)

summary(regressor)

dataset$quality = dataset$quality^3
regressor = lm(formula = quality ~ ., 
               data = dataset)

summary(regressor)
