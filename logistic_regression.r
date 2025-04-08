dataset = read.csv('accident.csv')

# Encode categorical data 
dataset$Gender = factor(dataset$Gender,
                        levels = c('Male', 'Female'),
                        labels = c(0, 1))

dataset$Helmet_Used = factor(dataset$Helmet_Used,
                             levels = c('No', 'Yes'),
                             labels = c(0, 1))

dataset$Seatbelt_Used = factor(dataset$Seatbelt_Used,
                               levels = c('No', 'Yes'),
                               labels = c(0, 1))

# Take care of missing data
dataset$Speed_of_Impact = ifelse(is.na(dataset$Speed_of_Impact),
                                 mean(dataset$Speed_of_Impact, na.rm = TRUE),
                                 dataset$Speed_of_Impact)

dataset <- na.omit(dataset, cols = 
                     "Gender")



#Splitting the dataset inot the training set and test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Survived, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


#Feature scaling
training_scaled_cols = scale(training_set[, 1:2])
training_set[, 1:2] = training_scaled_cols
test_set[, 1:2] = scale(test_set[, 1:2],
                        center = attr(training_scaled_cols, 'scaled:center'),,
                        scale = attr(training_scaled_cols, 'scaled:scale'))















