dataset = read.csv('accident.csv')

# Encode categorical data 
dataset$Gender = factor(dataset$Gender,
                        levels = c('Male', 'Female'),
                        labels = c(0, 1))
dataset$Gender = as.numeric(as.character(dataset$Gender))


dataset$Helmet_Used = factor(dataset$Helmet_Used,
                             levels = c('No', 'Yes'),
                             labels = c(0, 1))

dataset$Helmet_Used = as.numeric(as.character(dataset$Helmet_Used))

dataset$Seatbelt_Used = factor(dataset$Seatbelt_Used,
                               levels = c('No', 'Yes'),
                               labels = c(0, 1))

dataset$Seatbelt_Used = as.numeric(as.character(dataset$Seatbelt_Used))

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


numeric_cols <- sapply(training_set, is.numeric)
non_numeric_cols <- !numeric_cols

names(training_set)[numeric_cols]
names(training_set)[non_numeric_cols]


#Feature scaling
training_scaled_cols = scale(training_set)
training_set = training_scaled_cols
test_set = scale(test_set,
                        center = attr(training_scaled_cols, 'scaled:center'),,
                        scale = attr(training_scaled_cols, 'scaled:scale'))

#Error in scale(test_set, center = attr(training_scaled_cols, "scaled:center"),  : 
                 #unused argument (alist())













