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


dataset$Survived2= dataset$Survived^2 # Survived is an independent variable 




# building regressor 

regressor = lm(formula = Age ~ ., 
               data = dataset)

summary(regressor)




