# Determine the likelihood of a car crash victim surviving based on age, gender, speed, helmet, 
# and seatbelt use. 

# Importing the dataset
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

# Models
# Multiple Linear Regression (Kate)
# Random Forest Regression (Katie)
# Decision Tree Regression (Aime)