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



dataset$Speed2 = dataset$Speed_of_Impact^2
dataset$Speed3 = dataset$Speed_of_Impact^3


# building regressor 

regressor1 = lm(formula = Survived ~ ., 
               data = dataset)

summary(regressor)


regressor2 = lm(formula = Survived ~ Age + Gender + Speed_of_Impact + Helmet_Used + Seatbelt_Used + Speed2 +Speed3, 
               data = dataset)

summary(regressor2)

regressor3= lm(formula = Survived ~ Age + Gender + Speed_of_Impact + Helmet_Used + Seatbelt_Used + Speed2, 
                data = dataset)

summary(regressor3)

regressor4 = lm(formula = Survived ~ Age + Gender + Speed_of_Impact + Helmet_Used + Seatbelt_Used, 
               data = dataset)

summary(regressor4)

#The remaining are categorical and age is the closed to 0.05 thought it's 0.06

regressor5 = lm(formula = Survived ~ Age + Gender + Helmet_Used + Seatbelt_Used, 
                data = dataset)

summary(regressor5)




















