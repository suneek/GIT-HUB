# Total no of passengers in Titanic ship embarked : 2224
# Total no of passengers survived : 1502 (From the kaggel information)
# Total No of embarked passenger in Training and Test data : 1309
# passengers survived (Including Training and Test data(predic)) : 493


# Classification template
# Importing libraries
library(caTools)

# Importing the dataset
dataset_orig = read.csv('Titanic_train.csv')
dataset = read.csv('Titanic_train.csv')
# Removed columns name, Ticket, Fare, Cabin, Embarked
dataset = dataset[, -4]
dataset = dataset[,-c(10:11)]
# Removing the Passenger ID for fitting the model
dataset = dataset[, -1]
dataset = dataset[, -7]

# Encoding the target feature as factor
dataset$Survived = factor(dataset$Survived, levels = c(0, 1))





#trim the spaces
dataset$Sex = gsub('\\s+', '', dataset$Sex)

# Converting male and female to 0s and 1s
dataset$Sex = ifelse(dataset$Sex == 'male', 0, 1)

# fill in missing values for Age
dataset$Age[is.na(dataset$Age)] = as.integer(mean(dataset$Age, na.rm = TRUE))


# # Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Purchased, SplitRatio = 0.75)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)




# Finding the significant variables
# summary(glm(Survived ~ .,
#             family = binomial,
#             data = dataset))

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.64561    0.09137  -7.066 1.60e-12 ***
#   Pclass      -0.90785    0.11640  -7.800 6.21e-15 ***
#   Sex          1.32055    0.09509  13.887  < 2e-16 ***
#   Age         -0.51534    0.10138  -5.083 3.71e-07 ***
#   SibSp       -0.38598    0.12058  -3.201  0.00137 ** 
#   Parch       -0.08888    0.09459  -0.940  0.34742    
# Fare         0.14166    0.11730   1.208  0.22714    
# ---
  

# Removing insignificant variables which has less than "***"
dataset = dataset[,-c(5:7)]

# Feature Scaling
dataset[-1] = scale(dataset[-1])

# Finding the significant variables again after removing Parch and Sibsp
# summary(glm(Survived ~ .,
#             family = binomial,
#             data = dataset))

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.64999    0.08984  -7.235 4.66e-13 ***
#   Pclass      -0.97828    0.09957  -9.825  < 2e-16 ***
#   Sex          1.24872    0.08918  14.002  < 2e-16 ***
#   Age         -0.43471    0.09565  -4.545 5.50e-06 ***
#   ---


# Splitting the Training set into the Training set and Validation set
set.seed(789)
split = sample.split(dataset$Survived, SplitRatio = 0.8)
train = subset(dataset, split == TRUE)
test = subset(dataset, split == FALSE)


# --*********************************************
# # Fitting Linear SVM to the Training set
# classifier = svm(Survived ~ .,
#                  data = train,
#                  type = 'C-classification',
#                  kernel = 'linear')
# 
# # Predicting the Validation set results
# y_pred = predict(classifier, newdata = test[,-which(names(test)=="Survived")])
# 
# # Checking the prediction accuracy
# table(test$Survived, y_pred) # Confusion matrix
# ##    y_pred
# ##       0   1
# ##   0 106   4
# ##   1  18  50
# error <- mean(test$Survived != y_pred) # Misclassification error
# paste('Accuracy',round(1-error,4))
# 

#"Accuracy 0.8258"
# --**********************************************

# Fitting classifier to the Training set
# Create your classifier here
# Fitting Logistic Regression to the Training set
classifier = glm(formula = Survived ~ .,
                 family = binomial,
                 data = train)



# Predicting the Validation set results
prob_pred = predict(classifier, type = 'response', newdata = test)
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Checking the prediction accuracy
table(test$Survived, y_pred > 0.5) # Confusion matrix

error <- mean(test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))

#*************************************************


test_set =  read.csv('Titanic_test.csv')
test_set_orig =  read.csv('Titanic_test.csv')


test_set = test_set[, -3]
test_set = test_set[,-c(5:10)]


# Removing spaces in the column
test_set$Sex = gsub('\\s+', '', test_set$Sex)
# fill in missing values for Age
test_set$Age[is.na(test_set$Age)] = mean(test_set$Age, na.rm = TRUE)


# Converting male and female to 0s and 1s
test_set$Sex = ifelse(test_set$Sex == 'male', 0, 1)


# Feature Scaling

test_set[-1] = scale(test_set[-1])

# Predicting the Test set results
test_set_df = data.frame(test_set[,c(2:4)])

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set_df)


test_set$Survived = as.numeric(ifelse(prob_pred > 0.5, 1, 0))
table(test_set$Survived)

# The following is to send the predicted data to Kaggle
Predictions = data.frame(test_set[c("PassengerId","Survived")])
write.csv(file = "TitanicPred_2columns", x = Predictions)

# adding the column "Survived" to the test set after predicting
test_set_orig$Survived = as.numeric(ifelse(prob_pred > 0.5, 1, 0))

dataset_orig$Age[is.na(dataset_orig$Age)] = mean(dataset_orig$Age, na.rm = TRUE)

# The following are to be used by Tableau visualizations
write.csv(file = "TitanicPred", x = test_set_orig)
write.csv(file = "TitanicTrainWithNoNULLS", x = dataset_orig)

# # Visualising the Training set results
# library(ElemStatLearn)
# set = dataset
# X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
# X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
# grid_set = expand.grid(X1, X2)
# colnames(grid_set) = c('Age', 'EstimatedSalary')
# y_grid = predict(classifier, newdata = grid_set)
# plot(set[, -1],
#      main = 'Classifier (Training set)',
#      xlab = 'Age', ylab = 'Estimated Salary',
#      xlim = range(X1), ylim = range(X2))
# contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
# points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
# points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
# 
# # Visualising the Test set results
# library(ElemStatLearn)
# set = test_set
# X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
# X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
# grid_set = expand.grid(X1, X2)
# colnames(grid_set) = c('Age', 'EstimatedSalary')
# y_grid = predict(classifier, newdata = grid_set)
# plot(set[, -3], main = 'Classifier (Test set)',
#      xlab = 'Age', ylab = 'Estimated Salary',
#      xlim = range(X1), ylim = range(X2))
# contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
# points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
# points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))