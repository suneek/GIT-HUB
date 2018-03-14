# Classification template
# Importing libraries
# Load all the packages required for the analysis
library(dplyr) # Data Manipulation
library(Amelia) # Missing Data: Missings Map
library(ggplot2) # Visualization
library(scales) # Visualization
library(caTools) # Prediction: Splitting Data
library(car) # Prediction: Checking Multicollinearity
library(ROCR) # Prediction: ROC Curve
library(e1071) # Prediction: SVM, Naive Bayes, Parameter Tuning
library(rpart) # Prediction: Decision Tree
library(rpart.plot) # Prediction: Decision Tree
library(randomForest) # Prediction: Random Forest
library(caret) # Prediction: k-Fold Cross Validation

# Importing the dataset
dataset_orig = read.csv('Titanic_train.csv')
dataset = read.csv('Titanic_train.csv')


# fill in missing values for Age
dataset$Age[is.na(dataset$Age)] = as.integer(mean(dataset$Age, na.rm = TRUE))





# Splitting the Training set into the Training set and Validation set
set.seed(789)
split = sample.split(dataset$Survived, SplitRatio = 0.8)
train = subset(dataset, split == TRUE)
test = subset(dataset, split == FALSE)


# Fitting classifier to the Training set
# Create your classifier here
# Decision Tree classifier
classifier = rpart(Survived ~ ., data = train, method = 'class')



# Tree Visualization
rpart.plot(classifier, extra=4)



# Predicting the Validation set results
y_pred = predict(classifier, newdata = test[,-which(names(test)=="Survived")], type ='class')

# Checking the prediction accuracy
table(test$Survived, y_pred) # Confusion matrix
##    y_pred
##       0   1
##   0 102   8
##   1  21  47
error <- mean(test$Survived != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))