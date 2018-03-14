# Data Preprocessing Template

# Importing the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
plt.rc("font", size=14)
import seaborn as sns
sns.set(style="white") #white background style for seaborn plots
sns.set(style="whitegrid", color_codes=True)


# Importing the dataset
train_dataset = pd.read_csv('Titanic_train.csv')


# Finding and taking care of missing values and dropping a column of least significance
# and assigning to a new dataframe train_data
train_dataset.isnull().sum()


train_data = train_dataset.copy()
train_data["Age"].fillna(train_dataset["Age"].median(skipna=True), inplace=True)
train_data["Embarked"].fillna(train_dataset['Embarked'].value_counts().idxmax(), inplace=True)
train_data.drop('Cabin', axis=1, inplace=True)

train_data.isnull().sum()

# Finding and Trimming leading and trailing spaces in string data
# For practise purpose, i added leading spaces to sex in the original dataset
df_obj = train_data.select_dtypes(['object'])
train_data[df_obj.columns] = df_obj.apply(lambda x: x.str.strip())


## Create categorical variable for traveling alone
train_data['TravelAlone']=np.where((train_data["SibSp"]+train_data["Parch"])>0, 0, 1)
train_data.drop('SibSp', axis=1, inplace=True)
train_data.drop('Parch', axis=1, inplace=True)


#create categorical variables and drop some variables
training=pd.get_dummies(train_data, columns=["Pclass","Embarked","Sex"])
training.drop('Sex_female', axis=1, inplace=True)
training.drop('PassengerId', axis=1, inplace=True)
training.drop('Name', axis=1, inplace=True)
training.drop('Ticket', axis=1, inplace=True)

final_train = training


# Apply the same changes to Test data
test_dataset = pd.read_csv('Titanic_test.csv')

test_dataset.isnull().sum()



test_data = test_dataset.copy()
test_data["Age"].fillna(test_dataset["Age"].median(skipna=True), inplace=True)
test_data["Fare"].fillna(test_dataset["Fare"].median(skipna=True), inplace=True)
test_data.drop('Cabin', axis=1, inplace=True)

df_obj = test_data.select_dtypes(['object'])
test_data[df_obj.columns] = df_obj.apply(lambda x: x.str.strip())

test_data['TravelAlone']=np.where((test_data["SibSp"]+test_data["Parch"])>0, 0, 1)

test_data.drop('SibSp', axis=1, inplace=True)
test_data.drop('Parch', axis=1, inplace=True)

testing = pd.get_dummies(test_data, columns=["Pclass","Embarked","Sex"])
testing.drop('Sex_female', axis=1, inplace=True)
testing.drop('PassengerId', axis=1, inplace=True)
testing.drop('Name', axis=1, inplace=True)
testing.drop('Ticket', axis=1, inplace=True)

final_test = testing


# Adding another categorical variable "Minor"
final_train['IsMinor']=np.where(final_train['Age']<=16, 1, 0)

final_test['IsMinor']=np.where(final_test['Age']<=16, 1, 0)


#Exploratory Analysis
sns.barplot('Pclass', 'Survived', data=train_dataset, color="darkturquoise")
plt.show()

sns.barplot('Embarked', 'Survived', data=train_dataset, color="teal")
plt.show()

sns.barplot('TravelAlone', 'Survived', data=final_train, color="mediumturquoise")
plt.show()

sns.barplot('Sex', 'Survived', data=train_dataset, color="aquamarine")
plt.show()


sns.barplot('IsMinor', 'Survived', data=final_train, color="aquamarine")
plt.show()


#Recursive feature elimination : recursive feature elimination (RFE) is to select features by 
# recursively considering smaller and smaller sets of features.
#That procedure is recursively repeated on the pruned set until the desired number of features to select is eventually reached.

from sklearn.linear_model import LogisticRegression
from sklearn.feature_selection import RFE

cols = ["Age","Fare","TravelAlone","Pclass_1","Pclass_2","Embarked_C","Embarked_S","Sex_male","IsMinor"] 
X = final_train[cols]
y = final_train['Survived']

# Build a logreg and compute the feature importances
model = LogisticRegression()

# create the RFE model and select 8 attributes, (8 is a arbitrary number)
rfe = RFE(model, 8)
rfe = rfe.fit(X, y)


# summarize the selection of the attributes
print('Selected features: %s' % list(X.columns[rfe.support_]))


# RFECV performs RFE in a cross-validation loop to find the optimal number or the best number of features. 
from sklearn.feature_selection import RFECV
# Create the RFE object and compute a cross-validated score.
# The "accuracy" scoring is proportional to the number of correct classifications
rfecv = RFECV(estimator=LogisticRegression(), step=1, cv=10, scoring='accuracy')
rfecv.fit(X, y)

print("Optimal number of features: %d" % rfecv.n_features_)
print('Selected features: %s' % list(X.columns[rfecv.support_]))



Selected_features = ['Age', 'TravelAlone', 'Pclass_1', 'Pclass_2', 'Embarked_C', 
                     'Embarked_S', 'Sex_male', 'IsMinor']
X = final_train[Selected_features]

# Model evaluation based on simple Train/Test split
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.metrics import accuracy_score, classification_report, precision_score, recall_score 
from sklearn.metrics import confusion_matrix, precision_recall_curve, roc_curve, auc, log_loss

# create X (features) and y (response)
X = final_train[Selected_features]
y = final_train['Survived']

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=2)

# check classification scores of logistic regression
logreg = LogisticRegression()
logreg.fit(X_train, y_train)
y_pred = logreg.predict(X_test)

print('Train/Test split results:')
print(logreg.__class__.__name__+" accuracy is %2.3f" % accuracy_score(y_test, y_pred))

#Model evaluation based on K-fold cross-validation using cross_val_score() function
# 10-fold cross-validation logistic regression
logreg = LogisticRegression()

# Use cross_val_score function
# We are passing the entirety of X and y, not X_train or y_train, it takes care of splitting the data
# cv=10 for 10 folds

scores_accuracy = cross_val_score(logreg, X, y, cv=10, scoring='accuracy')
print('K-fold cross-validation results:')
print(logreg.__class__.__name__+" average accuracy is %2.3f" % scores_accuracy.mean())

# Predicting the resuls for the Final_Test dataset
logreg.fit(X,y)
final_test['Survived'] = logreg.predict(final_test[Selected_features])

final_test.to_csv('Titanic_Predictions_Python', encoding='utf-8', index=False)
final_train.to_csv('Titanic_Training_Python', encoding='utf-8', index=False)