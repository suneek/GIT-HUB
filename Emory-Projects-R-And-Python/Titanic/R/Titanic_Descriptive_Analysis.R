
dataset = read.csv('Titanic_train.csv')

require(graphics)

# Scatter Plots to find linear relationships between 
# the dependent (response) variable and independent (predictor) variables
scatter.smooth(x=dataset$Pclass, y=dataset$Survived, main="Survived ~ Pclass") 

dataset$Age[is.na(dataset$Age)] = mean(dataset$Age, na.rm = TRUE)
dataset$Sex = ifelse(dataset$Sex == 'male', 0, 1)
scatter.smooth(x=dataset$Sex, y=dataset$Survived, main="Survived ~ Sex") 


# Box plots to check for outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(dataset$Sex, main="Sex", sub=paste("Outlier rows: ", boxplot.stats(cars$Sex)$out))  # box plot for 'speed'
boxplot(dataset$Survived, main="Survived", sub=paste("Outlier rows: ", boxplot.stats(dataset$Survived)$out))  
boxplot(dataset$Pclass, main="Pclass", sub=paste("Outlier rows: ", boxplot.stats(dataset$Pclass)$out))  
boxplot(dataset$Age, main="Age", sub=paste("Outlier rows: ", boxplot.stats(dataset$Age)$out))  



which.nonnum <- function(x) {
  badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
  which(badNum & !is.na(x))
}

dataset$Sibsp.which.nonnum


# Correlations
cor(dataset$Age, dataset$Survived)
cor(dataset$Sex, dataset$Survived)
cor(dataset$Pclass, dataset$Survived)
cor(dataset$Sibsp, dataset$Survived)
cor(dataset$Parch, dataset$Survived)

