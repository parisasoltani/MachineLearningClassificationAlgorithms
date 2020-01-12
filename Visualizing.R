#visualizing


dataset = read.csv('heart_disease.csv')

#Fitting Dicision Tree Regressioin  Model to the dataset
library(rpart)
regressor = rpart(formula = target ~ . ,
                  data = dataset)

dataset = dataset [, c(1,5,14)]

# Encoding the target feature as factor
dataset$target = factor(dataset$target, levels = c(0, 1))

#Splitting thedataset into the Training and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$target , SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature Scaling
training_set[, 1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale(test_set[, 1:2])

#KNN====
#Fitting KNN to the training set and predictiong the test results
library(class)
y_pred = knn(train = training_set[, -3],
             test = test_set[, -3],
             cl = training_set[, 3],
             k = 5)


#Visualising the training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1 , X2)
colnames(grid_set) = c('Age', 'thalach')
y_grid = knn(train = training_set[, -3],
             test = grid_set,
             cl = training_set[, 3],
             k = 5)
plot(set[, -3],
     main = 'KNN (Training set)',
     xlab = 'Age', ylab = 'thalach',
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid ==1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3]==1, 'green4', 'red3'))

#Visualising the test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1 , X2)
colnames(grid_set) = c('Age', 'thalach')
y_grid = knn(train = training_set[, -3],
             test = grid_set,
             cl = training_set[, 3],
             k = 5)
plot(set[, -3],
     main = 'KNN (Test set)',
     xlab = 'Age', ylab = 'thalach',
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid ==1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3]==1, 'green4', 'red3'))

# Support Vector Machine (SVM)==============

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = target ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('age', 'thalach')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'age', ylab = 'thalach',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('age', 'thalach')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'age', ylab = 'thalach',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

#kernel SVM==========
# Fitting Kernel SVM to the Training set
library(e1071)
classifier = svm(formula = target ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('age', 'thalach')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Kernel SVM (Training set)',
     xlab = 'age', ylab = 'thalach',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('age', 'thalach')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'Kernel SVM (Test set)',
     xlab = 'age', ylab = 'thalach',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

#NAIVE BAYES========
#Fitting Naive Bayes to the training set
library(e1071)
classifier = naiveBayes(x = training_set[-3],
                        y = training_set$target)

#predicting the test set results
y_pred = predict(classifier, newdata = test_set[-3])

#Visualising the training set results89
#install.packages('ElemStatLearn')
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1 , X2)
colnames(grid_set) = c('age', 'thalach')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Naive Bayes (Training set)',
     xlab = 'age', ylab = 'thalach',
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid ==1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3]==1, 'green4', 'red3'))

#Visualising the test set results89
#install.packages('ElemStatLearn')
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1 , X2)
colnames(grid_set) = c('age', 'thalach')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Naive Bayes (Test set)',
     xlab = 'age', ylab = 'thalach',
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid ==1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3]==1, 'green4', 'red3'))
#Decision Tree=====
#Fitting Decision Tree to the training set
library(rpart)
classifier = rpart(formula = target ~ . ,
                   data = training_set)

#predicting the test set results
y_pred = predict(classifier, newdata = test_set[-3], type = 'class')

#Visualising the training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1 , X2)
colnames(grid_set) = c('age', 'thalach')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Decision Tree (Training set)',
     xlab = 'age', ylab = 'thalach',
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid ==1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3]==1, 'green4', 'red3'))

#Visualising the test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1 , X2)
colnames(grid_set) = c('age', 'thalach')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Decision Tree (Test set)',
     xlab = 'age', ylab = 'thalach',
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid ==1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3]==1, 'green4', 'red3'))

#Random Forest=======
# Fitting Random Forest Classification to the Training set
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-3],
                          y = training_set$target,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('age', 'thalach')
y_grid = predict(classifier, grid_set)
plot(set[, -3],
     main = 'Random Forest Classification (Training set)',
     xlab = 'age', ylab = 'thalach',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('age', 'thalach')
y_grid = predict(classifier, grid_set)
plot(set[, -3], main = 'Random Forest Classification (Test set)',
     xlab = 'age', ylab = 'thalach',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
