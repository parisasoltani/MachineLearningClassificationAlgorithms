#Classifications

#========Preprocessing==========
#Importing the dataset
dataset = read.csv('heart_disease.csv')

#Encoding the target feature as factor (Bayes)
dataset$target = factor(dataset$target, levels = c(0,1))


#Splitting thedataset into the Training and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$target , SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


#==============DEcision Tree classification============
#Fitting Decision Tree to the training set
library(rpart)

#Feature Scaling
#avoid acaling while you want to plot the Tree
training_set[, 1:9] = scale(training_set[, 1:9])
test_set[, 1:9] = scale(test_set[, 1:9])

classifier = rpart(formula = target ~ . ,
                   data = training_set)

#predicting the test set results
y_pred = predict(classifier, newdata = test_set[-14], type = 'class')

#Making the confusion Matrix
cm = table(test_set[, 14], y_pred)

#Accuracy
n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class 
accuracy = sum(diag) / n 
decisionTree_acc = accuracy

#plotting the Dicision tree (do not execute scale codes)
plot(classifier)
text(classifier)


#========KNN===========
#Feature Scaling
training_set[, 1:9] = scale(training_set[, 1:9])
test_set[, 1:9] = scale(test_set[, 1:9])


#Fitting KNN to the training set and predictiong the test results
library(class)
y_pred = knn(train = training_set[, -14],
             test = test_set[, -14],
             cl = training_set[, 14],
             k = 5)

#Making the confusion Matrix
cm = table(test_set[, 14], y_pred)

#Accuracy
n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class 
accuracy = sum(diag) / n 
KNN_acc = accuracy


#=========SVM===========
#Feature Scaling
training_set[, 1:9] = scale(training_set[, 1:9])
test_set[, 1:9] = scale(test_set[, 1:9])


#Fitting CLASSIFIER to the training set
library(e1071)
classifier = svm(formula = target ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

#predicting the test set results
y_pred = predict(classifier, newdata = test_set[-14])

#Making the confusion Matrix
cm = table(test_set[, 14], y_pred)

#Accuracy
n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class 
accuracy = sum(diag) / n 
SVM_acc = accuracy

#===========kernel SVM=============
# Feature Scaling
training_set[, -14] = scale(training_set[, -14])
test_set[, -14] = scale(test_set[, -14])

# Fitting Kernel SVM to the Training set
library(e1071)
classifier = svm(formula = target ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-14])

# Making the Confusion Matrix
cm = table(test_set[, 14], y_pred)

#Accuracy
n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class 
accuracy = sum(diag) / n 
kernelSVM_acc = accuracy

#========Naive Bayes classification===========
#Feature Scaling
training_set[, 1:9] = scale(training_set[, 1:9])
test_set[, 1:9] = scale(test_set[, 1:9])


#Fitting Naive Bayes to the training set
library(e1071)
classifier = naiveBayes(x = training_set[-14],
                        y = training_set$target)

#predicting the test set results
y_pred = predict(classifier, newdata = test_set[-14])

#Making the confusion Matrix
cm = table(test_set[, 14], y_pred)

#Accuracy
n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class 
accuracy = sum(diag) / n 
NaiveBayes_acc = accuracy

#========Random Forest classification===========
# Feature Scaling
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])

# Fitting Random Forest Classification to the Training set
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-14],
                          y = training_set$target,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-14])

# Making the Confusion Matrix
cm = table(test_set[, 14], y_pred)
#Accuracy
n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class 
accuracy = sum(diag) / n 
RandomForest_acc = accuracy

# Choosing the number of trees
plot(classifier)

#=========ACCURACY================
# Create the data for the chart
H <- c(NaiveBayes_acc*100,kernelSVM_acc*100,RandomForest_acc*100,SVM_acc*100,KNN_acc*100,decisionTree_acc*100)
M <- c("Bayes","kernelSVM","randomforest","SVM","KNN","decisionTree")
coll <- c("thistle1", "cornsilk1", "aliceblue", "azure3", "powderblue", "cornsilk2")

bp <-barplot(H,names.arg=M,ylab="ACCURACY (%)",
        col=coll,
        ylim = c(0,100),
        main="Classifications Evaluating",border="navyblue")
text(bp, 0, round(H, 1),cex=1,pos=3)