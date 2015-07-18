---
title: "Practical Machine Learning Course Project"
author: "Yuhua Zhang"
date: "Friday, July 17, 2015"
output: html_document
---

## Introduction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the project will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 

The goal of the project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. I may use any of the other variables to predict with, create a report describing how I built my model, how I used cross validation, what I think the expected out of sample error is, and why I made the choices I did.I will also use my prediction model to predict 20 different test cases. 

## Data

The training data for this project are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment. 

## Load and clean the data

```{r,echo=TRUE}
trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
trainda <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testda <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))
# Remove columns with more than 95% of "NA" or ""
library(caret)
training <- trainda[, 6:dim(trainda)[2]]
treshold <- dim(training)[1] * 0.95
goodCol <- !apply(training, 2, function(x) sum(is.na(x)) > treshold  || sum(x=="") > treshold)
training <- training[, goodCol]
badCol <- nearZeroVar(training, saveMetrics = TRUE)
training <- training[, badCol$nzv==FALSE]
training$classe = factor(training$classe)
```

## Partition training dataset

Part the training set into two data sets, 60% mytraining and 40% mytesting respectively.

```{r,echo=TRUE}
intrain <- createDataPartition(y=trainda$classe, p=0.6, list=FALSE)
mytrain <- training[intrain, ]
mytest <- training[-intrain, ]
dim(mytrain)
dim(mytest)
```

## Prediction

To make prediction,I will use decision tree and random forest algorithms. The algorithm with the highest accuracy will be chosen as final model. Cross-validation will be performed by subsampling our training data set randomly without replacement into 2 subsamples: mytraining data and myTesting data.Models will be fitted on mytraing dataset, and tested on mytesting dataset. The most accurate model fitted will be tested on the original Testing data set.

### Decision trees for prediction

```{r,echo=TRUE}
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
model1 <- rpart(classe ~ ., data=mytrain, method="class")
fancyRpartPlot(model1)
# Predict
pred1 <- predict(model1, mytest, type = "class")
# Use confusion matrix to test results
confusionMatrix(pred1, mytest$classe)
```

### Random forest for prediction

```{r,echo=TRUE}
library(randomForest)
model2 <- randomForest(classe ~. , data=mytrain)
# Predict
pred2 <- predict(model2, mytest, type = "class")
# Use confusion matrix to test results
confusionMatrix(pred2, mytest$classe)
```

## Conclusion

As is shown above, random forest yields better prediction than decision trees. Accuracy for random forest model was 0.9968 (95% CI : (0.9953, 0.9979)), which is better than decision tree model with accurancy only 0.7348 (95% CI : (0.7249, 0.7445). 

Thue we choose model2. The expected out-of-sample error is estimated at 0.32%. The expected out-of-sample error is calculated as 1 - accuracy for predictions made against the cross-validation set, and the test data set comprises 20 cases.

## Submission

```{r,echo=TRUE}
# Function to submit assignment
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
# Use random forest on testing dataset
answers <- predict(model2, testda, type = "class")
answers

pml_write_files(answers)
```
