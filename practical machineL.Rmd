---
title: "Practical Machine Learning Project"
author: "Shadi"
date: "5/14/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Summary

This is Project aims to predict the manner in which the six participants performed the exercises described below and to answer the questions of the associated course quiz.


We start our analysis  loading the reaquired package and libraries and continue with
reading the train and test data 


```{r , echo=TRUE,message=FALSE}
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(caret)
library(KernSmooth)
library(Hmisc)
library(foreach)
library(doParallel)

```


```{r , echo=TRUE}
set.seed(4356)
setwd("~/Desktop/test/data")
train<-read.csv(file="pml-training.csv" , sep ="," )
validation<-read.csv(file="pml-testing.csv",sep =",")

```

Our data are cleaned and ready for the analysis, but before starting the analysis  we need a validation data set since are data set is large enough to have a validation an test data set. I’ll use cross validation within the training partition to improve the model fit and then do an out-of-sample test with the testing partition.


```{r , echo=TRUE}
set.seed(127)
training_sample <- createDataPartition(y=train$classe, p=0.7, list=FALSE)
training <- train[training_sample, ]
testing <- train[-training_sample, ]


```

Since we aim to do in sample and out sample prediction and check the errors the NA variable are of importance, so first we check the porportion of column with NA and then if we encounter NA then we remove those Columnes with NA 


```{r , echo=TRUE}
NAs <- colMeans(is.na(training))
table(NAs)
```

```{r , echo=TRUE}
all_zero_colnames <- sapply(names(validation), function(x) all(is.na(validation[,x])==TRUE))
nznames <- names(all_zero_colnames)[all_zero_colnames==FALSE]
nznames <- nznames[-(1:7)]
nznames <- nznames[1:(length(nznames)-1)]

```

Now that we removed missing values, We are  ready for  Model building, I start with predicting with the tree but it dosen't seems a good model for perdiction.


```{r , echo=TRUE}
fitControl <- trainControl(method='cv', number = 3)
modFit_tree <- train(classe ~ ., data=training[, c('classe', nznames)],trControl=fitControl, method='rpart')
save(modFit_tree, file='./modFit_tree.RData')

print(modFit_tree$finalModel)

plot(modFit_tree$finalModel, uniform=TRUE, 
      main="Classification Tree")
text(modFit_tree$finalModel, use.n=TRUE, all=TRUE, cex=.8)

```

Therefore, instead of moving to error test I will fit a random forest model. since they are usually giving the most acurate predictions.


```{r , echo=TRUE}

set.seed(10)
fitControl <- trainControl(method='cv', number = 3)

modFit_RF <-  train(
  classe ~ ., data=training[, c('classe', nznames)],trControl=fitControl, method='rf', ntree=100)
save(modFit_RF, file='./modFit_RF.RData')

```
now it's time to check the prediciton of the model and it accuracy on the validation set 

```{r , echo=TRUE}

pred <- predict(modFit_RF, newdata = testing)

confusionMatrix(pred, testing$classe)


```
The accuracy of the prediction is 99.24%. Hence, the out-of-sample error is 0.76%.


Finally, it is time to use our model for prediciton

```{r , echo=TRUE}

predValidation <- predict(modFit_RF, newdata=validation)
ValidationPredictionResults <- data.frame(
  problem_id=validation$problem_id,
  predicted=predValidation
)
print(ValidationPredictionResults)

```


