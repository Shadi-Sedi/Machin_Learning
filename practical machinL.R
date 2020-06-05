#The goal of your project is to predict the manner 
#in which they did the exercise. This is the "classe" 
#variable in the training set. You may use any of the other variables 
#to predict with. You should create a report describing how you built your model,
#how you used cross validation, what you think the expected out of sample error is,
#and why you made the choices you did. You will also use your prediction model to
#predict 20 different test cases.
library(caret)

setwd("~/Desktop/test/data")
training<-read.csv(file="pml-training.csv" , sep ="," )
testing<-read.csv(file="pml-testing.csv",sep =",")

testing$user_name<-sort(testing$user_name)
training$user_name<-sort(training$user_name)


## Clean data
#how many missing data in each col
NAs <- colMeans(is.na(training))
table(NAs)

#Then we remove columns that contain NA missing values.
training <- training[, colSums(is.na(training)) == 0] 
testing <- testing[, colSums(is.na(testing)) == 0] 
 
## corss validation
inTrain <- createDataPartition(training$classe, p=0.70, list=F)
train <- training[inTrain, ]
validation <- training[-inTrain, ]

## tree
trControl <- trainControl(method="cv", number=5)



modFit_tree<- train(classe ~ ., method="rpart", data=train,trControl=trControl )
print(modFit$finalModel)

plot(modFit_tree$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(modFit_tree$finalModel, use.n=TRUE, all=TRUE, cex=.8)


##random forest
modFit_RF <- train(classe~., method="rf", data=train, trControl=trControl, verbose=FALSE)


#  prediction

pred <- predict(modFit_RF, newdata = validation)

confusionMatrix(pred, validation$classe)


pred <- predict(modFit_RF, newdata = testing)

confusionMatrix(pred, testing$classe)


#final prediction

pred <- predict(modFit_RF, newdata = testing)


