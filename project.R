setwd("C:/Users/Jindra/Dropbox/R/Machine Learning")
library(caret)
library(randomForest)
library(rattle)
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")
names(training)
table(training$classe)
table(is.na(training))
na_count <-sapply(training, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
training <- training[,na_count == 0]
testing <- testing[, na_count == 0]


nzv <- nearZeroVar(training, saveMetrics = TRUE)
training <- training[, nzv$nzv == FALSE]
testing <- testing[, nzv$nzv == FALSE]
training <- training[, -5]
testing <- testing[, -5]

folds <- createFolds(y=training$classe, k=5, list=TRUE, returnTrain=FALSE)
sum(sapply(folds, length))
misclass <- rep(0,5)
par(mfrow=c(3,2))
for (i in 1:5){
trains <- training[-folds[[i]],]
modFit <- train(classe ~., method = "rpart", data = trains)
pred <- predict(modFit, newdata = training[folds[[i]],])
M <- table(pred, training[folds[[i]],]$classe)
misclass[i] <- 1-(sum(diag(M))/sum(M))
fancyRpartPlot(modFit$finalModel)
}
par(mfrow=c(1,1))
fancyRpartPlot(modFit$finalModel)
predict(modFit, testing)

misclassRF <- rep(0,5)
for (i in 1:5){
trains <- training[-folds[[i]],]
fitRF <- randomForest(classe~., data = trains)
predRF <- predict(fitRF, newdata = training[folds[[i]],])
M <- table(predRF, training[folds[[i]],]$classe)
misclassRF[i] <- 1-(sum(diag(M))/sum(M))
}
misclassRF
confusionMatrix(predRF, training[folds[[5]],]$classe)

levels(testing$user_name) <- levels(training$user_name)


fitForest <- randomForest(classe~., data = training[-folds[[5]],])
prediction <- predict(fitRF, newdata = training[folds[[5]],], type = "class")
confusionMatrix(prediction, training[folds[[5]],]$classe)
testPred <- predict(fitRF, testing)
testPred
