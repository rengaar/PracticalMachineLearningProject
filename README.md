# PracticalMachineLearningProject
---
## To run the code, please use project.R file.
---
### Loading packages and Getting data
```{r, eval=F}
getwd()
library(caret)
library(randomForest)
library(rattle)
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")
````
### Cleaning data
#### Dropping NA variables
```{r, eval=F}
na_count <- sapply(training, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
training <- training[,na_count == 0]
testing <- testing[, na_count == 0]
```
#### Dropping variable that are near zero variance
Dropping time of the record too (var 5).
```{r, eval=F}
nzv <- nearZeroVar(training, saveMetrics = TRUE)
training <- training[, nzv$nzv == FALSE]
testing <- testing[, nzv$nzv == FALSE]
training <- training[, -5]
testing <- testing[, -5]
```
58 variables incl. classe remained.

#### Creating Five Folds
```{r, eval=F}
folds <- createFolds(y=training$classe, k=5, list=TRUE, returnTrain=FALSE)
sum(sapply(folds, length))
```
### Random tree
```{r, eval=F}
misclass <- rep(0,5)
for (i in 1:5){
trains <- training[-folds[[i]],]
modFit <- train(classe ~., method = "rpart", data = trains)
pred <- predict(modFit, newdata = training[folds[[i]],])
M <- table(pred, training[folds[[i]],]$classe)
misclass[i] <- 1-(sum(diag(M))/sum(M))
}
```
misclassification for each fold:
```{r, cache = T, eval=FALSE}
# displaying plot for the 5th fold predicted
fancyRpartPlot(modFit$finalModel)
```
![alt text](http://s23.postimg.org/6yq0djijf/tree.png)
```{r, cache = T, eval=FALSE}
misclass
```
##### Misclassification
######Fold 1: 0.3384263;
######Fold 2: 0.3381753;
######Fold 3: 0.3385164;
######Fold 4: 0.3384302;
######Fold 5: 0.3386850;

The algorithm is not able to classify C, D classes at all. The rest is classified properly. The algorithm seems to have about 34 % out of sample error.


### Random forest
Employing random forest to increase precision of prediction.
```{r, eval=F}
misclassRF <- rep(0,5)
for (i in 1:5){
trains <- training[-folds[[i]],]
fitRF <- randomForest(classe~., data = trains)
predRF <- predict(fitRF, newdata = training[folds[[i]],])
M <- table(predRF, training[folds[[i]],]$classe)
misclassRF[i] <- 1-(sum(diag(M))/sum(M))
}
```
#### Prediction for the last fold:
```{r, eval=F}
confusionMatrix(predRF, training[folds[[5]],]$classe)
```
Overall Statistics
                                     
               Accuracy : 1          
                 95% CI : (0.9991, 1)
    No Information Rate : 0.2845     
    P-Value [Acc > NIR] : < 2.2e-16 

The algorithm classified all observations correctly, which is weird... Indicating no out of sample error.

### Predicting on the testing set
Using the 5th fold as an evaluation set. Therefore predicting based on folds 1-4.
```{r, eval=F}
testPred <- predict(fitRF, testing)
```
