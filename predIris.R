
train.flag <- createDataPartition(y=pmlTrain$classe,p=0.05,list=FALSE)

training <- pmlTrain[train.flag,]

Validation <- pmlTrain[-train.flag,] 

library(randomForest)

library(randomForestSRC)

library(caret)

modfit <- train(classe~ .,method="rf",data=training)

pred <- predict(modfit,training)

table(pred,training$classe)