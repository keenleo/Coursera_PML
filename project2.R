library(randomForest)
library(randomForestSRC)
library(caret)


## 75% of the sample size
#smp_size <- floor(0.7 * nrow(pmlTrain))

## set the seed to make your partition reproductible
set.seed(666)
train.flag <- createDataPartition(pmlTrain$classe,p=0.75,list=FALSE)


#train_ind <- sample(seq_len(nrow(pmlTrain)), size = smp_size)
nzv <- nearZeroVar(pmlTrain)
filtered <- pmlTrain[, -nzv]
filtered$X <- NULL
#filtered.imputed <- rfImpute(classe ~ ., filtered)

head(filtered)

inc.cols <- c("user_name", "num_window", "roll_belt", "pitch_belt", "yaw_belt", "gyros_belt_x", "gyros_belt_y", "gyros_belt_z", "accel_belt_x", "accel_belt_y", "accel_belt_z", "magnet_belt_x", "magnet_belt_y", "magnet_belt_z", "roll_arm", "pitch_arm", "yaw_arm", "total_accel_arm", "gyros_arm_x", "gyros_arm_y", "gyros_arm_z", "accel_arm_x", "accel_arm_y", "accel_arm_z", "magnet_arm_x", "magnet_arm_y", "magnet_arm_z", "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell", "gyros_dumbbell_x", "gyros_dumbbell_y", "gyros_dumbbell_z", "accel_dumbbell_x", "accel_dumbbell_y", "accel_dumbbell_z", "magnet_dumbbell_x", "magnet_dumbbell_y", "magnet_dumbbell_z", "roll_forearm", "pitch_forearm", "yaw_forearm", "total_accel_forearm", "gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_z", "accel_forearm_x", "accel_forearm_y", "accel_forearm_z", "magnet_forearm_x", "magnet_forearm_y", "magnet_forearm_z", "classe")
filtered <- filtered[, inc.cols]

filtered.train <- filtered[train_ind, ]
filtered.test <- filtered[-train_ind, ]

model <- train(filtered.train$classe ~.,data=filtered.train, method="rf")

predTest <- predict(model,filtered.test)

table(predTest,as.factor(filtered.test$classe))

confusionMatrix(predTest,as.factor(filtered.test$classe))

#classe<-filtered$classe
#nums <- sapply(filtered, is.numeric)
#filtered2<-cbind(filtered$classe,training[,nums])

#train2 <- filtered[train_ind, ]
#train1 <- filtered[-train_ind, ]
#nzv <- nearZeroVar(pmlTrain, saveMetrics = TRUE)
#nzv <- nearZeroVar(pmlTrain)
#modfit <- train(Species~ .,method="rf",data=train2)