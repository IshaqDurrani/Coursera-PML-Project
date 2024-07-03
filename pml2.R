library(lattice)
library(ggplot2)
library(caret)
library(kernlab)
library(rattle)
library(corrplot)
set.seed(1234)

traincsv <- read.csv("pml-training.csv")
 testcsv <- read.csv("pml-testing.csv")
 
    dim(traincsv)

 dim(testcsv)

 View(testcsv)
 View(traincsv)
 traincsv <- traincsv[,colMeans(is.na(traincsv)) < .9] #removing mostly na columns
 traincsv <- traincsv[,-c(1:7)] #removing metadata which is irrelevant to the outcome
 nvz <- nearZeroVar(traincsv)
 traincsv <- traincsv[,-nvz]
 dim(traincsv)

 inTrain <- createDataPartition(y=traincsv$classe, p=0.7, list=F)
 train <- traincsv[inTrain,]
 valid <- traincsv[-inTrain,]
 control <- trainControl(method="cv", number=3, verboseIter=F)
 mod_trees <- train(classe~., data=train, method="rpart", trControl = control, tuneLength = 5)
 fancyRpartPlot(mod_trees$finalModel)
 pred_trees <- predict(mod_trees, valid)
 cmtrees <- confusionMatrix(pred_trees, factor(valid$classe))
 cmtrees
     


 mod_rf <- train(classe~., data=train, method="rf", trControl = control, tuneLength = 5)
 
   pred_rf <- predict(mod_rf, valid)
 cmrf <- confusionMatrix(pred_rf, factor(valid$classe))
 cmrf

 mod_svm <- train(classe~., data=train, method="svmLinear", trControl = control, tuneLength = 5, verbose = F)
 
   pred_svm <- predict(mod_svm, valid)
 cmsvm <- confusionMatrix(pred_svm, factor(valid$classe))
 cmsvm

 models <- c("Tree", "RF", "GBM", "SVM")
 accuracy <- round(c( cmtrees$overall[1], cmrf$overall[1], cmgbm$overall[1], cmsvm$overall[1]),3) #accuracy
 oos_error <- 1 - accuracy #out of sample error
 
 data.frame(accuracy = accuracy, oos_error = oos_error, row.names = models)
 pred <- predict(mod_rf, testcsv)
 print(pred)
 corrPlot <- cor(train[, -length(names(train))])
 corrplot(corrPlot, method="color")
 plot(mod_trees)
 plot(mod_rf)
 plot(mod_gbm)