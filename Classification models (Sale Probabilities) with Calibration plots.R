
####################################################################################
### Classification Models                                                        ###
####################################################################################

##############################################
###### Logistic Regression              ######
##############################################
library(e1071)
require(caret)
TC <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary, verboseIter = TRUE)
logit <- caret::train(Sold~.,
               data = training_base,
               method = "glm",
               family = "binomial",
               metric = "ROC", 
               trControl = TC,
               na.action=na.exclude)

### Find Confusion Matrix for training data
logitProbsTrain <- predict(logit, newdata=training_base, type="prob")[,1]
logitClassesTrain <- predict(logit, newdata=training_base)
trcm_logit <- confusionMatrix(logitClassesTrain, training_base$Sold)

### Find Confusion Matrix for testing data
logitProbsTest <- predict(logit, newdata=testing_base, type="prob")[,1]
logitClassesTest <- predict(logit, newdata=testing_base)
tecm_logit <- confusionMatrix(logitClassesTest, testing_base$Sold)

### calibration plot
calCurveLogit = calibration(testing_base$Sold ~ logitProbsTest, cuts=20)
xyplot(calCurveLogit)

##############################################
###### Boosting model                   ######
##############################################

TC <- trainControl(method="cv",number=5)
logitBoosted <- caret::train(Sold~.
                      , data = training_base
                      , method = "gbm"
                      , verbose = F
                      , trControl = TC)

### Find Confusion Matrix for training data
logitBoostedProbsTrain <- predict(logitBoosted, newdata=training_base, type="prob")[,1]
logitBoostedClassesTrain <- predict(logitBoosted, newdata=training_base)
trcm_boost <- confusionMatrix(logitBoostedClassesTrain, training_base$Sold)

### Find Confusion Matrix for testing data
logitBoostedProbsTest <- predict(logitBoosted, newdata=testing_base, type="prob")[,1]
logitBoostedClassesTest <- predict(logitBoosted, newdata=testing_base)
tecm_boost <- confusionMatrix(logitBoostedClassesTest, testing_base$Sold)
tecm_boost
### calibration plot
calCurvelogitBoosted = calibration(testing_base$Sold ~ logitBoostedProbsTest, cuts=20)
xyplot(calCurvelogitBoosted)

##############################################
###### Bagging model                    ######
##############################################

TC <- trainControl(method="cv",number=5)
Bagged <- caret::train(Sold~.
                , data = training_base
                , method = "treebag"
                , trControl=TC)

### Find Confusion Matrix for training data
BaggedProbsTrain <- predict(Bagged, newdata=training_base, type="prob")[,1]
BaggedClassesTrain <- predict(Bagged, newdata=training_base)
trcm_bag <- confusionMatrix(BaggedClassesTrain, training_base$Sold)

### Find Confusion Matrix for testing data
BaggedProbsTest <- predict(Bagged, newdata=testing_base, type="prob")[,1]
BaggedClassesTest <- predict(Bagged, newdata=testing_base)
tecm_bag <-confusionMatrix(BaggedClassesTest, testing_base$Sold)

### calibration plot
calCurvelogitBagged = calibration(testing_base$Sold ~ BaggedProbsTest, cuts=20)
xyplot(calCurvelogitBagged)

##############################################
###### Cart model                       ######
##############################################

ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary,verboseIter = TRUE)
cartmod1 <- caret::train(Sold~.,
                  data = training_base,
                  method = "rpart",
                  preProc = c("center","scale"),
                  metric="ROC",
                  trControl=ctrl)

### Find Confusion Matrix for training data
cartProbsTrain <- predict(cartmod1, newdata=training_base, type="prob")[,1]
cartClassesTrain <- predict(cartmod1, newdata=training_base)
trcm_cart <- confusionMatrix(cartClassesTrain, training_base$Sold)

### Find Confusion Matrix for testing data
CartProbsTest <- predict(cartmod1, newdata=testing_base, type="prob")[,1]
CartClassesTest <- predict(cartmod1, newdata=testing_base)
tecm_cart <- confusionMatrix(CartClassesTest, testing_base$Sold)

### calibration plot
calCurveCart = calibration(testing$Sold ~ CartProbsTest, cuts=20)
xyplot(calCurveCart)

##############################################
###### LDA                              ######
##############################################
ldamodA <- caret::train(Sold~.,
                 data = training_base
                 , method = "lda"
                 , family = "binomial"
                 , trControl = ctrl
                 , metric = "ROC")

### Find Confusion Matrix for training data
ldaProbsTrain <- predict(ldamodA, newdata=training_base, type="prob")[,1]
ldaClassesTrain <- predict(ldamodA, newdata=training_base)
trcm_lda <- confusionMatrix(ldaClassesTrain, training_base$Sold)

### Find Confusion Matrix for balanced testing data
ldaProbsTest <- predict(ldamodA, newdata=testing, type="prob")[,1]
ldaClassesTest <- predict(ldamodA, newdata=testing)
tecm_lda <- confusionMatrix(data=ldaClassesTest, testing$Sold)

### calibration plot
calCurveLDA = calibration(testing$Sold ~ ldaProbsTest, cuts=20)
xyplot(calCurveLDA)


##############################################
###### MLP                              ######
##############################################

ctrl <- trainControl(summaryFunction=twoClassSummary,	classProbs=TRUE)
mlpmod <- caret::train(Sold~.
                , data = training_base
                , method = "mlp"
                , preProc = c("center","scale")
                , metric="ROC"
                , trControl = ctrl)

### Find Confusion Matrix for training data
mlpProbsTrain <- predict(mlpmod, newdata=training_base, type="prob")[,1]
mlpClassesTrain <- predict(mlpmod, newdata=training_base)
confusionMatrix(mlpClassesTrain, training_base$Sold)

### Find Confusion Matrix for testing data
mlpProbsTest <- predict(mlpmod, newdata=testing_base, type="prob")[,1]
mlpClassesTest <- predict(mlpmod, newdata=testing_base)
tecm_mlp <- confusionMatrix(mlpClassesTest, testing_base$Sold)

### calibration plot
calCurveMLP = calibration(testing_base$Sold ~ mlpProbsTest, cuts=20)
xyplot(calCurveMLP)

### Model Scoring and Evaluation
testStats <- rbind(
  t(rbind(data.frame(logit=tecm_logit$overall), data.frame(logit=tecm_logit$byClass))),
  t(rbind(data.frame(boost=tecm_boost$overall), data.frame(boost=tecm_boost$byClass))),
  t(rbind(data.frame(bag=tecm_bag$overall), data.frame(bag=tecm_bag$byClass))),
  t(rbind(data.frame(cart=tecm_cart$overall), data.frame(cart=tecm_cart$byClass))),
  t(rbind(data.frame(mlp=tecm_mlp$overall), data.frame(mlp=tecm_mlp$byClass)))
)
testStats
