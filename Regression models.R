
####################################################################################
### Regression Models                                                            ###
####################################################################################

demo_base_reg<-demo[,c(-15,-16,-17)]
str(demo_base_reg)
demo_base_reg = demo_base_reg[!demo_base_reg$QTY_SOLD_CY<0,]

training1_base <- demo_base_reg[trainIndex_base,]
testing1_base <- demo_base_reg[-trainIndex_base,]

##### MLR
m1f <- glm(QTY_SOLD_CY ~ ., data=training1_base)
summary(m1f)

# forward selection
library(leaps)
mlf <- regsubsets(QTY_SOLD_CY ~ ., data=training1_base, nbest=1, intercept=T, method='forward') #plot(mlf)
vars2keep <- data.frame(summary(mlf)$which[which.max(summary(mlf)$adjr2),])
names(vars2keep) <- c("keep")  
head(vars2keep)
library(data.table)
vars2keep <- setDT(vars2keep, keep.rownames=T)[]
vars2keep <- c(vars2keep[which(vars2keep$keep==T & vars2keep$rn!="(Intercept)"),"rn"])[[1]]
vars2keep

##[1] "QTY_SOLD_PPY"       "QTY_SOLD_PY"        "LOST_QTY_PY"        "AVG_TOTAL_PY"       "SOLD_SINCE_MAXI"   
##[6] "TOTAL_VIO_PY"       "APP_COUNT"          "UNADJ_TOTAL_VIO_PY"

modelFormula <- paste("QTY_SOLD_CY ~ QTY_SOLD_PPY + QTY_SOLD_PY +LOST_QTY_PY + AVG_TOTAL_PY + SOLD_SINCE_MAXI + UNADJ_TOTAL_VIO_PY + APP_COUNT + TOTAL_VIO_PY")
m1f <- lm(modelFormula, data=training1_base)
summary(m1f)

# backward selection
mlb <- regsubsets(QTY_SOLD_CY ~ ., data=training1_base, nbest=1, intercept=T, method='backward') #plot(mlf)
vars2keep <- data.frame(summary(mlb)$which[which.max(summary(mlb)$adjr2),])
names(vars2keep) <- c("keep")
head(vars2keep)
vars2keep <- setDT(vars2keep, keep.rownames=T)[]
vars2keep <- c(vars2keep[which(vars2keep$keep==T & vars2keep$rn!="(Intercept)"),"rn"])[[1]]
vars2keep

### forward and backward selection output the exact same set of significant features

require(ggplot2)
require(pscl)
require(boot)

### Poisson regression model
pois <- glm(QTY_SOLD_CY ~ QTY_SOLD_PPY + QTY_SOLD_PY + AVG_TOTAL_PY + SKU_EXISTENCE_PY + SOLD_SINCE_MAXI, family = poisson, data=training1_base)
summary(pois)

### Zero-inflated Poisson regression
zero_pois <- zeroinfl(QTY_SOLD_CY ~ QTY_SOLD_PPY + QTY_SOLD_PY + AVG_TOTAL_PY + SKU_EXISTENCE_PY , data=training1_base)
summary(zero_pois)


##### Neural Networks
sum(is.na(training1_base))

ctrl <- trainControl(method="cv" , number=3)
(maxvalue <- summary(training1_base$QTY_SOLD_CY)["Max."][[1]])
nnet1 <- caret::train(QTY_SOLD_CY/104 ~ QTY_SOLD_PPY + QTY_SOLD_PY + AVG_TOTAL_PY + SKU_EXISTENCE_PY + SOLD_SINCE_MAXI,
               data = training1_base,     
               method = "nnet",   
               trControl = ctrl,  
               tuneLength = 15,
               maxit = 100,
               metric = "RMSE",
               na.action = na.pass
)

nnet1$finalModel$tuneValue
##size decay
##62    9 1e-04

##### Decision Tree
library(tree)
tree1 = tree(QTY_SOLD_PPY ~ .
             , control = tree.control(nobs=nrow(training1_base)[[1]]
                                      , mincut = 0
                                      , minsize = 1
                                      , mindev = 0.01)
             , data = training1_base)
summary(tree1)
plot(tree1); text(tree1, pretty=0) # plot the tree

# perform cross-validation to find optimal number of terminal nodes
cv.tree1 = cv.tree(tree1)
par(mfrow=c(1,1))
plot(cv.tree1$size
     , cv.tree1$dev
     , type = 'b')

# prune tree where the number of terminal nodes is 4
prunedfit = prune.tree(tree1, best=4)
summary(prunedfit)
plot(prunedfit); text(prunedfit, pretty=0)

### baged tree
tree1b <- caret::train(QTY_SOLD_PPY ~ .,
                data = training1_base, 
                method = "treebag",    
                trControl = ctrl,  
                metric = "RMSE")

yhat_m1f <- predict(m1f, newdata=training1_base)
yhat_m1b <- predict(m1b, newdata=training1_base)
yhat_nn1 <- predict(nnet1, newdata=training1_base)*maxvalue
yhat_dt1 <- predict(tree1, newdata=training1_base)
yhat_dt1b <- predict(tree1b, newdata=training1_base)

yhat_m1f_te <- predict(m1f, newdata=testing1_base)
yhat_m1b_te <- predict(m1b, newdata=testing1_base)
yhat_nn1_te <- predict(nnet1, newdata=testing1_base)*maxvalue
yhat_dt1_te <- predict(tree1, newdata=testing1_base)
yhat_dt1b_te <- predict(tree1b, newdata=testing1_base)

results <- matrix(rbind(
  cbind(t(postResample(pred=yhat_m1f, obs=training1_base$QTY_SOLD_PPY)), t(postResample(pred=yhat_m1f_te, obs=testing1_base$QTY_SOLD_PPY))),
  cbind(t(postResample(pred=yhat_nn1, obs=training1_base$QTY_SOLD_PPY)), t(postResample(pred=yhat_nn1_te, obs=testing1_base$QTY_SOLD_PPY))),
  cbind(t(postResample(pred=yhat_dt1, obs=training1_base$QTY_SOLD_PPY)), t(postResample(pred=yhat_dt1_te, obs=testing1_base$QTY_SOLD_PPY))),
  cbind(t(postResample(pred=yhat_dt1b, obs=training1_base$QTY_SOLD_PPY)), t(postResample(pred=yhat_dt1b_te, obs=testing1_base$QTY_SOLD_PPY)))
),nrow=4)

colnames(results) <- c("Train_RMSE", "Train_R2","Train_MAE","Test_RMSE", "Test_R2","Test_MAE")
rownames(results) <- c("MLR_Forward","NN_ForBackFeatures","Tree_Numerics+Factors","BaggedTree_Numerics+Factors")

results[,c(-3,-6)]
