library(caret)

source("src/dataprep.R")
source("src/utils.R")
train <- getFeatures(path = "input/train.csv", onehot = T)
holdOut <- getFeatures(path = "input/test.csv",onehot = F,requestedYears = 2008)
#dataset$Label <- as.factor(dataset$Label)
inTrain <- createDataPartition(y = train$WnvPresent,p = .5,list = F)
#label <- dataset[inTrain,"Label"]
training <- train[inTrain,] 
testing <- train[-inTrain,]

cvCtrl <- trainControl(method="cv", 
                       number = 5,
                       repeats = 1,
                       summaryFunction = twoClassSummary,classProb = TRUE)

dtFit <- train(form = WnvPresent ~.,data = training,
               method = "rpart",
               trControl =cvCtrl, metric = "ROC")

dtClass <- predict(dtFit, newdata = testing) # classification outcome required for the confusion matrix
dtProb <- predict(dtFit, newdata = testing, type = "prob") # predicted data required for ROC curve
source("src/plots.R")
dtps <- getPredictionStates(classPrediction = dtClass,probPrediction = dtProb,Label = testing$WnvPresent,fittedModel = dtFit)


rfFit <- train(form = WnvPresent ~.,data = training,
               method = "rf",
               trControl =cvCtrl, metric = "ROC")

rfClass <- predict(rfFit, newdata = testing) # classification outcome required for the confusion matrix
rfProb <- predict(rfFit, newdata = testing, type = "prob") # predicted data required for ROC curve


xgps <- getPredictionStates(classPrediction = rfClass,probPrediction = rfProb,Label = testing$WnvPresent,fittedModel = rfFit)

xgboostFit <- train(form = WnvPresent ~.,data = training,
                    method = 'xgbTree',
                    preProcess = c("center", "scale"),
                    trControl = trainControl(method = "cv"))

xgClass <- predict(xgboostFit, newdata = testing) # classification outcome required for the confusion matrix
xgProb <- predict(xgboostFit, newdata = testing, type = "prob") # predicted data required for ROC curve
xgps <- getPredictionStates(classPrediction = xgClass,probPrediction = xgProb,Label = testing$WnvPresent,fittedModel = xgboostFit)


lrFit <- train(form = WnvPresent ~.,data = training,
               method="glm", family="binomial",
               trControl = trainControl(method = "cv"))

lrClass <- predict(lrFit, newdata = testing) # classification outcome required for the confusion matrix
lrProb <- predict(lrFit, newdata = testing, type = "prob") # predicted data required for ROC curve
lrps <- getPredictionStates(classPrediction = lrClass,probPrediction = lrProb,Label = testing$WnvPresent,fittedModel = lrFit,withTuning = F)


glmnetFit <- train(form = WnvPresent ~.,data = training,
               method="glmnet", family="binomial",
               trControl = trainControl(method = "cv"))

glmnetClass <- predict(glmnetFit, newdata = testing) # classification outcome required for the confusion matrix
glmnetProb <- predict(glmnetFit, newdata = testing, type = "prob") # predicted data required for ROC curve
glmnetps <- getPredictionStates(classPrediction = glmnetClass,probPrediction = glmnetProb,Label = testing$WnvPresent,fittedModel = glmnetFit,withTuning = F)



plotROC(probPrediction = xgProb,Label = testing$WnvPresent,fittedModel = xgboostFit)
plotROC(probPrediction = lrProb,Label = testing$WnvPresent,fittedModel = lrFit,addToPrev= F)
plotROC(probPrediction = rfProb,Label = testing$WnvPresent,fittedModel = rfFit)
plotROC(probPrediction = dtProb,Label = testing$WnvPresent,fittedModel = dtFit)
plotROC(probPrediction = glmnetProb,Label = testing$WnvPresent,fittedModel = glmnetFit)
library(ROCR)

rfAUC <- getAUC(rfProb,testing$WnvPresent)
xgAUC <- getAUC(xgProb,testing$WnvPresent)
lrAUC <- getAUC(lrProb,testing$WnvPresent)
dtAUC <- getAUC(dtProb,testing$WnvPresent)
glmnetAUC <- getAUC(glmnetProb,testing$WnvPresent)
cat("RF: ",rfAUC,"XG:",xgAUC,"LR:",lrAUC,"DT:",dtAUC, "glmNET:",glmnetAUC)


