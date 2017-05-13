library(caret)

source("src/dataprep.R")
train <- getFeatures(path = "input/train.csv",requestedYear = 2007, onehot = F)
holdOut <- getFeatures(path = "input/test.csv",onehot = F,requestedYears = 2008)
#dataset$Label <- as.factor(dataset$Label)
inTrain <- createDataPartition(y = dataset$WnvPresent,p = .5,list = F)
#label <- dataset[inTrain,"Label"]
training <- train[inTrain,] 
testing <- train[-inTrain,]

cvCtrl <- trainControl(method="cv", 
                       number = 5,
                       repeats = 1,
                       summaryFunction = twoClassSummary,classProb = TRUE)


library(rpart)

#fit <- rpart(formula = WnvPresent ~.,data = training, control = rpart.control(cp = 0.01))

rfFit <- train(form = WnvPresent ~.,data = training,
                  method = "rf",
                  trControl =cvCtrl, metric = "ROC")

rfClass <- predict(rfFit, newdata = testing) # classification outcome required for the confusion matrix
rfProb <- predict(rfFit, newdata = testing, type = "prob") # predicted data required for ROC curve

source("src/plots.R")
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
plotROC(probPrediction = lrProb,Label = testing$WnvPresent,fittedModel = lrFit,addToPrev= F)
plotROC(probPrediction = xgProb,Label = testing$WnvPresent,fittedModel = xgboostFit)
plotROC(probPrediction = rfProb,Label = testing$WnvPresent,fittedModel = rfFit)
library(ROCR)

pred <- prediction(lrProb[,2], Label=="Yes")
ROCR::performance(prediction.obj = pred,measure = "auc")
