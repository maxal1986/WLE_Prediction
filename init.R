library(ggplot2)
library(caret)
library(rattle)
library(corrplot)

train_csv <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_csv <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

files <- downloadFiles(train_csv, test_csv)

training <- createDataset(files[1])
training <- filterVariables(training)

predicting <- createDataset(files[2])
predicting <- filterVariables(predicting)

rm(train_csv, test_csv, files)

set.seed(12345)
inTrain <- createDataPartition(y = training$classe,
                               p = 0.6, list = FALSE)
myTraining <- training[inTrain, ]
myTesting <- training[-inTrain, ]

corMat <- cor(myTraining[,-dim(myTraining)[2]],)
corrplot(corMat, method = "color", type="lower", order="hclust", tl.cex = 0.75, tl.col="black", tl.srt = 45)

exclude <- findCorrelation(corMat, cutoff = 0.5)
myNewTraining <- myTraining[,-exclude]

modFit <- train(classe ~ ., method = "rf", data = myNewTraining)
predictions <- predict(modFit, myTesting)
confMat <- confusionMatrix(predictions, myTesting$classe)
confMat$table

accuracy <- sum((predictions == myTesting$classe))/dim(myTesting)[1]
oos_error <- 1 - accuracy