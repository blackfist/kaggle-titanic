library(caret)
library(rpart)
library(dplyr)

# Read the kaggle data
inData <- read.csv("train.csv")
submitData <- read.csv("test.csv")

# We should change our outcome variable to a factor
inData$Survived <- factor(inData$Survived)
inData$Pclass <- factor(inData$Pclass)
submitData$Pclass <- factor(submitData$Pclass)

randomSelection <- createDataPartition(inData$Survived, p=0.7, list=FALSE)
modelTrain <- inData[randomSelection, ]
modelTest <- inData[-randomSelection, ]

# Explore the data a bit
plot(density(modelTrain$Age, na.rm=TRUE))
plot(density(modelTrain$Fare, na.rm = T))
counts <- table(modelTrain$Survived, modelTrain$Sex)
# counts <- prop.table(counts, 2)
barplot(counts, xlab = "Gender", ylab = "Number of People", main = "survived and deceased between male and female")


# Now train a model using random forest
rfModel <- train(Survived ~ Pclass + Sex, data=modelTrain, method="rf", verbose=F)
# adaboost
adaModel <- train(Survived ~ Pclass + Sex, data=modelTrain, method="ada", verbose=F)
# decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=modelTrain, method="class")

# Predict something
rfPredictions <- predict(rfModel, modelTest)
adaPredictions <- predict(dfModel, modelTest)
dfPredictions <- predict(fit, modelTest, type = "class")

# Check it
confusionMatrix(rfPredictions, modelTest$Survived)
confusionMatrix(adaPredictions, modelTest$Survived)
confusionMatrix(dfPredictions, modelTest$Survived)

# Now add predictions to the submitData. To get the csv to write properly I need to 
# convert the factor back to an integer. But when I do that everything gets incremented
# by one, so then I have to take that back off.
submitData$Survived <- as.integer(predict(rfModel, submitData)) -1
submitData %>% select(PassengerId, Survived) %>% write.csv(file="submission.csv", row.names=F)

submitData$Survived <- predict(fit, submitData, type = "class")
submitData %>% select(PassengerId, Survived) %>% write.csv(file="submission.csv", row.names=F)
