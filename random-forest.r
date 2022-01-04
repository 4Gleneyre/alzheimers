library(tidyverse)
library(randomForest)
alzheimersData <- read.csv("oasis_longitudinal.csv")
alzheimersData <- subset(alzheimersData,!is.na(alzheimersData$MMSE))
str(alzheimersData)
alzheimersData$Group <- factor(alzheimersData$Group)
index <- sample(1:nrow(alzheimersData), size = nrow(alzheimersData) * 0.6)
training <- alzheimersData[index,]
testing <- alzheimersData[-index,]
nrow(training)
nrow(testing)

rf <- randomForest(Group ~ MMSE + eTIV + nWBV + Age + M.F + EDUC, data = training, mtry = 2, ntree =3001, importance = TRUE)
plot(rf)
legend("top", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)
attributes(rf)
rf
importance(rf)
varImpPlot(rf)

pred <- predict(rf,newdata=testing, type="class")

head(pred)

nrow(testing)
head(testing$Group)
library(caret)
library(e1071)
class(pred)
confusionMatrix(pred,testing$Group)
library(pROC)
g <- roc(testing$Group ~ as.numeric(pred), data = testing)
plot(g, main = "")
