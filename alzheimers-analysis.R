# 1) logistic regression 2) 
library(tidyverse)
alzheimersData <- read.csv("oasis_longitudinal.csv")
str(alzheimersData)
alzheimersData <- subset(alzheimersData,alzheimersData$Group!='Converted')
levels(alzheimersData$Group)
nrow(alzheimersData)
alzheimersData$nGroup[alzheimersData$Group == "Demented"] = 1
alzheimersData$nGroup[alzheimersData$Group == "Nondemented"] = 0
table(alzheimersData$nGroup)
alzheimersData$Group <- alzheimersData$nGroup
alzheimersData <- subset(alzheimersData, !is.na(alzheimersData$MMSE))
library(caTools)
nrow(alzheimersData)
index <- sample(1:nrow(alzheimersData), size = nrow(alzheimersData) * 0.8)
train <- alzheimersData[index,]
test <- alzheimersData[-index,]
training <- train
testing <- test
nrow(train)
nrow(test)
model2 <- glm(Group ~ nWBV + eTIV + Age + MMSE + EDUC + M.F , na.action = na.omit,data = train, family = 'binomial')
class(model2)
logisticRes <- predict(model2, test,type="response")

levels(alzheimersData$M.F)
confmatrix <- table(Actual_value = test$Group, Predicted_value = res > 0.4)
confmatrix
(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)
model2$coefficients
exp(model2$coefficients[-1])
test$Prob <- res

library(pROC)
g <- roc(test$Group ~ res, data = test)
plot(g, main = "")

library(randomForest)
library(tidyverse)
rf <- randomForest(Group ~ MMSE + eTIV + nWBV + Age + M.F + EDUC, data = training, mtry = 2, ntree =3001, importance = TRUE)
plot(rf)
legend("top", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)
attributes(rf)
rf
importance(rf)
varImpPlot(rf)

rfRes <- predict(rf,newdata=testing, type="class")

head(rfRes)

nrow(testing)
head(testing$Group)
library(caret)
library(e1071)
class(pred)
head(pred)
confusionMatrix(pred,testing$Group)

print(data.frame(logisticRes),row.names = FALSE)
print(data.frame(rfRes),row.names = FALSE)

train.data <- subset(train,select=c(6,8,9,11,13,14))
train.data <- as.data.frame(lapply(train.data[,2:6],normalize))
train.labels <- subset(train,select=c(3))
test.data <- subset(test,select=c(6,8,9,11,13,14))
test.data <- as.data.frame(lapply(test.data[,2:6],normalize))
head(test.data)
test.labels <- subset(test,select=c(3))
test.labels = test.labels[,1]
train.labels = train.labels[,1]
class(train.labels)
library(class)
knn.16 <- knn(train = train.data, test = test.data, cl = train.labels, k = 1)
knn.17 <- knn(train = train.data, test = test.data, cl = train.labels, k = 17)

ACC.16 <- 100 *sum(test.labels == knn.16) / length(test.labels)
ACC.17 <- 100 *sum(test.labels == knn.17) / length(test.labels)
ACC.17
k.optm = 1
for (i in 1:216) {
  knn.mod <- knn(train = train.data, test = test.data, cl = train.labels, k = i)
  acc.mod <- 100 *sum(test.labels == knn.mod) / length(test.labels)
  k.optm[i] <- acc.mod
}
print(k.optm)
plot(k.optm, type="l", xlab="K- Value", ylim = c(50,85),ylab="Accuracy (% Correct)", main = "Figure 9: Accuracy Plot of KNN Algorithm")


knn.7 <- knn(train=train.data, test=test.data, cl = train.labels, k = 7)
table(knn.7,test.labels)
sum(test.labels == knn.7)/NROW(test.labels)
y <- data.frame(knn.7)
print(y,row.names = FALSE)
class(test.labels)
class(knn.1)
library(pROC)
g <- roc(test.labels ~ as.numeric(knn.1))
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
summary(trctrl)
train$Group <- as.factor(train$Group)
model <- train(Group ~ MMSE + eTIV + nWBV + M.F + Age, data = train, method = "svmLinear",
               trControl = trctrl,
               preProcess = c("center","scale"),
               tuneLength = 10)
summary(model)
model
test_pred <- predict(model, newdata = testing)

print(data.frame(test_pred),row.names=FALSE)

confusionMatrix(table(test_pred,testing$Group))
#Neural networks
library(neuralnet)
nrow(train)
nrow(test)
head(train)
head(test)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
train$Age <- range01(train$Age)
train$EDUC <- range01(train$EDUC)
train$MMSE <- range01(train$MMSE)
train$M.F <- range01(train$M.F)
train$eTIV <- range01(train$eTIV)
summary(train)
train$MMSE <- as.numeric(train$MMSE)
summary(train)
train$M.F <- as.numeric(train$M.F)
n <- neuralnet(Group~MMSE + eTIV + nWBV + Age + EDUC + M.F,
               data = train,
               hidden = 1,
               err.fct = "ce",
               linear.output = FALSE,
)
nrow(test)
print(n)
test$M.F <- as.numeric(test$M.F)
test$Age <- range01(test$Age)
test$EDUC <- range01(test$EDUC)
test$MMSE <- range01(test$MMSE)
test$M.F <- range01(test$M.F)
test$eTIV <- range01(test$eTIV)
output <- compute(n,test)
class(output$net.result)
print(data.frame(output$net.result[,2]),row.names=FALSE)

print(data.frame(test$Group),row.names=FALSE)
