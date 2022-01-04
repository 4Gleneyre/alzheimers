alzheimersData <- read.csv("oasis_longitudinal.csv")
alzheimersData <- subset(alzheimersData,!is.na(alzheimersData$MMSE))
alzheimersData$Group <- factor(alzheimersData$Group)

library('caret')
str(alzheimersData)

head(alzheimersData)
index <- sample(1:nrow(alzheimersData), size = nrow(alzheimersData) * 0.6)
training <- alzheimersData[index,]
testing <- alzheimersData[-index,]
nrow(training)
nrow(testing)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
summary(trctrl)
model <- train(Group ~ MMSE + eTIV + nWBV + M.F + Age, data = training, method = "svmLinear",
               trControl = trctrl,
               preProcess = c("center","scale"),
               tuneLength = 10)
summary(model)
model
test_pred <- predict(model, newdata = testing)
test_pred

confusionMatrix(table(test_pred,testing$Group))
nrow(testing)
grid <- expand.grid(C = c(0,0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.7,2))
grid
model.grid <- train(Group ~ MMSE + eTIV + nWBV + M.F + Age, data = training, method = "svmLinear",
                    trControl = trctrl,
                    preProcess = c("center","scale"),
                    tuneGrid = grid,
                    tuneLength = 10)
model.grid

plot(model.grid, xlab = "C value", ylab = "Accuracy")

test_pred_grid <- predict(model.grid,newdata=testing)
confusionMatrix(table(test_pred_grid, testing$Group))

library(pROC)
g <- roc(as.numeric(testing$Group) ~ as.numeric(test_pred_grid))
plot(g, main = "")
