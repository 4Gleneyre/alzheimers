library(tidyverse)
alzheimersData <- read.csv("oasis_longitudinal.csv")
str(alzheimersData)

# Min-max normalization
alzheimersData$Group <- factor(alzheimersData$Group)
levels(alzheimersData$Group)
alzheimersData$nGroup[alzheimersData$Group == 'Demented'] = 1
alzheimersData$nGroup[alzheimersData$Group == 'Nondemented'] = 0
alzheimersData$Group <- alzheimersData$nGroup
str(alzheimersData)
alzheimersData$M.F <- as.numeric(alzheimersData$M.F)
alzheimersData <- subset(alzheimersData,!is.na(alzheimersData$MMSE))
alzheimersData$MMSE <- (alzheimersData$MMSE - min(alzheimersData$MMSE)) / (max(alzheimersData$MMSE) - min(alzheimersData$MMSE))
alzheimersData$eTIV <- (alzheimersData$eTIV - min(alzheimersData$eTIV)) / (max(alzheimersData$eTIV) - min(alzheimersData$eTIV))
alzheimersData$nWBV <- (alzheimersData$nWBV - min(alzheimersData$nWBV)) / (max(alzheimersData$nWBV) - min(alzheimersData$nWBV))
alzheimersData$ASF <- (alzheimersData$ASF - min(alzheimersData$ASF)) / (max(alzheimersData$ASF) - min(alzheimersData$ASF))
alzheimersData$Age <- (alzheimersData$Age - min(alzheimersData$Age)) / (max(alzheimersData$Age) - min(alzheimersData$Age))
alzheimersData$EDUC <- (alzheimersData$EDUC - min(alzheimersData$EDUC)) / (max(alzheimersData$EDUC) - min(alzheimersData$EDUC))
str(alzheimersData)
summary(alzheimersData)
#Data partition
index <- sample(1:nrow(alzheimersData), size = nrow(alzheimersData) * 0.6)
training <- alzheimersData[index,]
testing <- alzheimersData[-index,]
nrow(training)
nrow(testing)

#Neural networks
library(neuralnet)
set.seed(333)
class(training)
class(training$Group)
n <- neuralnet(Group~MMSE + eTIV + nWBV + Age + EDUC + M.F,
               data = training,
               hidden = 1,
               err.fct = "ce",
               linear.output = FALSE,
              )
plot(n)
n$result.matrix

# neural network uses the sigmoid activation function

# Prediction
output <- compute(n,training[,-3])

# Confusion Matrix and Misclassification Error
netResult <- output$net.result
output$net.result
netResult
pred1 <- ifelse(netResult>=0.5,1,0)
tab1 <- table(pred1,training$Group)
tab1
sum(diag(tab1)/sum(tab1))
summary(training)
output1 <- compute(n,testing)
head(testing)
netResult1 <- output1$net.result
pred1 <- ifelse(netResult1>=0.5,1,0)
tab1 <- table(pred1,testing$Group)
print(netResult1)
tab1
sum(diag(tab1)/sum(tab1))

# neural network with 2 hidden layers (using only MMSE, eTIV, and nWBV)
n1 <- neuralnet(Group~MMSE + eTIV + nWBV + Age + EDUC + M.F,
               data = training,
               hidden = 2,
               err.fct = "ce",
               linear.output = FALSE,
)

plot(n1, rep = 3)
plot(n1)

str(training)
output <- compute(n1,training[,-3])
netResult <- output$net.result
output$net.result
netResult
pred1 <- ifelse(netResult>=0.5,1,0)
tab1 <- table(pred1,training$Group)
tab1
sum(diag(tab1)/sum(tab1))

output1 <- neuralnet::compute(n1, testing[-3])
netResult1 <- output1$net.result
head(netResult1)
pred1 <- ifelse(netResult1>=0.5,1,0)
tab1 <- table(pred1,testing$Group)
tab1
sum(diag(tab1)/sum(tab1))
n1$result.matrix

par(mfrow=c(1,1))
gwplot(n1,selected.covariate = "MMSE")
gwplot(n1,selected.covariate = "eTIV")
gwplot(n1,selected.covariate = "nWBV")
gwplot(n1,selected.covariate = "Age")
gwplot(n1,selected.covariate = "EDUC")
gwplot(n1,selected.covariate = "M.F")

