library(tidyverse)
alzheimersData <- read.csv("oasis_longitudinal.csv")
alzheimersData$Group <- factor(alzheimersData$Group)
alzheimersData$Group <- as.numeric(alzheimersData$Group) # nondemented = 2, demented = 1
head(alzheimersData)
nrow(alzheimersData)
alzheimersData <- subset(alzheimersData,!is.na(alzheimersData$MMSE))
nrow(alzheimersData)
summary(alzheimersData)
alzheimersData$M.F <- as.numeric(alzheimersData$M.F)
alzheimersData$MMSE <- alzheimersData$MMSE - 17
alzheimersData.subset <- alzheimersData[c("Group","MMSE","nWBV","eTIV","Age","M.F","EDUC")]
head(alzheimersData.subset)
#normalize data
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE)-min(x, na.rm = TRUE)))
}

alzheimersData.subset.n <- as.data.frame(lapply(alzheimersData.subset[,2:7],normalize))
head(alzheimersData.subset.n)
summary(alzheimersData.subset.n)
nrow(alzheimersData)
set.seed(123)
dat.d <- sample(1:nrow(alzheimersData.subset.n), size = nrow(alzheimersData.subset.n)*0.6,replace=FALSE)

train.data <- alzheimersData.subset.n[dat.d,]
test.data <- alzheimersData.subset.n[-dat.d,]

head(train.data)
nrow(test.data)

train.labels <- alzheimersData.subset[dat.d,1]
test.labels <- alzheimersData.subset[-dat.d,1]

library(class)

class(train.data)
class(test.data)
class(train.labels)
knn.16 <- knn(train = train.data, test = test.data, cl = train.labels[1], k = 1)
knn.17 <- knn(train = train.data, test = test.data, cl = train.labels, k = 17)

ACC.16 <- 100 *sum(test.labels == knn.16) / length(test.labels)
ACC.17 <- 100 *sum(test.labels == knn.17) / length(test.labels)



k.optm = 1
for (i in 1:216) {
  knn.mod <- knn(train = train.data, test = test.data, cl = train.labels, k = i)
  acc.mod <- 100 *sum(test.labels == knn.mod) / length(test.labels)
  k.optm[i] <- acc.mod
}
summary(k.optm)
plot(k.optm, type="l", xlab="K- Value", ylim = c(50,85),ylab="Accuracy (% Correct)", main = "Figure 9: Accuracy Plot of KNN Algorithm")


knn.1 <- knn(train=train.data, test=test.data, cl = train.labels, k = 1)
table(knn.1,test.labels)
sum(test.labels == knn.1)/NROW(test.labels)
class(test.labels)
class(knn.1)
library(pROC)
g <- roc(test.labels ~ as.numeric(knn.1))

plot(g, main = "")
