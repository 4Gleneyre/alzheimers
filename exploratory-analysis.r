library(ggplot2)
alzheimersData <- read.csv("oasis_longitudinal.csv")
summary(alzheimersData$SES)
str(alzheimersData)
alzheimersData <- subset(alzheimersData,alzheimersData$Group!='Converted')
alzheimersData <- subset(alzheimersData,!is.na(alzheimersData$MMSE))
summary(alzheimersData$Group)
nrow(alzheimersData)
ggplot(alzheimersData, aes(x=Group)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label=..count..), vjust = -1) +
  ylim(0,250) +
  ggtitle ("Figure 1: Count of Demented and Nondemented Visits") +
  theme (plot.title = element_text(hjust=0.5)) +
  labs (x = "Dementia Status")
ggplot(alzheimersData, aes(x=Group,fill=as.factor(MMSE))) +
  geom_bar() +
  ggtitle ("Figure 2: Count of Group by MMSE") +
  theme (plot.title = element_text(hjust=0.5)) +
  scale_fill_discrete(name="MMSE")
ggplot(alzheimersData, aes(x=Group, y = MMSE)) +
  geom_boxplot() +
  ggtitle ("Figure 2: Boxplot of MMSE by Dementia Status") +
  theme (plot.title = element_text(hjust=0.5)) +
  labs (x = "Dementia Status")
ggplot(alzheimersData, aes(x=Group, y = nWBV)) +
  geom_boxplot() +
  ggtitle ("Figure 3: Boxplot of nWBV by Dementia Status") +
  theme (plot.title = element_text(hjust=0.5)) +
  labs (x = "Dementia Status")
ggplot(alzheimersData, aes(x=Group, y = EDUC)) +
  geom_boxplot() +
  ggtitle ("Figure 4: Boxplot of Years of Education By Dementia Status") +
  theme (plot.title = element_text(hjust=0.5)) +
  labs (x = "Dementia Status", y = "Years of Education")
library(ggcorrplot)
str(alzheimersData)
alzheimersData$MRI.ID <- NULL
alzheimersData$Subject.ID <- NULL
alzheimersData$MR.Delay <- NULL
alzheimersData$Visit <- NULL
alzheimersData$ASF <- NULL
alzheimersData$Hand <- NULL
alzheimersData$CDR <- NULL
alzheimersData$Group <- NULL
alzheimersData$M.F <- NULL
alzheimersData$SES <- NULL
summary(alzheimersData)
colnames(alzheimersData)[which(names(alzheimersData) == "Yrs of Edu")] <- "Years of Education"
corr <- round(cor(alzheimersData),1)
ggcorrplot(corr, 
           type = "lower",
           method = "circle",
           lab = TRUE,
           colors = c("tomato2","white","springgreen3"),
           title = "Figure 5: Correlogram of Continuous Variables",
           ggtheme = theme(plot.title = element_text(hjust = 0.5)),
           legend.title = "Correlation"
           )
