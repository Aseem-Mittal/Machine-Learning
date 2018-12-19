library(caret)
library(kernlab)
library(data.table)
library(dplyr)
library(ggplot2)
library(sandwich) # For White correction
library(lmtest) # More advanced hypothesis testing tools
library(plm)
library(gradDescent)
library(data.table)
library(GGally)
library(BBmisc)

set.seed(1111)
context1 <- fread("student-mat.csv", stringsAsFactors = FALSE)
glimpse(context1)
#?plot
summary(context1)
context1$school <- as.numeric(as.factor(context1$school))
context1$sex <- as.numeric(as.factor(context1$sex))
context1$Mjob <- as.numeric(as.factor(context1$Mjob))
context1$Fjob <- as.numeric(as.factor(context1$Fjob))
context1$address <- as.numeric(as.factor(context1$address))
context1$famsize <- as.numeric(as.factor(context1$famsize))
context1$Pstatus <- as.numeric(as.factor(context1$Pstatus))
context1$reason <- as.numeric(as.factor(context1$reason))
context1$guardian <- as.numeric(as.factor(context1$guardian))
context1$schoolsup <- as.numeric(as.factor(context1$schoolsup))
context1$famsup <- as.numeric(as.factor(context1$famsup))
context1$paid <- as.numeric(as.factor(context1$paid))
context1$activities <- as.numeric(as.factor(context1$activities))
context1$nursery <- as.numeric(as.factor(context1$nursery))
context1$higher <- as.numeric(as.factor(context1$higher))
context1$internet <- as.numeric(as.factor(context1$internet))
context1$romantic <- as.numeric(as.factor(context1$romantic))
context1$G1 <- as.numeric(as.integer(context1$G1))
context1$G2 <- as.numeric(as.integer(context1$G2))
plot(context1$G3,context1$Fedu)

lapply(context1, var)
context1 <- context1[,-31:-32]
#data_var = cor(context1)
# head(context1)
#?cor
?scale
scaledData = normalize(context1)
#?ggcorr
ggcorr(scaledData, label_alpha = TRUE, nbreaks = 4)

# Data Splitting
?createDataPartition()
inTrain <- createDataPartition(y= scaledData$G3, p= 0.70, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

# Fit a Model
?train
modelfit <- train(G3~., data = training, method = "lmStepAIC")
modelfit

modelfit2 <- train(G3~., data = training, method = "lm")
modelfit2
summary(modelfit2)
# Final Model
modelfit$finalModel

modelfit3 <- lm(G3~., data = context1)
summary(modelfit3)

predictions <- predict(modelfit, newdata= training)
predictions
context2 <- cbind(testing, predictions)
plot(training$G3, resid(modelfit))
plot(testing$G3, predictions)
plot(testing$G3, resid(modelfit))
plot(training$G3, predictions)

?varImp
plot(varImp(modelfit))
plot(varImp(modelfit2))

predictions2 <- predict(modelfit2, newdata= testing)
context3 <- data.frame(obs= testing$G3, pred= predictions2)
defaultSummary(context3)
# Confusion Matrix
confusionMatrix(predictions2, testing$G3)
accuracy(predictions2)
predictions2
plot(varImp(predictions2))

predictions3 <- predict(modelfit3, newdata= testing)
predictions3




