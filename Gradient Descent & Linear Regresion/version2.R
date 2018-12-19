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
library(qpcR)
library(Metrics)


set.seed(1111)
context1 <- fread("student-mat.csv", stringsAsFactors = FALSE)
glimpse(context1)
context1 <- data.frame(context1[,-31:-32])
summary(context1)
#?plot
#summary(context1)
context1$school <- as.factor(context1$school)
context1$sex <- as.factor(context1$sex)
context1$Mjob <- as.factor(context1$Mjob)
context1$Fjob <- as.factor(context1$Fjob)
context1$address <- as.factor(context1$address)
context1$famsize <- as.factor(context1$famsize)
context1$Pstatus <- as.factor(context1$Pstatus)
context1$reason <- as.factor(context1$reason)
context1$guardian <- as.factor(context1$guardian)
context1$schoolsup <- as.factor(context1$schoolsup)
context1$famsup <- as.factor(context1$famsup)
context1$paid <- as.factor(context1$paid)
context1$activities <- as.factor(context1$activities)
context1$nursery <- as.factor(context1$nursery)
context1$higher <- as.factor(context1$higher)
context1$internet <- as.factor(context1$internet)
context1$romantic <- as.factor(context1$romantic)

#plot(context1$G3,context1$Fedu)

#lapply(context1, var)

#data_var = cor(context1)
# head(context1)
#?cor
#?scale
scaledData = data.frame(normalize(context1))
#?ggcorr
ggcorr(scaledData, label_alpha = TRUE, nbreaks = 4)

# Data Splitting
#?createDataPartition()
inTrain <- createDataPartition(y= scaledData$G3, p= 0.70, list = FALSE)

training <- scaledData[inTrain,]
testing <- scaledData[-inTrain,]
#dim(training)

# Fit a Model
#?train

modelfit <- train(G3~., data = training, method = "lm")
modelfit
summary(modelfit)
RMSE(modelfit)

#modelfit$finalModel

#?predict
predictions <- predict(modelfit, newdata= testing)
summary(predictions)

?RMSE
rmse(testing$G3, predicted = predictions)
# range(context1$G3)
# Final Model
#modelfit2$finalModel


#RMSE(predictions2)

# plot(predictions)
# plot(predictions2)
# plot(modelfit2)
# abline(reg = modelfit2, col="blue")
# ?abline
# plot(resid(modelfit2))
# plot(testing$G3, predictions2)
# plot(testing$G3, resid(modelfit))
# plot(training$G3, predictions)

# ?varImp
# plot(varImp(modelfit))
plot(varImp(modelfit))


context3 <- data.frame(obs= testing$G3, pred= predictions2)
defaultSummary(context3)
# Confusion Matrix
confusionMatrix(predictions2, testing$G3)
accuracy(predictions2,testing$G3)
accuracy(predictions3,testing$G3)

accuracy(predictions,testing$G3)
?lm
?varImp
plot(varImp(scaledData))
predictions2
plot(varImp(predictions2))

predictions3 <- predict(modelfit3, newdata= testing)
predictions3

################# Taking random 10 variables
?select
scaledData2 <- data.frame(cbind(scaledData[,1:10], scaledData$G3))

inTrain2 <- createDataPartition(scaledData2$V2, p=0.7, list = FALSE)
training2 <- scaledData2[inTrain2]
testing2 <- scaledData2[-inTrain2]
modelfit2 <- train(V2~., data = training2, method= 'lm')
modelfit2
summary(modelfit2)
RMSE(modelfit2)


################### Intution based most important 10 variables
plot(varImp(modelfit))
plot(varImp(scaledData))
typeof(context1)
