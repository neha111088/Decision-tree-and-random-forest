install.packages("randomForest")
install.packages("MASS")
install.packages("caret")
library(randomForest)
library(MASS)
library(caret)

set.seed(100)

hist(Fraud_check$Taxable.Income)
hist(Fraud_check$Taxable.Income, main = "Sales of Company",xlim = c(0,100000),
     breaks=c(seq(40,60,80)), col = c("green","blue", "red","violet"))

Risky_Good = ifelse(Fraud_check$Taxable.Income<= 30000, "Risky", "Good")

fraud_checktemp= data.frame(Fraud_check,Risky_Good)
fraud_check = fraud_checktemp[,c(1:7)]


str(fraud_check)

table(fraud_check$Risky_Good)

train<-fraud_check[1:400,]
test<-fraud_check[401:600,]

rf <- randomForest(Risky_Good~., data=train)
rf



pred1 <- predict(rf, train)
head(pred1)
head(train$Risky_Good)


pred2 <- predict(rf, test)
confusionMatrix(pred2, test$Risky_Good)


plot(rf)


hist(treesize(rf), main = "No of Nodes for the trees", col = "blue")


rf1 <- randomForest(Risky_Good~., data=train, ntree = 200, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf1