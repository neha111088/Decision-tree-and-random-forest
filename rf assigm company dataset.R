install.packages("randomForest")
install.packages("Mass")
install.packages("caret")
library(randomForest)
library(MASS)
library(caret)


set.seed(100)

hist(Company_Data$Sales)

highsale = ifelse(Company_Data$Sales<= 7, "low", "high")

high_saletemp= data.frame(Company_Data,highsale)
sales =high_saletemp[,c(1:12)]

str(sales)
table(sales$highsale)


test<-sales[1:300,]
train<-sales[301:400,]

rf <- randomForest(highsale~., data=train)


pred1 <- predict(rf, train)
head(pred1)
head(train$highsale)

pred2 <- predict(rf, test)
confusionMatrix(pred2, test$highsale)


pred2 <- predict(rf, test)
confusionMatrix(pred2, test$highsale)

plot(rf)


hist(treesize(rf), main = "No of Nodes for the trees", col = "blue")


rf1 <- randomForest(highsale~., data=train, ntree = 200, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf1