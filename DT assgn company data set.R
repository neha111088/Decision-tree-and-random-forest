install.packages("C50")
library(C50)

install.packages("tree")

View(Company_Data)

summary(Company_Data)

attach(Company_Data)

is.na(Company_Data)
sum(is.na(Company_Data))

data <- Company_Data$Sales
View(data)


Company_Data$Sales <- as.factor(Company_Data$Sales)

train<-Company_Data[1:300,]
test<-Company_Data[302:400,]


train_C5.0<-C5.0(train[,-1],train$Sales)

windows()
plot(train_C5.0)


pred_train<-predict(train_C5.0,train)

mean(train$Sales==pred_train)

library(caret)
confusionMatrix(pred_train,train$Sales)


test_C5.0<-C5.0(test[,-1],test$Sales)
plot(test_C5.0)

pred_test<-predict(test_C5.0,test)

mean(test$Sales==pred_test)

confusionMatrix(pred_test,test$Sales)

library(gmodels)
CrossTable(test$Sales,pred_test)
