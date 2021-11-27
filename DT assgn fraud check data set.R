install.packages("C50")
install.packages("tree")
library(C50)

View(Fraud_check)

summary(Fraud_check)

attach(Fraud_check)

is.na(Fraud_check)
sum(is.na(Fraud_check))

data<-Fraud_check$Taxable.Income
View(data)

Fraud_check$Taxable.Income<-as.factor(Fraud_check$Taxable.Income)

train<-Fraud_check[1:400,]
test<-Fraud_check[401:600,]


train_C5.0<-C5.0(train[,-3],train$Taxable.Income)
windows()

plot(train_C5.0)


pred_train<-predict(train_C5.0,train)

mean(train$Taxable.Income==pred_train)  

library(caret)

confusionMatrix(pred_train,train$Taxable.Income)

test_C5.0<-C5.0(test[,-3],test$Taxable.Income)
plot(test_C5.0)


pred_test<-predict(test_C5.0,test)

mean(test$Taxable.Income==pred_test)

confusionMatrix(pred_test,test$Taxable.Income)

library(gmodels)
CrossTable(test$Taxable.Income,pred_test)
