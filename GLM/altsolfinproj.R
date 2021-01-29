income.svm.1=svm(income~race+mari.sta+workclass+agest+gender+hourst+edu,data=down_train,
                 kernel="linear",probability=F,cost=10,scale =FALSE)
print(income.svm.1)
summary(income.svm.1)
pre.train <- fitted(income.svm.1)

svm.pred=predict(income.svm.1,newdata=train,type="response")#fitted value
confusionMatrix(svm.pred,train$income, positive = ">50K")

svm.pred=predict(income.svm.1,newdata=test,type="response")#fitted value
confusionMatrix(svm.pred,test$income, positive = ">50K")

income.svm.2=svm(income~race+mari.sta+workclass+agest+gender+hourst+edu,data=down_train,kernel="radial",cost=10,scale =FALSE)
print(income.svm.2)
summary(income.svm.2)
pre.train2 <- fitted(income.svm.2)

svm.pred=predict(income.svm.2,newdata=train,type="response")#fitted value
confusionMatrix(svm.pred,train$income, positive = ">50K")

svm.pred=predict(income.svm.2,newdata=test,type="response")#fitted value
confusionMatrix(svm.pred,test$income, positive = ">50K")

