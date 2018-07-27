#https://will-stanton.com/2015/03/08/machine-learning-with-r-an-irresponsibly-fast-tutorial/

library(caret)
library(randomForest)
library(plyr)

testset<-read.table("test.csv",sep=",",header=TRUE)
trainset<-read.table("train.csv",sep=",",header=TRUE)

table(trainset[,c("Survived","Pclass")])
table(trainset[,c("Survived","Sex")])

trainset$Survived=as.factor(trainset$Survived)
#trainset$Survived=revalue(trainset$Survived,c("1"="Survived","0"="Died"))
plot(trainset$Survived,trainset$Age)
plot(trainset$Survived,trainset$Sex)
plot(trainset$Survived,trainset$Fare)


###Model time####
set.seed(42)
model=train(Survived~Pclass+Sex+SibSp+Embarked+Parch+Fare,
             data=trainset,
             method="rf",
             trControl=trainControl(method="cv", number=5))
model

testset$Fare<-ifelse(is.na(testset$Fare),mean(testset$Fare,na.rm=TRUE),testset$Fare)
testset$Survived<-predict(model,newdata=testset)
submission<-testset[,c("PassengerId","Survived")]
write.table(submission,file="submission.csv",col.names=TRUE,row.names=FALSE,sep=",")
