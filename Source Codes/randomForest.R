library(rpart)
library(rpart.plot)
library(caret)
library(ROCR)
library(randomForest)
library(rattle)
library(tree)

wine<-read.csv("../wineQualityWhites.csv" )

wine$quality<-as.factor(wine$quality)

set.seed(1)
#Dividing the dataset into Training and Testing sets.
index<-createDataPartition(wine$quality, p= .9, list=FALSE)
Train<-wine[index,]
Test<-wine[-index,]

set.seed(1)
model2<-randomForest(Train$quality~., data=Train, ntree=50, do.trace=T, importance=T)
varImpPlot(model2)

# making predictions 
pred2<-predict(model2, newdata=Test, type="class")
confusionMatrix(pred2, Test$quality)
numFolds <- trainControl(method = "cv", number = 5)
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(quality ~ ., data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
