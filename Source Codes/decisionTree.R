library(rpart)
library(rpart.plot)
library(caret)
library(ROCR)
library(rattle)
library(readxl)
library(tree)
wine<-read.csv("../wineQualityWhites.csv")
str(wine)
wine$quality<-as.factor(wine$quality)
wine$X<-NULL
set.seed(1)


index<-createDataPartition(wine$quality, p= .9, list=FALSE)
wine_train<-wine[index,]
wine_test<-wine[-index,]


set.seed(1)
tree<-rpart(wine$quality~., data=wine)
prp(tree, type=3, extra=3, tweak=0.8, main="The Quality of Wine", compress=TRUE )

model1<-rpart(wine_train$quality~., data=wine_train)
pred<-predict(model1, wine_test, type="class")
confusionMatrix(pred, wine_test$quality)


set.seed(1815850)

train.control <- trainControl(method = "cv", number = 3)
model <- train(quality ~., data = wine, method = "ctree",trControl = train.control)
model

