library(dplyr)
library(class) 
library(gmodels)
library(tree)

theme_set(theme_classic())

wine <- read.csv ("C:/Users/hale irem beyaz/Desktop/DM/wineQualityWhites.csv")
glimpse(wine)

wine$X <- NULL  # removing the first column 'X'
str(wine)
summary(wine)


table(wine$quality)

round(prop.table(table(wine$quality)) * 100, digits = 1)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

wine_n <- as.data.frame(lapply(wine[1:11], normalize))
summary(wine_n$fixed.acidity)

index<-createDataPartition(wine$quality, p= .9, list=FALSE)
wine_train<-wine[index,]
wine_test<-wine[-index,]

wine_train_labels <- wine[index, 12]
wine_test_labels <- wine[-index, 12] 

cl <- wine_train_labels

dt_acc <- numeric()

#cross validation
trial_sum <- numeric(20)
trial_n <- numeric(20)



for(i in 1:100){
  
  wine_sample <- sample(1:nrow(wine), size=nrow(wine)*0.9)
  wine_train <- wine[wine_sample,]
  wine_test <- wine[-wine_sample,]
  test_size <- nrow(wine_test)
  
  for(j in 1:20){
    predict <- knn(wine_train[,-12], wine_test[,-12], wine_train$quality, k=j)
    trial_sum[j] <- trial_sum[j] + sum(predict==wine_test$quality)
    trial_n[j] <- trial_n[j] + test_size
    
  }
}

plot(1-trial_sum / trial_n, type="l", ylab="Error Rate",xlab="K",main="RED WINE With Varying K (100 Samples)")
print(trial_sum / trial_n)
