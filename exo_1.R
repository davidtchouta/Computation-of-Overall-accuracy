library(caret)
data(iris)
iris <- iris[-which(iris$Species=="setosa"),]
y <- iris$Species
levels(y)
head(y, 100)
nombre_elements <- sum(iris$Species=="setosa")
nombre_elements <- sum(iris$Species=="versicolor")


# set.seed(2) # if using R 3.5 or earlier
set.seed(2)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

colhighacc <- function(x){
  rangedValues <- seq(range(x)[1], range(x)[2], by=0.1) #de la val min à la val max avec un pas de 0.1
  sapply(rangedValues, function(i){
    y_hat <-ifelse(x>i, "virginica","versicolor")
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5], 2, colhighacc)
sapply(predictions, max)


# Overall accuracy
predictions <- colhighacc(train[,4])
rangedValues <- seq(range(train[,4])[1], range(train[,4])[2],by=0.1)
cutoffs <- rangedValues[which(predictions==max(predictions))]

y_hat <-ifelse(test[,4]>cutoffs, "virginica","versicolor")
mean(y_hat==test$Species)

## test data set
colhighacc <- function(x){
  rangedValues <- seq(range(x)[1], range(x)[2], by=0.1) #de la val min à la val max avec un pas de 0.1
  sapply(rangedValues, function(i){
    y_hat <-ifelse(x>i, "virginica","versicolor")
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5], 2, colhighacc)
sapply(predictions, max)

##explanatory analysis
plot(iris,pch=21,bg=iris$Species)


petalLengthRange <- seq(range(train$Petal.Length)[1], range(train$Petal.Length)[2], by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1], range(train$Petal.Width)[2], by=0.1)

length_predictions <- sapply(petalLengthRange, function(i){
  y_hat <-ifelse(train$Petal.Length>i, "virginica","versicolor")
  mean(y_hat==train$Species)
})

length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange, function(i){
  y_hat <-ifelse(train$Petal.Width>i, "virginica","versicolor")
  mean(y_hat==train$Species)
})

width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.7

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff, "virginica", "versicolor")
mean(y_hat==test$Species)
  
  
  
  
  
  
