rm(list=ls())
gc()

#####################################################################
################ Chapter09. Classification Algorithm ################
#####################################################################

# install.packages("Hmisc")
library(Hmisc)

data("mtcars")
str(mtcars)

describe(mtcars)
summary.formula(mpg ~ cyl + hp, data = mtcars)
summary(mpg ~ cyl + hp, data = mtcars, fun = var)
summary.formula(mpg ~ cyl + hp, data = mtcars, method = "reverse")
summary.formula(mpg ~ cyl + hp, data = mtcars, method = "cross")

plot(iris)
plot(iris$Sepal.Length)
plot(iris$Species)

plot(Species ~ Sepal.Length, data = iris)
with(iris,{
  plot(Sepal.Length, Sepal.Width, pch = as.numeric(Species))
  legend("topright", legend = levels(iris$Species), pch = 1:3)
})

# install.packages("caret", dependencies = TRUE)
library(caret)
featurePlot(iris[,1:4], iris$Species, "ellipse")

### scale() = x - mu / sigma
cbind(as.data.frame(scale(iris[1:4])), iris$Species)


### PCA (Principal Component Analysis)
x <- 1:10
y <- x + runif(10, min = -0.5, max = 0.5)
z <- x + y + runif(10, min = -0.1, max = 0.1)
data <- data.frame(x, y, z)
data
pr <- princomp(data)
summary(pr)
pr$scores
loadings(pr)
plot(pr, type = "lines")
biplot(pr)

### one-hot encoding
x <- data.frame(lvl = factor(c("A","B","A","A","C")),
                value = c(1,3,2,4,5))
model.matrix(~ lvl, data = x)[,-1]

### Imputation of NA
iris_na <- iris
iris_na[c(10,20,25,40,32),3] <- NA
iris_na[c(33,100,123),1] <- NA
iris_na[!complete.cases(iris_na),]
iris_na[is.na(iris_na$Sepal.Length),]
apply(iris_na[,1:4], 2, function(x){
  median(x, na.rm = T)
})

# install.packages("DMwR")
library(DMwR)
iris_na[!complete.cases(iris_na),]
row.names(iris_na[!complete.cases(iris_na),])

# central value of all data (ex. central value of Sepal.Length / NOT by Species)
# if imputate to central value of data by Species, predictions will increase.
centralImputation(iris_na[1:4])[
  as.numeric(row.names(iris_na[!complete.cases(iris_na),])),]
knnImputation(iris_na[,1:4])[
  as.numeric(row.names(iris_na[!complete.cases(iris_na),])),]

##### Variable Selection (Feature Selection)

### Near Zero Variance
library(caret)
library(mlbench)
data("Soybean")
str(Soybean)
nearZeroVar(Soybean, saveMetrics = TRUE)
nearZeroVar(Soybean) # Index number of Column
mySoybean <- Soybean[,-nearZeroVar(Soybean)]

### Correlation 
data("Vehicle")
head(Vehicle)
str(Vehicle)
findCorrelation(cor(subset(Vehicle, select = -c(Class))))

# install.packages("FSelector")
library(FSelector)
data(Ozone)
head(Ozone)
v <- linear.correlation(V4 ~ ., data = subset(Ozone, select = -c(V1,V2,V3)))
v
cutoff.k(v, 5)

### Chi-Squared Test
library(FSelector)
library(mlbench)
data("Vehicle")
cs <- chi.squared(Class ~., data = Vehicle)
cs

### Model Validation 
predicted <- c(1,0,0,1,1,1,0,0,0,1,0,0)
actual <- c(1,0,0,1,1,0,1,1,0,0,0,0)
xtabs(~predicted+actual)

confusionMatrix(predicted, actual)

set.seed(137)
probs <- runif(100)
labels <- as.factor(ifelse(probs>0.5 & runif(100)<0.4, "A", "B"))
# install.packages("ROCR")
library(ROCR)
pred <- prediction(probs, labels)
plot(performance(pred, "tpr", "fpr"))
performance(pred, "auc")


### Cross validation
# install.packages("cvTools")
library(cvTools)
cvFolds(10, 5, type = "random")
cvFolds(10, 5, type = "consecutive")

set.seed(719)
cv <- cvFolds(NROW(iris), K=10, R=3)
cv
head(cv$which, 30)
head(cv$subsets)

library(foreach)
set.seed(719)
R = 3
K = 10
cv <- cvFolds(NROW(iris), K=K, R=R)

# foreach(r=1:R) %do% {
#   foreach(k=1:K, .combine = c) %do% {
#     validation_idx <- cv$subsets[which(cv$which == k), r]
#     train <- iris[-validation_idx,]
#     validation <- iris[validation_idx,]
#     # Data preprocessing
#     
#     
#     # train model
#     
#     
#     # predictrion
#     
#     
#     # performance evaluation
#     # return(performance value)
#   }
# }

# createDataPartition(), createFolds(), createMultiFolds()
parts <- createDataPartition(iris$Species, p=0.8) #iris의 80%를 훈련데이터로 쓸거임 #분류기준은 Species
parts
table(iris[parts$Resample1, "Species"]) #분류기준별로 몇개의 훈련 데이터가 뽑혓는지 확인
table(iris[-parts$Resample1, "Species"]) #분류기준별로 몇개의 테스트 데이터가 뽑혓는지 확인 
createFolds(iris$Species, k=10)
createMultiFolds(iris$Species, k=10, times=3)

k <- 10
times <- 3
set.seed(137)
cv <- createMultiFolds(iris$Species, k, times)

# for (i in 1:times){
#   for (j in 1:k){
#     train_idx <- cv[[i*times + k]] ------------------ i*times + k ?? 확인해볼 것 !
#     iris_train <- iris[train_idx,]
#     iris_validation <- iris[-train_idx,]
#     # modeling
#     # ...
#     # evaluation
#     # ...
#   }
# }