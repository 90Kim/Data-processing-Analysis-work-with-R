rm(list=ls())
gc()

########################################################################
################### ch10. Classification Algorithm 2 ###################
########################################################################

##### 01. Logistic Regression

d <- subset(iris, Species == "virginica" | Species == "versicolor")
str(d)
d$Species
d$Species <- factor(d$Species)
d$Species

m <- glm(Species ~., data = d, family = "binomial")
m
fitted(m)[c(1:5, 51:55)]
f <- fitted(m)
as.numeric(d$Species)
ifelse(f > 0.5, 1, 0) == as.numeric(d$Species) - 1
is_correct <- ifelse(f > 0.5, 1, 0) == as.numeric(d$Species) - 1
sum(is_correct)
sum(is_correct) / NROW(is_correct)
predict(m, newdata = d[c(1,10,55),], type = "response")

##### 02. Multinomial Logistic Regression

library(nnet)
m2 <- multinom(Species ~ ., data = iris)
summary(m2)
fitted(m2)
predict(m2, newdata = iris[c(1,51,101),], type = "class")
predict(m2, newdata = iris[c(1,51,101),], type = "probs")
predicted <- predict(m2, newdata = iris)
sum(predicted == iris$Species) / NROW(iris)
xtabs(~ predicted + iris$Species)

##### 03. Dicision Tree

# install.packages("rpart")
library(rpart)

m3 <- rpart(Species ~ ., data = iris)
m3
plot(m3, compress = TRUE, margin = 0.2)
text(m3, cex = 1.5)

# install.packages("rpart.plot")
library(rpart.plot)
prp(m3, type = 4, extra = 2, digits = 3)

# install.packages("party")
library(party)
m4 <- ctree(Species ~ ., data = iris)
m4
plot(m4)

# install.packages("randomForest")
library(randomForest)
m5 <- randomForest(Species ~ ., data = iris)
m5

system.time(
  m5 <- randomForest(Species ~ ., data = iris)
)

system.time(
  m5 <- randomForest(iris[,1:4], iris[,5])
)

m6 <- randomForest(Species ~ ., data = iris, importance = TRUE)
importance(m6)
varImpPlot(m6)

library(cvTools)
library(foreach)
library(randomForest)
set.seed(719)
K <- 10
R <- 3
cv <- cvFolds(NROW(iris), K=K, R=R)
grid <- expand.grid(ntree = c(10,100,200), mtry = c(3,4))

# ## example ##
# 
# result <- foreach(g = 1:NROW(grid), .combine = rbind) %do% {
#   foreach(r = 1:R, .combine = rbind) %do% {
#     foreach(k = 1:K, .combine = rbind) %do% {
#       
#       # modeling
#       validation_idx <- cv$subsets[which(cv$which == k), r]
#       train <- iris[-validation_idx,]
#       validation <- iris[validation_idx,]
#       m <- randomForest(Species ~ ., data = train,
#                         ntree = grid[g,"ntree"], mtry = grid[g,"mtry"])
#       
#       # prediction
#       predicted <- predict(m, newdata = validation)
#       
#       # evaluation
#       precision <- sum(predicted == validation$Species) / NROW(predicted)
#       return (data.frame(g = g, precision = precision))
#     }
#   }
# }
# result
# 
# library(plyr)
# ddply(result, .(g), summarize, mean_precision = mean(precision))

##### 04. Neural Network

library(nnet)
m6 <- nnet(Species ~ ., data = iris, size = 3)
predict(m6, newdata = iris)
predict(m6, newdata = iris, type = "class")

class.ind(iris$Species)
m7 <- nnet(iris[,1:4], class.ind(iris$Species), size = 3, softmax = TRUE)

summary(m6)
summary(m7)

##### 05. Support Vector Machine - kernlab, el071

# install.packages("kernlab")
library(kernlab)
m8 <- ksvm(Species ~ ., data = iris)
m8
m9 <- ksvm(Species ~ ., data = iris, kernel = "vanilladot")
m9
m10 <- ksvm(Species ~., data = iris, kernel = "polydot", kpar = list(degree = 3))
m10

predict(m8, newdata = iris)
predict(m9, newdata = iris)
predict(m10, newdata = iris)

# install.packages("e1071")
library(e1071)
tune(svm, Species ~ ., data = iris, gamma = 2^(-1:1), cost = 2^(2:4))


# 
# 
# 
# 
# 
# 
#     Study more about SVM       
# 
# 
# 
# 
# 
# 
# 


##### 06. Class Imbalance

library(caret)
library(mlbench)
data("BreastCancer")
table(BreastCancer$Class)

x <- upSample(subset(BreastCancer, select = -Class), BreastCancer$Class)
x2 <- downSample(subset(BreastCancer, select = -Class), BreastCancer$Class)
table(BreastCancer$Class)
table(x$Class)
table(x2$Class)

# # compare decision tree model of (upSample data & just data)
# library(party)
# str(BreastCancer)
# data <- subset(BreastCancer, select = -Id)
# parts <- createDataPartition(data$Class, p=0.8)
# # just
# data_train <- data[parts$Resample1,]
# data_test <- data[-parts$Resample1,]
# m11 <- rpart(Class ~ ., data = data_train)
# confusionMatrix(data_test$Class, predict(m11, newdata = data_test, type = "class"))
# # upSample
# data_up_train <- upSample(subset(data_train, select = -Class), data_train$Class)
# m12 <- rpart(Class ~ ., data = data_up_train)
# confusionMatrix(data_test$Class, predict(m12, newdata = data_test, type = "class"))


##### 07. Document Classification

install.packages("tm")
library(tm)
data(crude)
summary(crude)

inspect(crude)
inspect(crude[1])
inspect(crude[[1]])

crude[[1]]$content
crude[[1]]$meta

inspect(tm_map(tm_map(crude, tolower), removePunctuation)[1])
inspect(tm_map(tm_map(crude, tolower)[1], removePunctuation))

x <- TermDocumentMatrix(crude)
x
inspect(x[1:10, 1:10])

x <- TermDocumentMatrix(crude, control = list(weighting = weightTfIdf))
inspect(x[1:10, 1:5])

findFreqTerms(TermDocumentMatrix(crude), lowfreq = 10)
head(rownames(x))
head(colnames(x))

findAssocs(TermDocumentMatrix(crude), "oil", 0.7)
