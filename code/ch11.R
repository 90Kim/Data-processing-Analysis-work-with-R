titanic <- read.csv("data/titanic3.csv")
head(titanic)
str(titanic)
titanic <- titanic[, !names(titanic) %in% c("home.dest","boat","body")]
str(titanic)
names(titanic)

titanic$pclass <- as.factor(titanic$pclass)
titanic$name <- as.character(titanic$name)
titanic$ticket <- as.character(titanic$ticket)
titanic$cabin <- as.character(titanic$cabin)
titanic$survived <- factor(titanic$survived, levels = c(0,1), labels = c("dead","survived"))
str(titanic)

levels(titanic$embarked)
table(titanic$embarked)
levels(titanic$embarked)[1] <- NA
table(titanic$embarked, useNA = "always")

titanic$cabin <- ifelse(titanic$cabin == "", NA, titanic$cabin)

library(caret)
set.seed(137)
test_idx <- createDataPartition(titanic$survived, p = 0.1)$Resample1
titanic_test <- titanic[test_idx,]
titanic_train <- titanic[-test_idx,]
NROW(titanic_test)
NROW(titanic_train)
prop.table(table(titanic_test$survived))
prop.table(table(titanic_train$survived))

save(titanic, titanic_test, titanic_train, file = "Rdata/titanic.RData")

createFolds(titanic_train$survived, k=10)

create_ten_fold_cv <- function(){
  set.seed(137)
  lapply(createFolds(titanic_train$survived, k=10), function(idx){
    return(list(train = titanic_train[-idx,],
                validation = titanic_train[idx,]))
  })
}

x <- create_ten_fold_cv()
x[1]
head(x$Fold10$train)

library(Hmisc)
data <- create_ten_fold_cv()[[1]]$train
summary(survived ~ pclass + sex + age + sibsp + parch + fare + embarked,
        data = data, method = "reverse")
data.complete <- data[complete.cases(data),]

featurePlot(
  data.complete[,sapply(names(data.complete), function(n) {
    is.numeric(data.complete[,n])})],
  data.complete[,c("survived")],
  "ellipse"
)

mosaicplot(survived ~ pclass + sex, data = data, color = TRUE, main = "pclass & sex")
xtabs( ~ pclass + sex, data = data)
xtabs(survived == "survived" ~ pclass + sex, data = data)
xtabs(survived == "survived" ~ pclass + sex, data = data) / xtabs( ~ pclass + sex, data = data)

library(rpart)

m <- rpart(
  survived ~ pclass + sex + age + sibsp + parch + fare + embarked, data = titanic_train
)

p <- predict(m, newdata = titanic_train, type = "class")
head(p)
head(titanic_train)

library(rpart)
library(foreach)
folds <- create_ten_fold_cv()

rpart_result <- foreach(f=folds) %do% {
  model_rpart <- rpart(
    survived ~ pclass + age + sex + sibsp + parch + fare + embarked, data = f$train
  )
  predicted <- predict(model_rpart, newdata = f$validation, type = "class")
  return(list(actual = f$validation$survived, predicted = predicted))
}

evaluation <- function(lst){
  accuracy <- sapply(lst, function(one_result){
    return(sum(one_result$predicted == one_result$actual) / NROW(one_result$actual))
  })
  print(sprintf("MEAN +/- SD: $.3f +/- #.3f", mean(accuracy), sd(accuracy)))
  return(accuracy)
}

rpart_accuracy <- evaluation(rpart_result)


















