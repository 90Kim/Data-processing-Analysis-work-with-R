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