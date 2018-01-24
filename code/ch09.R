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

install.packages("caret", dependencies = TRUE)
library(caret)
featurePlot(iris[,1:4], iris$Species, "ellipse")

cbind(as.data.frame(scale(iris[1:4])), iris$Species)
