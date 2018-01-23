rm(list=ls())
gc()

##### 01. 산점도

install.packages("mlbench")
library(mlbench)
data("Ozone")
plot(Ozone$V8, Ozone$V9)

##### 02. 그래프 옵션

plot(Ozone$V8, Ozone$V9, xlab = "Sandburg Temperature", ylab = "EL Monte Temperature",
     main = "Ozone", pch = 20, col = "blue", col.axis = "red",
     xlim = c(20,100), ylim = c(20,90))

?par


data(cars)
head(cars)
plot(cars, type = "o")
plot(tapply(cars$dist, cars$speed, mean), type = "o", xlab = "speed", ylab = "dist",
     lty = "twodash")


opar <- par(mfrow = c(1,2))

plot(Ozone$V8, Ozone$V9, xlab = "Sandburg Temperature", ylab = "EL Monte Temperature",
     main = "Ozone", pch = 20, col = "blue", col.axis = "red",
     xlim = c(20,100), ylim = c(20,90))

plot(Ozone$V8, Ozone$V9, xlab = "Sandburg Temperature", ylab = "EL Monte Temperature",
     main = "Ozone2", pch = 20, col = "blue", col.axis = "red",
     xlim = c(20,100), ylim = c(20,90))
par(opar)


head(Ozone[,c("V6","V7")])

plot(Ozone$V6, Ozone$V7, xlab = "Windspeed", ylab = "Humidity",
     main = "Ozone", pch = 20, cex = 0.5)

plot(jitter(Ozone$V6), jitter(Ozone$V7), xlab = "Windspeed", ylab = "Humidity",
     main = "Ozone", pch = 20, cex = 0.5)


data("iris")
plot(iris$Sepal.Width, iris$Sepal.Length, cex=0.5, pch=20, 
     xlab = "Width", ylab = "Length", main = "iris")
points(iris$Petal.Width, iris$Petal.Length, cex=0.5, pch="+", col="red")

with(iris, {
  plot(NULL, xlim = c(0,5), ylim = c(0,10), xlab = "width", ylab = "length",
       main = "iris", type = "n")
  points(Sepal.Width, Sepal.Length, cex = 0.5, pch = 20)
  points(Petal.Width, Petal.Length, cex = 0.5, pch = "+", col = "blue")
})

x <- seq(0, 2*pi, 0.1)
y <- sin(x)
plot(x, y, cex = 0.5, col = "red")
lines(x, y)

data(cars)
head(cars)
str(cars)
plot(cars)
lines(lowess(cars))
lines(loess(cars), col = "red")
lines(smooth.spline(cars), col = "blue")

plot(cars, xlim = c(0,25))
abline(a = -5, b = 3.5, col = "red")
abline(h = mean(cars$dist), lty=2, col = "blue")
abline(v = mean(cars$speed), lty=2, col = "blue")

curve(sin, 0, 2*pi)

m <- lm(dist ~ speed, data = cars)
m
plot(cars)
abline(m)

p <- predict.lm(m, interval = "confidence")
x <- c(cars$speed, tail(cars$speed, 1), rev(cars$speed), cars$speed[1])
y <- c(p[,"lwr"], tail(p[,"upr"], 1), rev(p[,"upr"]), p[,"lwr"][1])
plot(cars)
abline(m)
polygon(x, y, col = rgb(0.7, 0.7, 0.7, 0.5))
text(cars$speed, cars$dist, pos = 4)
identify(cars$speed, cars$dist)

data(iris)
with(iris, {
  plot(NULL, xlim = c(0,5), ylim = c(0,10), xlab = "width", ylab = "length",
       main = "iris", type = "n")
  points(Sepal.Width, Sepal.Length, cex = 0.5, pch = 20)
  points(Petal.Width, Petal.Length, cex = 0.5, pch = "+", col = "red")
})
legend("topright", legend = c("Sepal","Petal"), 
       pch = c(20, 43), col = c("black","red"), bg = "grey")

x <- seq(-2*pi, 2*pi, 0.01)
y <- matrix(c(cos(x),sin(x)), ncol = 2)
matplot(x, y, lty = c("solid","dashed"), cex = 0.2, type = "l")
abline(h=0, v=0)

boxstats <- boxplot(iris$Sepal.Width, horizontal = TRUE)
text(boxstats$out, rep(1, NROW(boxstats$out)), labels = boxstats$out, pos = c(1,1,3,1))

sv <- subset(iris, Species=="setosa" | Species=="versicolor")
str(sv)
sv$Species <- factor(sv$Species)
boxplot(Sepal.Width ~ Species, data = sv, notch = TRUE)
hist(iris$Sepal.Length, freq = FALSE)
lines(density(iris$Sepal.Length))
plot(density(iris$Sepal.Width))
rug(jitter(iris$Sepal.Width))

barplot(tapply(iris$Sepal.Width, iris$Species, mean))

cut(1:10, breaks = c(0,5,10))
cut(1:10, breaks = c(1,5,10))
cut(1:10, breaks = 3)
cut(iris$Sepal.Width, breaks = 10)

table(rep(c("a","b","c"), 1:3))
table(cut(iris$Sepal.Width, breaks = 10))

pie(table(cut(iris$Sepal.Width, breaks = 10)))

data("Titanic")
str(Titanic)
mosaicplot(Titanic, color = TRUE)
mosaicplot(~ Class + Survived, data = Titanic, color = TRUE, cex = 1.2)
mosaicplot(~ Class + Age, data = Titanic, color = TRUE, cex = 1.2)

pairs(~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length,
      data = iris, pch = c(1,2,3)[iris$Species])

outer(1:5, 1:3, "+")

install.packages("mvtnorm")
library(mvtnorm)
dmvnorm(c(0,0), rep(0,2), diag(2))
x <- seq(-3, 3, 0.1)
y <- x
f <- function(x,y) {dmvnorm(cbind(x,y))}
persp(x, y, outer(x, y, f), theta = 30, phi = 30)
contour(x, y, outer(x, y, f))
