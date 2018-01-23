getwd()
setwd("c:/Users/GooYoung/Documents/R/Data processing & Analysis work with R/")
data("iris")
head(iris)
str(iris)

data("iris3")
iris3
head(iris3)
data("mtcars")
head(mtcars)

(x <- read.csv("a.csv"))
str(x)
x$name <- as.character(x$name)
str(x)

(x <- read.csv(file = "c.csv"))
str(x)
x <- read.csv(file = "c.csv", na.strings = c("NIL"))
str(x)

write.csv(x, file = "d.csv")

rm(list=ls())
gc()
x <- 1:5
y <- 6:10
save(x, y, file = "xy.RData")

a <- 1:5
b <- 6:10
c <- 11:15
save(list=ls(), file = "abc.RData")

rm(list=ls())
ls()
load(file = "abc.RData")
ls()

d <- matrix(1:9, ncol = 3)
d
apply(d, 1, sum)
apply(d, 2, sum)
data("iris")
head(iris)
apply(iris[,1:4], 2, sum)
colSums(iris[,1:4])

(result <- lapply(1:3, function(x){x*2}))
result[2]
unlist(result)
x <- list(a=1:3, b=4:6)
x
lapply(x, mean)
lapply(iris[,1:4], mean)
(d <- as.data.frame(matrix(unlist(lapply(iris[,1:4], mean)), ncol=4, byrow=TRUE)))
names(d) <- names(iris[,1:4])
d
data.frame(do.call(cbind, lapply(iris[,1:4], mean)))

x <- list(data.frame(name="foo", value=1),
          data.frame(name="bar", value=2))
x
unlist(x)
do.call(rbind, x)
x <- sapply(iris[,1:4], mean)
t(as.data.frame(x))
as.data.frame(t(x))
y <- sapply(iris[,1:4], function(x){x>3})
head(y)

tapply(1:10, 1:10 %% 2 == 1, sum)
tapply(iris$Sepal.Length, iris$Species, mean)

m <- matrix(1:8, ncol = 2,
            dimnames = list(c("spring","summer","fall","winter"),
                            c("male","female")))
m
tapply(m, list(c(1,1,1,2,1,1,2,2),
               c(1,1,1,1,1,2,2,2)), sum)

mapply(rnorm,
       c(1,2,3),
       c(0,10,100),
       c(1,1,1))
mapply(mean, iris[,1:4])

install.packages("doBy")
library(doBy)
summaryBy(Sepal.Width + Sepal.Length ~ Species, iris)

tapply(iris$Sepal.Length, iris$Species, mean)
tapply(iris$Sepal.Width, iris$Species, mean)
order(iris$Sepal.Width)
iris[order(iris$Sepal.Width),]
iris[order(iris$Sepal.Length,iris$Sepal.Width),]
orderBy(~ Sepal.Width, iris)
sample(1:10, 5)
sample(1:10, 5, replace = TRUE)

sample(c("김구영", "문종호", "정윤섭", "강재수", "전지혜", "김혜진"), 1)

sample(10,10)
sampleBy(~ Species, frac = 0.1, data = iris)
split(iris, iris$Species)
lapply(split(iris$Sepal.Length, iris$Species), mean)
class(subset(iris, Species == "setosa"))
search()

rm(list=ls())
gc()
data("iris")
head(iris)
attach(iris)
Sepal.Width[1] = -1
Sepal.Width
head(iris)
detach(iris)

a <- aggregate(Sepal.Width ~ Species, iris, mean)
b <- tapply(iris$Sepal.Width, iris$Species, mean)

x <- read.csv(file = "data/data.csv")
x
x_stacked <- stack(x)
x_stacked
summaryBy(values ~ ind, x_stacked)
unstack(x_stacked, values ~ ind)
