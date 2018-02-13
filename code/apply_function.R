###########################################################################
# function of apply
###########################################################################

# apply

d <- matrix(1:9, ncol = 3)
d
apply(d, 1, sum)
apply(d, 2, sum)

data(iris)
head(iris)

apply(iris[,1:4], 2, sum)
colSums(iris[,1:4])

apply(iris[,1:4], 2, mean)
colMeans(iris[,1:4])

# lapply

result <- lapply(1:3, function(x){x^2})
result
unlist(result)

lst <- list(a=1:3, b=c(5,10,15))
lst
lapply(lst, sum)
lapply(lst, mean)
lapply(lst, function(x){x^2})
lapply(lst, var)

lapply(iris[,1:4], mean)
colMeans(iris[,1:4])

# --- data.frame of colMeans(iris[,1:4])
as.data.frame(t(as.matrix(unlist(lapply(iris[,1:4], mean)))))
as.data.frame(lapply(iris[,1:4], mean))

# --- do.call : 속도가 느림, rbindlist() 함수
x <- list(data.frame(name = "foo", value = 1),
          data.frame(name = "bar", value = 2))
x
unlist(x)
as.data.frame(x)
do.call(rbind, x)
do.call(cbind, x)

x1 <- x <- list(data.frame(name = "foo", value = 1),
                data.frame(name = "bar", value = 2),
                data.frame(name = "goo", value = 5))
x1
do.call(rbind,x1)
do.call(cbind,x1)

# sapply

lapply(iris[,1:4], mean)
sapply(iris[,1:4], mean)
class(sapply(iris[,1:4], mean))
sapply(iris[,1:4], mean)[1]

x <- sapply(iris[,1:4], mean)
as.data.frame(x)
t(as.data.frame(x))
class(t(x))

class(sapply(iris, class))

a <- sapply(iris[,1:4], function(x){x>3})
class(a)

# tapply

tapply(iris$Sepal.Length, iris$Species, mean)

m <- matrix(1:8, ncol = 2,
            dimnames = list(c("spring","summer","fall","winter"),
                            c("male","female")))
m
tapply(m, list(c(1,1,2,2,1,1,2,2),
               c(1,1,1,1,2,2,2,2)), sum)
tapply(m, list(c(1,1,2,2,1,1,2,2),
               c(1,1,1,1,2,2,2,2)), function(x){list(x)})


# mapply

rnorm(5, 0, 1)
mapply(rnorm, c(1,2,3), c(0,0,0), c(1,5,10))
mapply(rnorm, c(1,5,10), c(0,0,0), c(1,5,10))

rpois(5, 1)
mapply(rpois, c(3,5,8,10), c(0,10,100,1000))
