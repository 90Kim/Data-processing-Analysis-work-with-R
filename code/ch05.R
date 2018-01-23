rm(list=ls())
gc()


##### 02. SQL을 사용한 데이터 처리

install.packages("sqldf")
library(sqldf)
data(iris)
head(iris)
iris$Sepal.Length

sqldf("select distinct Species from iris")
sqldf("select avg(Sepal_Length) from iris where Species='setosa'")

mean(subset(iris, Species=="setosa")$Sepal.Length)
sqldf("select Species, avg(sepal_length) from iris group by species")
sapply(split(iris$Sepal.Length, iris$Species), mean)


##### 03. 분할, 적용, 재조합을 통한 데이터 분석

install.packages("plyr")
library(plyr)

apply(iris[,1:4], 1, function(row){print(row)})
apply(iris, 1, function(row){print(row)})
apply(iris, 1, function(row){row$Sepal.Length>=5.0})
adply(iris, 1, function(row){data.frame(sepal_ge_5=row$Sepal.Length>=5.0)})
ddply(iris, .(Species), function(sub){
  data.frame(sepal.width.mean = mean(sub$Sepal.Width))
})
ddply(iris, .(Species, Sepal.Length > 5.0), function(sub){
  data.frame(sepal.width.mean = mean(sub$Sepal.Width))
})

data("baseball")
head(baseball)

subset(baseball, id=="ansonca01")
ddply(baseball, .(id), function(sub){
  mean(sub$g)
})

head(ddply(baseball, .(id), transform, cyear = year - min(year) + 1))
head(ddply(baseball, .(id), mutate, cyear = year - min(year) + 1, cyear_2 = cyear**2))
head(ddply(baseball, .(id), summarise, minyear=min(year), maxyear=max(year), working_period=maxyear-minyear))
head(ddply(baseball, .(id), subset, g==max(g)))

x <- data.frame(mean=c(1,10,100), sd=rep(1,3))
x
mdply(x, rnorm, n=5)

##### 04. 데이터 구조의 변형과 요약

install.packages("reshape2")
library(reshape2)

data("french_fries")
head(french_fries)
str(french_fries)

m <- melt(french_fries, id.vars = 1:4)
head(m)
View(m)
ddply(m, .(variable), summarise, mean = mean(value, na.rm = TRUE))

r <- dcast(m, time + treatment + subject + rep ~ ...)
r
rownames(r) <- NULL
rownames(french_fries) <- NULL
identical(r, french_fries)

dcast(m, time ~ variable)
dcast(m, time ~ variable, mean, na.rm = TRUE)
dcast(m, time ~ treatment + subject + variable, mean, na.rm = TRUE)

ddply(m, .(time, treatment, variable), function(row){
  return(mean(row$value, na.rm = TRUE))
})

##### 05. 데이터 테이블

install.packages("data.table")
library(data.table)
iris_table <- as.data.table(iris)
x <- data.table(x = c(1,2,3), y = c("a","b","c"))
tables()
DT <- as.data.table(iris)
DT[1,]
DT[DT$Species=="setosa"]
DT[1,Sepal.Length]
DT[1,1]
DT[1, list(Sepal.Length, Species)]
DT[1, c(Sepal.Length, Species)]
DT[, mean(Sepal.Length)]
DT[, mean(Sepal.Length - Sepal.Width)]
DT[, list("Mean_S.L" = mean(Sepal.Length),
          "Mean_S.L-S.W" = mean(Sepal.Length - Sepal.Width))]
DT[, .(Mean_Spec = mean(Sepal.Length)), by = "Species"]

DF <- data.frame(x=runif(26000), y=rep(LETTERS, each=10000))
head(DF)
str(DF)
system.time(x <- DF[DF$y=="C", ])
DT <- as.data.table(DF)
setkey(DT, y)
system.time(x <- DT[J("C"), ])
DT[J("C"), mean(x)]
DT[J("C"), list(x_mean = mean(x), x_std = sd(x))]
head(DT)

# DT[, list(x_mean = mean(x), x_std = sd(x)), keyby = ]

DT1 <- data.table(x = runif(26000), y = rep(LETTERS, each = 10000))
DT2 <- data.table(y = c("A","B","C"), z = c("a","b","c"))
head(DT1)
head(DT2)
setkey(DT1, y)
DT1[DT2,]
setkey(DT2, y)
DT2[DT1,]

library(plyr)

system.time(x <- ldply(1:10000, function(x) {
  data.frame(val = x,
             val2 = 2*x, 
             val3 = 2 / x,
             val4 = 4*x,
             val5 = 4/x)
}))
head(x)

system.time(x <- llply(1:10000, function(x) {
  data.frame(val = x,
             val2 = 2*x, 
             val3 = 2 / x,
             val4 = 4*x,
             val5 = 4/x)
}))
head(x)

x <- lapply(1:10000, function(x) {
  data.frame(val = x,
             val2 = 2*x, 
             val3 = 2 / x,
             val4 = 4*x,
             val5 = 4/x)
})
head(x)

system.time(y <- do.call(rbind, x))
head(y)

##### 06. 더 나은 반복문 

install.packages("foreach")
library(foreach)

foreach(i = 1:5) %do% {
  return(i)
}

foreach(i = 1:5, .combine = c) %do% {
  return(i)
}

(a <- foreach(i = 1:10, .combine = "+") %do% {
  return(i)
})

##### 07. 병렬 처리

install.packages("doParallel")
library(doParallel)
library(plyr)

registerDoParallel(cores = 4)

big_data <- data.frame(
  value = runif(NROW(LETTERS) * 2000000),
  group = rep(LETTERS, 2000000)
)
head(big_data)

dlply(big_data, .(group), function(x){
  mean(x$value)
}, .parallel = TRUE)

foreach(i = 1:800000) %dopar% {
  mean(big_data$value + i)
}

rm(list=ls())
gc()

##### 08. 유닛 테스팅과 디버깅

install.packages("testthat")
library(testthat)

a <- 1:3
b <- 1:3
expect_equal(a,b)
expect_equivalent(a, b)
names(a) <- c('a','b','c')
expect_equal(a,b)
expect_equivalent(a, b)
