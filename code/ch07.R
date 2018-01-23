rm(list=ls())
gc()

##### 01. 난수 생성 및 분포 함수

rnorm(100, 0, 1)
plot(density(rnorm(10000,0,1)))

##### 02. 기초 통계량
summary(1:11)

x <- factor(c("a","b","c","c","c","d","d"))
table(x)
which.max(table(x))

##### 03. 표본 추출

sample(1:10, 5)
sample(1:10, 5, replace = TRUE)

# 가중치 고려 표본 추출 : prob
sample(1:10, 5, replace = TRUE, prob = 1:10)

# 층화 임의 추출
install.packages("sampling")
library(sampling)
x <- strata(c("Species"), size=c(3,1,1), method = "srswor", data = iris)
x
getdata(iris, x)

# 계통 추출
install.packages("doBy")
library(doBy)
(x <- data.frame(x=1:10))
sampleBy(~1, frac = 0.4, data = x, systematic = TRUE)

##### 04. 분할표

table(c("a","b","b","c","c","c","d"))

d <- data.frame(x = c("1","2","2","1"),
                y = c("A","B","A","B"),
                num = c(3,5,8,7))
d
(xt <- xtabs(num ~ x+y, data = d))

d2 <- data.frame(x=c("a","a","a","b","b"))
(xtabs(~x, data = d2))

margin.table(xt, 1)
margin.table(xt, 2)
margin.table(xt)
prop.table(xt, 1)
prop.table(xt, 2)
prop.table(xt)

x <- seq(1,10,0.1)
plot(x, dchisq(x, 6), type = "l")

library(MASS)
data("survey")
str(survey)
head(survey)
xtabs(~ Sex + Exer, data = survey)
chisq.test(xtabs(~ Sex + Exer, data = survey))
xtabs(~ W.Hnd + Clap, data = survey)
chisq.test(xtabs(~ W.Hnd + Clap, data = survey))
fisher.test(xtabs(~ W.Hnd + Clap, data = survey))
Performance <- matrix(c(794, 86, 150, 570), nrow = 2,
                      dimnames = list("1st Survey" = c("Approve", "Disapprove"),
                                       "2nd Survey" = c("Approve", "Disapprove")))
Performance
mcnemar.test(Performance)

##### 05. 적합도 검정

table(survey$W.Hnd)
chisq.test(table(survey$W.Hnd), p = c(0.3, 0.7))
shapiro.test(rnorm(1000))

x <- rnorm(1000, mean = 10, sd = 1)
qqnorm(x)
qqline(x)

x <- runif(1000)
qqnorm(x)
qqline(x)

qqplot(runif(1000, min = 1, max = 10), 1:10)

##### 06. 상관분석

cor(iris$Sepal.Length, iris$Sepal.Width)
cor(iris[,1:4])
symnum(cor(iris[,1:4]))

install.packages("corrgram")
library(corrgram)
corrgram(iris, upper.panel = panel.conf)

x <- c(3,4,5,3,2,1,7,5)
rank(sort(x))
(m <- matrix(c(1:10, (1:10)^2), ncol = 2))
cor(m, method = "spearman")
cor(m, method = "pearson")

cor(c(1,2,3,4,5),c(1,0,3,4,5), method = "kendall")

cor.test(c(1,2,3,4,5), c(1,0,3,4,5), method = "pearson")
cor.test(c(1,2,3,4,5), c(1,0,3,4,5), method = "spearman")
cor.test(c(1,2,3,4,5), c(1,0,3,4,5), method = "kendall")

##### 07. 추정 및 검정

x <- rnorm(30)
t.test(x)
x <- rnorm(30, mean = 10)
t.test(x, mu = 10)

sleep
sleep2 <- sleep[,-3]
tapply(sleep2$extra, sleep2$group, mean)
library(doBy)
summaryBy(extra ~ group, sleep2)
var.test(extra ~ group, sleep2)
t.test(extra ~ group, data = sleep2, paired = FALSE, var.equal = TRUE)
with(sleep, t.test(extra[group==1], extra[group==2], paired = TRUE))
with(iris, var.test(Sepal.Width, Sepal.Length))
prop.test(42, 100)
prop.test(c(45,55), c(100,90))
