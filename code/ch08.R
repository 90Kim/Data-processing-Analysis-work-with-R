rm(list=ls())
gc()

#################################################################


##### 2. 단순 선형 회귀 (Simple Linear Regression)

data(cars)
head(cars)
m <- lm(dist ~ speed, cars)
m

### 선형 회귀 결과 추출

coef(m) # 회귀계수
fitted(m) # 적합된 값 - 몇 번째 데이터에 적합된 값인지 
residuals(m) # 잔차 - 몇 번째 데이터의 잔차인지

fitted(m)[1:4] + residuals(m)[1:4]
cars$dist[1:4]

confint(m) # t분포를 사용한 신뢰구간
deviance(m) # 잔차제곱합

predict(m, newdata = data.frame(speed=3))
coef(m)
-17.579095 + 3.932409*3 # predict한 값과 같음을 알 수 있음 

# 평균 신뢰구간 (오차의 평균은 0이기에 무시)
predict(m, newdata = data.frame(speed=3), interval = "confidence") 

# 해당 차량에 대한 예측 신뢰구간? 오차항 때문에 값이 커짐 ---??----
predict(m, newdata = data.frame(speed=3), interval = "prediction") 

summary(m)

### ANOVA

anova(m)
full <- lm(dist ~ speed, data = cars)
reduced <- lm(dist ~ 1, data = cars)
anova(full,reduced)
anova(reduced, full)

plot(m)
plot(cars$speed, cars$dist)
abline(coef(m))

summary(cars$speed)
predict(m, 
        newdata = data.frame(speed = seq(4.0, 25.0, 0.2)),
        interval = "confidence")
speed <- seq(min(cars$speed), max(cars$speed), 0.1)
ys <- predict(m, newdata = data.frame(speed=speed), interval = "confidence")
matplot(speed, ys, type = "n")
matlines(speed, ys, lty = c(1,2,2), col = 1)

##### 3. 중선형 회귀 (Multiple Linear Regression)

data(iris)
head(iris)

m2 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
summary(m2)

m3 <- lm(Sepal.Length ~ ., data = iris)
summary(m3)
model.matrix(m3)[c(1,51,101),]
anova(m3)

with(iris, plot(Sepal.Length, Sepal.Width, cex = 0.7, pch = as.numeric(Species)))
m4 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)

coef(m4)
abline(2.25, 0.8, lty = 1)
abline(2.25 + 1.46, 0.8, col = "blue")
abline(2.25 + 1.95, 0.8, lty = 3)
legend("topright", levels(iris$Species), pch = 1:3, bg = "white")


### 교호작용 !! ###

data(Orange)
head(Orange, 10)
with(Orange,
     plot(Tree, circumference, xlab = "tree", ylab = "circumference"))
with(Orange, interaction.plot(age, Tree, circumference))

Orange[,"fTree"] <- factor(Orange$Tree, ordered = FALSE)
Orange
m5 <- lm(circumference ~ fTree * age, data = Orange)
summary(m5)
anova(m5)
head(model.matrix(m5), 20)

##### 5. 변수 선택

# install.packages("mlbench")
library(mlbench)
data("BostonHousing")
head(BostonHousing)
m_1 <- lm(medv ~ ., data = BostonHousing)
m_2 <- step(m_1, direction = "both")
formula(m_2)


### 모든 경우에 대한 비교

# install.packages("leaps")
library(leaps)
m_3 <- regsubsets(medv ~ ., data = BostonHousing)
summary(m_3)
plot(m_3, scale = "adjr2")
