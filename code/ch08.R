rm(list=ls())
gc()

#################################################################


##### 2. 단순 선형 회귀
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
