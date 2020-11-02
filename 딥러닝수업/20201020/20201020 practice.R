insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)

# 회귀모델은 작성전에 정규성 확인
# 선형회귀는 종속변수가 정규분포를 따르는게 더 잘 fitting

# 요약통계
summary(insurance$expenses)
# 중앙값보다 평균값이 크다 => 의료비 분포가 오른쪽으로 길게 분포
# 대부분이 0~15000 달러 사이에 위치
hist(insurance$expenses)

# 회귀모델은 모든 특징이 수치 데이터야만 함
# 3개의 팩터 타입(sex, smoker, region)

table(insurance$sex)
table(insurance$smoker)
table(insurance$region)

# 회귀 모델 만들기 전에 독립변수가 종속변수와 어떤 관계? 상관분석
cor(insurance[c("age", "bmi", "children", "expenses")])

pairs(insurance[c("age", "bmi", "children", "expenses")])

install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

# 종속변수~독립변수
# lm(expenses~독립변수+독립변수+..., data = insurance)

ins_model <- lm(expenses~age+children+bmi+sex+smoker+region, data = insurance)
# ins_model <- lm(expenses~., data = insurance)
# .은 expenses를 제외한 모든 컬럼

ins_model
str(insurance)
temp <- data.frame(age=25, children=2, bmi=30, sex='female', smoker='yes', region = 'northeast')

temp

predict(ins_model, newdata = temp)


mydata <- read.csv("ins_model_test.csv", stringsAsFactors = TRUE)
mydata
predict(ins_model, newdata = mydata)


# 예측값 변수 <- (age+children+bmi+sex+smoker+region 데이터들)
# predict(ins_model, newdata = 예측값 변수)

ins_model
# 독립변수들이 모두 0일때 절편은 expenses의 예측값 : -11941.6

expenses = 256.8*age+475.7*children+...-959.3*regionsouhwest-11941.6


# 범주별로 더미변수화 : 명목형 특징값 -> 수치

# sex특징이 female, male 두개의 범주
# sexmale = 1 or 0
# smokeryes = 1 or 0

summary(ins_model)

# Residuals(잔차 : 예측에 대한 오차(실제값 - 예측값)의 요약)
# 오차(실제값-예측값) 연산에 대한 최소값 : -11302$
# 오차 최대값 : 29981$(실제값보다 낮게 예측했다는 의미)
# 오차의 50%는 q1과 q3차이, -2850.9 ~ 1383.9 달러 사이에 있다


coefficients
# Pr(>|t|) : 추정된 계수가 실제 0일 확률 추정치.
# p값이 작은 경우 실제계수가 아닐 가능성이 높다는 것을 의미(특징변수가
# 종속변수와 관계가 없을 가능성이 아주 낮다는 의미)

# *(별)은 유의 수준

# Multiple R-squared(다중 r 제곱값, 결정계수) : 모델이 종속변수 값을 얼마만큼
# 잘 설명하는지를 측정한 값. 1에 가까울수록 모델이 완벽하게 종속변수를 설명
# 0.7509 => 모델이 종속변수 변화량의 약 75%를 설명하고 있다

# 선형회귀 -> 연속형 값 예측
# 로지스틱회귀 -> 선형회귀 -> 연속형 갑 예측 -> 특수한 함수 ->
# 분류 결과(0 or 1, yes or no, 0~9, ...)
# 연속형 값 예측(선형회귀), 분류결과(로지스틱회귀)

# 신경망(로지스틱 회귀(너무 단순, 간단한 모델 적절) 방식의 한계점을 개선)

# 주성분분석(pca, 차원 축소)

# 전파? 신호(가중치 변수값)가 전달(방향 : 좌->우) => 순전파(학습), 우->좌 => 역전파(오류 조정)
# 딥러닝은 순전파/역전파가 번갈아가며 수행되는 학습 방법

# 손실함수(loss(cost) function) : 실제값-예상값 : 오차를 계산하는 함수
#
손실함수의 결과를 바탕으로 오차를 최소화하는 과정
# (GD알고리즘 : COST의 최소값에 해당되는 위치(w, b)를 찾는 것)