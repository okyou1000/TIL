member <- data.frame(spent = c(10, 1, 1, 17, 4, 6, 1, 15, 22, 3, 0, 3, 7, 0, 2),
           time = c(15, 2, 10, 7, 5, 7, 1, 10, 18, 3, 1, 3, 7, 10, 2))
member
# member를 3개의 클러스터로 구성
res <- kmeans(member, 3)
res

install.packages("fpc")
library(fpc)

plotcluster(member, res$cluster, color = TRUE)

res$cluster # 각 데이터가 소속된 클러스터
res$centers # 각 클러스터의 중심 좌표
res$totss # 각 클러스터와 데이터간 거리 제곱의 합의 전체 합 1028
res$withinss # 응집력
res$tot.withinss # 응집력의 총합(작으면 좋다)
res$betweenss # 클러스터간 떨어져 있는 거리(크면 좋다)
res$size # 클러스터에 소속된 데이터 개수
res$iter # 몇 번 반복? 2

# 인사이트 도출 => 클러스터에 대한 그룹별 연산
member$cluster <- res$cluster
member

aggregate(data = member, spent ~ cluster, max)

install.packages("NbClust")
library(NbClust)

class(data)

nb <- NbClust(member[, 1:2], min.nc = 2, max.nc = 5, method = 'kmeans')
nb$Best.partition


nb <- NbClust(iris[, 1:4], min.nc = 2, max.nc = 5, method = 'kmeans')
nb$Best.partition


data(iris)
head(iris)

irisScale <- scale(iris[, -5])

k.max = 10
wss <- rep(NA, k.max)
nClust <- list()

for(i in 1:k.max){
  irisRes = kmeans(irisScale, i)
  wss[i] = irisRes$tot.withinss
  nClust[[i]] = irisRes$size
  
  # res$tot.withinss # 응집력의 총합(작으면 좋다)
  # res$betweenss # 클러스터간 떨어져 있는 거리(크면 좋다)
  
}
wss
nClust

plot(1:k.max, wss, type = 'b')

fitk <- kmeans(irisScale, 3)
str(fitk)

plot(iris, col = fitk$cluster)

table(predicted = fitk$cluster, Actual = iris$Species)
1 - (11+14)/(50+39+36+25)



a <- matrix(rnorm(100), nrow = 5) # 5행 20열, 표준정규분포
# 5건의 데이터, 20차원

# 5개의 데이터를 h-clustering 알고리즘 -> 군집화(1~5)
# hclust(거리행렬, method = 거리구하는방법법)

dist(a) # 유클리디안거리


plot(h <- hclust(dist(a), method = "single")) # 가장 가까운 거리
plot(h <- hclust(dist(a), method = "complete")) # 가장 먼 거리
plot(h <- hclust(dist(a), method = "average")) # 클러스터 평균 간 거리
plot(h <- hclust(dist(a), method = "centroid")) # 클러스터 중심 간 거리


#          1        2        3        4
# 2 8.067289                           
# 3 7.670292 7.811749                  
# 4 6.630950 6.191785 7.161551         
# 5 6.683614 6.536970 6.601344 5.521290


# 강사님 데이터 기준
# 1, 2, 3, 4, 5 거리행렬
# (2, 4), 1, 3, 5 거리행렬
# (1, 2, 4), 3, 5 거리행렬
# (1, 2, 4, 5), 3 거리행렬
# (1, 2, 3, 4, 5) 거리행렬

# knn(k-nearest neighbor) : 거리 기반으로 유사도 측정
# 레이블이 없는 데이터들에 대해 거리 기반으로 클래스를 할당
# 영화, 음악 추천시스템템
# 패턴 인식(얼굴, 글자 인식)

# 장점 : 단순, 효율, 데이터 분포를 가정하지 않음, 빠름
# 단점 : 적절한 k 선택, 결측값 처리(모든 점들이 숫자로 표현)

# knn? 가장 가까운 거리에 있는 k개 데이터를 추출 -> 대상 데이터에 대한 레이블링 수행

wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE) # stringsAsFactors = FALSE (DEFAULT)
# stringsAsFactors = TRUE : 범주(FACTOR)형으로 문자 데이터를 읽겠습니까? TRUE
wbcd
# 문자 컬럼 : 5개, 2개(범주), 3개(문자)

str(wbcd)

wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
      labels = c("Benign", "Malignant"))

str(wbcd)


wbcd$diagnosis
table(wbcd$diagnosis)

round(prop.table(table(wbcd$diagnosis))*100, 1)

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

wbcd <- wbcd[-1] # id 제외

head(wbcd)

# 정규화 : normalize()    0 <= (각 데이터 - 최소값) / (최대값 + 최소값) <= 1
normalize <- function(x){
  return ( (x-min(x)) / (max(x)-min(x)) )
}

normalize(c(1, 2, 3, 4, 5)) # c함수 : 벡터 생성 함수

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize)) # 수치 데이터 정규화
wbcd_n

wbcd_train <- wbcd_n[1:469, ] # 트레이닝 입력 데이터
wbcd_test <- wbcd_n[470:569, ] # 테스트 입력 데이터

wbcd_train_labels <- wbcd[1:469, 1] # 정답
wbcd_test_labels <- wbcd[470:569, 1] # 정답

# 1) 트레이닝 입력 데이터와 정답
# 2) 모델링
# 3) 모델에 테스트 입력 데이터 -> 예측/분류 결과

library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)
wbcd_test_pred

install.packages("gmodels")
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred)



# 표준화 -> 모델링
wbcd_z <- as.data.frame(scale(wbcd[-1])) # 표준화



wbcd_train <- wbcd_z[1:469, ] # 트레이닝 입력 데이터
wbcd_test <- wbcd_z[470:569, ] # 테스트 입력 데이터

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred)



# k = 5, 11, 15, 27

for(i in c(5, 11, 15, 27)){
  print(i)
  wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = i)
  CrossTable(x = wbcd_test_labels, y = wbcd_test_pred)
}
# k = 5 -> 97%
# k = 11 -> 96%
# k = 15 -> 97%
# k = 27 -> 95%




predict.kmeans <- function(x, newdata){
  
}
data(iris)
mydata <- iris
m <- mydata[1:4]
train <- head(m, 100)
xNew <- head(m, 10)
xNew


norm_eucl <- function(train){
  # train연산
  print(train/apply(train, 1, function(x)sum(x^2)^.5))
}
m_norm <- norm_eucl(train)
m_norm


# 연습문제 (임의의 데이터 입력 -> kmeans 클러스터 -> 클러스터를 할당 구현)
# 입력데이터 <-> 클러스터 중심1, 2, 3
# 데이터차원 : 30차원
# 
# 클러스터 : c1, c2, c3
# 
# c1 = (0.5, 0.7, ..., 0.4)
# c2 = (1.5, 0.7, ..., 0.4)
# c3 = (1.5, 0.7, ..., 0.4)
# 
# newData(0.5, 0.2, ..., 0.1)