# Q1.

library(foreign)
library(readxl)
library(ggplot2)
library(dplyr)

raw_welfare <- read.spss(file = "Data/koweps_hpc10_2015_beta1.sav", to.data.frame = T)
welfare <- raw_welfare

welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7
)

welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
welfare$income <- ifelse(welfare$income %in% c(0, 5000), NA, welfare$income)
welfare$birth <- ifelse(welfare$birth == 999, NA, welfare$birth)
welfare$age <- 2015 - welfare$birth + 1
welfare <- welfare %>% 
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age <= 59, "middle", "old")))

list_region <- data.frame(code_region = c(1 : 7),
                          region = c("서울", "수도권(인천/경기",
                                     "부산/경남/울산", "대구/경북",
                                     "대전/충남", "강원/충북",
                                     "광주/전남/전북/제주도"))
list_region

table(welfare$code_region)

welfare <- left_join(welfare, list_region, id = "code_region")

welfare %>% 
  select(code_region, region) %>% 
  head

str(welfare)

# 지역별 연령대 비율 조사
welfare %>% 
  group_by(region, ageg) %>% 
  summarise(cnt = n()) %>% 
  mutate(tot_group = sum(cnt)) %>% 
  mutate(pct = round(cnt/tot_group*100, 2))

# 지역별 연령대 비율 조사(count함수 이용)
region_ageg <- welfare %>% 
  count(region, ageg) %>% 
  group_by(region) %>% 
  mutate(pct = round(n/sum(n)*100, 2))

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip()

# 노년층 비율 내림차순 정렬
list_order_old <- region_ageg %>% 
  filter(ageg == "old") %>% 
  arrange(pct)

order <- list_order_old$region
order

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order) # limits = c(서울, 부산, 수도권, ...)


# Q2.

mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA

table(mpg$drv)

table(is.na(mpg$drv))
table(is.na(mpg$hwy))

mpg %>% 
  filter(!is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise((mean_hwy = mean(hwy)))


# 오후 연습문제, 시각화연습
str(iris)
str(iris3)
iris3
# 리스트 다양한 타입의 자료들이 저장될 수 있는 자료구조



# Q3.

mpg <- as.data.frame(ggplot2::mpg) # mpg 데이터 불러오기 
mpg[c(10, 14, 58, 93), "drv"] <- "k" # drv 이상치 할당 
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42) # cty 이상치 할당

table(mpg$drv)
mpg$drv <- ifelse(mpg$drv %in% c("4", "f", "r"), mpg$drv, NA)

table(mpg$drv)

boxplot(mpg$cty)

boxplot(mpg$cty)$stats

boxplot(mpg$cty)$stats[1] # 아랫쪽 극단치 경계값

boxplot(mpg$cty)$stats[5] # 윗쪽 극단치 경계값

# 극단치를 NA로 대체
mpg$cty <- ifelse(mpg$cty < boxplot(mpg$cty)$stats[1] | mpg$cty > boxplot(mpg$cty)$stats[5], NA, mpg$cty)

boxplot(mpg$cty)

mpg %>% 
  filter(!is.na(drv) & !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_cty = mean(cty))


#===============================================================================

y <- rnorm(100) # 평균 : 0, 표준편차 : 1, 정규분포 난수
hist(y)

x <- matrix(rnorm(100), nrow = 5) # 난수 100개 생성 -> 5행 * 20열
dist(x)
# 20차원에 해당되는 데이터에서 1번째, 2번째 데이터간의 유클리디안 거리 = 7.023595

#       1        2        3        4
# 2 7.023595                           
# 3 5.830326 7.010568                  
# 4 7.456684 6.899279 7.220625         
# 5 7.951390 7.319124 6.676590 5.748246

dist(x, method = "manhattan")


data(iris) # data함수 : 데이터를 로드
head(iris)

iris[,-5]


# elbow


# iris는 4차원(features 개수가 4개) 데이터 => 3개 클러스터
# 
# => 3개 클러스터에 대한 중심점의 좌표 확인(centers)
# ex)
# 1 cluster의 cp : (5.7, 2.5, 1.3, 0.15)
# 2 cluster의 cp : (4.7, 1.5, 1.3, 0.15)
# 3 cluster의 cp : (3.7, 2.7, 1.3, 0.15)
# 3행 4열

# ex) 3만명 sns데이터, 5개 클러스터, features 30개
# 5개 클러스터에 대한 중심점의 shape? 5행 30열 => 1행 값들의 의미? 1번째 클러스터의 중심점 좌표
# 2행 값들의 의미? 2번째 클러스터의 중심점 좌표...



kmeans.iris <- kmeans(iris[, -5], 3)

kmeans.iris$centers

kmeans.iris$cluster

iris[, 5]
kmeans.iris$cluster


table(iris[, 5], kmeans.iris$cluster)


kmeans.iris <- kmeans(iris[, -5], 3, nstart = 100)

table(iris[, 5], kmeans.iris$cluster)

kmeans.iris <- kmeans(iris[, -5], 3, nstart = 50)

table(iris[, 5], kmeans.iris$cluster)

kmeans.iris <- kmeans(iris[, -5], 3)

kmeans.iris

kmeans10.iris <- kmeans(iris[, -5], 3, nstart = 10)
round(sum(kmeans10.iris$withinss), 2) # 78.85

data(iris)

set.seed(123)

kmeans.iris <- kmeans(iris[, -5], 3)
round(sum(kmeans10.iris$withinss), 2) # 78.85

kmeans10.iris <- kmeans(iris[, -5], 3, nstart = 10)
round(sum(kmeans10.iris$withinss), 2) # 78.85


iris_plot <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point(shape = 19, size = 4) +
  ggtitle("iris data")

iris_plot2 <- iris_plot +
  annotate("text", x = 1.5, y = 0.7, label = "Setosa", size = 5) +
  annotate("text", x = 3.5, y = 1.5, label = "Verisicolor", size = 5) +
  annotate("text", x = 6, y = 2.7, label = "Virginica", size = 5)


iris_k_means <- kmeans(iris[, c("Petal.Length", "Petal.Width")], 3)  

iris_k_means


iris_k_means$cluster

iris_k_means$totss

iris_k_means$withinss

iris_k_means$betweenss


prop.table(iris_k_means$size)


iris_k_means_centers <- iris_k_means$centers
iris_k_means_centers

iris_plot2 +
  annotate("point", x = 5.59, y = 2.03, size = 5, color = "black") +
  annotate("point", x = 1.46, y = 0.24, size = 5, color = "black") +
  annotate("point", x = 4.26, y = 1.34, size = 5, color = "black")

# 고려사항 : 표준화 작업
# 범주별 데이터 개수가 비슷한 경우 -> 클러스터링 잘 됨
# 범주별 데이터 개수가 차이가 심한 경우 -> 클러스터링 잘 되지 않음 => 데이터 증식
# 데이터 제거 => 범주별 데이터 개수를 비슷하게 해줌
# 범주별 밀도가 다른 경우에 클러스터링이 잘 되지 않을 수 있음


z_iris <- scale(iris[, -5])



# iris 컬럼 2개, 3개, 4개 => 클러스터링 수행




# 논문(DB알고리즘)
# https://www.aaai.org/Papers/KDD/1996/KDD96-037.pdf