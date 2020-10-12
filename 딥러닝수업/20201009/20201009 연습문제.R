# 1. 연습문제
# gender를 다른 값으로 대체, na 값을 갖는 학생과 가장 유사한(거리) 학생 10명 검색 ->
# 성별 판단 = 클러스터링

library(dplyr)
library(proxy)

teens <- read.csv("snsdata.csv")

teens$gender <- ifelse(teens$gender == "F", 1, 0)

teens_g_NA <- teens[5,]     # 임의로 한명 선택

teens_g_notNA <- teens %>%  # gender가 NA가 아닌 학생들
  filter(!is.na(gender))

doc <- rbind(teens_g_NA, teens_g_notNA[1:100,])

dist(doc, method = "cosine")


# 2. 모델 파라미터 조정(n-start 등)

teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)

ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE))

teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

interests <- teens[5:40]

interests_z <- as.data.frame(lapply(interests, scale)) # 표준화(mu = 0, sigma = 1)

set.seed(12345)

teen_clusters <- kmeans(interests_z, 5, nstart = 5)

teen_clusters
teen_clusters$cluster
teen_clusters$size      # 601  1038 21581  4064  2716
teen_clusters$centers
teen_clusters$totss     # 1079964
teen_clusters$withinss  # 36141.05 184943.37 264427.49 298417.13 162194.09
teen_clusters$betweenss # 133840.9


# 3. 각 클러스터 해석(using aggregate())
# ??? ~ cluster => 해석 결과를 문장으로 작성
aggregate(data = teens, age ~ gradyear, FUN = function(x) mean(x, na.rm = TRUE))


# 4. wine 데이터 클러스터링


