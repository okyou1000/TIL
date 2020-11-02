# groceries <- read.csv("groceries.csv")
# dim(groceries)

install.packages("arules")
library(arules)
groceries <- read.transactions("groceries.csv", sep = ",")
summary(groceries)

inspect(groceries[1:5])
groceries

# 9835*169 = 100만개 중 2% => 2만개 셀(0이 아님), 98만개 셀(0)

# 9835건 거래 중에서 2513건 거래에 whole milk가 있음

# 아이템 1개만 이루어진 거래가 총 2159건 있음
# sizes
# 1
# 2159

itemFrequency(groceries[, 3])
# itemFrequency함수로 거래 비율 확인 : support(지지도)


itemFrequencyPlot(groceries, support = 0.08)
itemFrequencyPlot(groceries, topN = 20)

image(groceries[1:169])
# help -> apriori 검색
# data : 거래데이터 행렬,
# support : 요구되는 최소 규칙 지지도
groceryRules <- apriori(groceries, parameter = list(support = 0.1))
# support가 0.1 이상 이면서 confidence가 0.8 이상인 조건을 만족하는 연관규칙 생성
groceryRules

groceryRules <- apriori(groceries, parameter = list(support = 0.005, confidence = 0.25, minlen = 2))
# support가 0.005 이상 이면서 confidence가 0.25 이상인 조건을 만족하는 연관규칙 생성
groceryRules

????????

# minlen = 2 : 2개 미만의 아이템을 갖는 규칙을 제외
# ex) {} -> {whole milk} 와 같은 규칙은 필요 없음

# confidence(빵->우유) : support(빵, 우유) / support()
#                                   0.05 / 0.1 => 빵

summary(groceryRules)

# 규칙 : 아이템(lhs) -> 아이템(rhs)

# {빵} -> {우유} : 길이가 2
# {빵, 우유} -> {아이스크림} : 3
# {빵} -> {우유, 아이스크림} : 3