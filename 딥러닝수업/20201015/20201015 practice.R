credit <- read.csv("credit.csv")
str(credit)

table(credit$checking_balance) # °èÁÂ ÀÜ°í
table(credit$savings_balance) # ÀúÃà °èÁÂ ÀÜ°í

summary(credit$months_loan_duration)
# ´ëÃâ±İ 4°³¿ù ~ 72°³¿ù
summary(credit$amount)
# ´ëÃâ±İ : 250 ~ 18424 Áß¾Ó°ª 2320 Æò±Õ 3271

table(credit$default) # 30% Ã¤¹«ºÒÀÌÇà

# ÈÆ·Ã : 90%, Å×½ºÆ® : 10%

# ·£´ı »ùÇÃ¸µ
set.seed(123)
sample(10, 5)

trainsample <- sample(1000, 900)
str(trainsample)

str(credit)
creditTrain <- credit[trainsample,] # 900°³
creditTest <- credit[-trainsample,] # 100°³

table(creditTrain$default) # 628(no), 272(yes)
table(creditTest$default) # 72(no), 28(yes)


# ÀÇ»ç°áÁ¤Æ®¸®
install.packages("C50")
library(C50)

str(creditTrain)
creditTrain$default <- factor(creditTrain$default)

str(creditTrain)

model <- C5.0(creditTrain[-17], creditTrain$default, trials = 50)

model

creditPred <- predict(model, creditTest)

library(gmodels)

CrossTable(creditTest$default, creditPred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c("act defaul", "pred default"))
