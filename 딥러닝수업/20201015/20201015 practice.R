credit <- read.csv("credit.csv")
str(credit)

table(credit$checking_balance) # ���� �ܰ�
table(credit$savings_balance) # ���� ���� �ܰ�

summary(credit$months_loan_duration)
# ����� 4���� ~ 72����
summary(credit$amount)
# ����� : 250 ~ 18424 �߾Ӱ� 2320 ��� 3271

table(credit$default) # 30% ä��������

# �Ʒ� : 90%, �׽�Ʈ : 10%

# ���� ���ø�
set.seed(123)
sample(10, 5)

trainsample <- sample(1000, 900)
str(trainsample)

str(credit)
creditTrain <- credit[trainsample,] # 900��
creditTest <- credit[-trainsample,] # 100��

table(creditTrain$default) # 628(no), 272(yes)
table(creditTest$default) # 72(no), 28(yes)


# �ǻ����Ʈ��
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
