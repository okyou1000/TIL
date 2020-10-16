sms_raw <- read.csv("sms_spam_ansi.txt", stringsAsFactors = FALSE)
sms_testdata <- read.csv("ÇÜ½ºÆÔÅ×½ºÆ®.txt", stringsAsFactors = FALSE, header = FALSE)
names(sms_testdata) <- c("type", "text")
sms_raw$type <- factor(sms_raw$type)
sms_testdata$type <- factor(sms_testdata$type)

library(tm)

sms_corpus <- VCorpus(VectorSource(sms_raw$text))

sms_corpus_clean <- tm_map(sms_corpus, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean, content_transformer(tolower))
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

sms_corpus2 <- VCorpus(VectorSource(sms_testdata$text))

sms_corpus2_clean <- tm_map(sms_corpus2, removeNumbers)
sms_corpus2_clean <- tm_map(sms_corpus2_clean, content_transformer(tolower))
sms_corpus2_clean <- tm_map(sms_corpus2_clean, removePunctuation)
sms_corpus2_clean <- tm_map(sms_corpus2_clean, removeWords, stopwords())
sms_corpus2_clean <- tm_map(sms_corpus2_clean, stemDocument)
sms_corpus2_clean <- tm_map(sms_corpus2_clean, stripWhitespace)

sms_dtm2 <- DocumentTermMatrix(sms_corpus2_clean)

sms_train_labels <- sms_raw$type
sms_test_labels <- sms_testdata$type

sms_dtm_train <- sms_dtm
sms_dtm_test <- sms_dtm2

library(e1071)

sms_dtm_freq_train <- removeSparseTerms(sms_dtm_train, 0.999)

sms_freq_words <- findFreqTerms(sms_dtm_train, 5)

sms_dtm_freq_train <- sms_dtm_train[ ,sms_freq_words]

convertCounts <- function(x){
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convertCounts)
sms_test <- apply(sms_dtm_test, MARGIN = 2, convertCounts)

smsClassifier <- naiveBayes(sms_train, sms_train_labels, laplace = 1)

sms_test_pred <- predict(smsClassifier, sms_test)

library(gmodels)

CrossTable(sms_test_pred, sms_test_labels, dnn = c("predicted", "actual"))