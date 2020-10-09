# iris 컬럼 2개, 3개, 4개 => 클러스터링 수행

library(foreign)
library(readxl)
library(ggplot2)
library(dplyr)

data(iris)
head(iris)

# 컬럼 2개 : Sepal.Length와 Sepal.Width
iris_kmeans_SLSW <- kmeans(iris[, c("Sepal.Length", "Sepal.Width")], 3) 

table(iris[, 5], iris_kmeans_SLSW$cluster)

iris_kmeans_SLSW

iris_kmeans_SLSW$cluster

iris_kmeans_SLSW$centers
#   Sepal.Length Sepal.Width
# 1     5.773585    2.692453
# 2     6.812766    3.074468
# 3     5.006000    3.428000

iris_kmeans_SLSW$totss      # 130.4753
iris_kmeans_SLSW$withinss   # 11.3000 12.6217 13.1290
iris_kmeans_SLSW$betweenss  # 93.42456

iris_plot_SLSW <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point(shape = 19, size = 4) +
  ggtitle("iris data")

iris_plot_SLSW


# 컬럼 2개 : Sepal.Length와 Petal.Length
iris_kmeans_SLPL <- kmeans(iris[, c("Sepal.Length", "Petal.Length")], 3)

table(iris[, 5], iris_kmeans_SLPL$cluster)

iris_kmeans_SLPL$centers
#   Sepal.Length Petal.Length
# 1     6.839024     5.678049
# 2     5.007843     1.492157
# 3     5.874138     4.393103

iris_kmeans_SLPL$totss # 566.4937
iris_kmeans_SLPL$withinss # 20.407805  9.893725 23.508448
iris_kmeans_SLPL$betweenss # 512.6838

iris_plot_SLPL <- ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Length, colour = Species)) +
  geom_point(shape = 19, size = 4) +
  ggtitle("iris data")

iris_plot_SLPL


# 컬럼 2개 : Sepal.Length와 Petal.Width
iris_kmeans_SLPW <- kmeans(iris[, c("Sepal.Length", "Petal.Width")], 3)

table(iris[, 5], iris_kmeans_SLPW$cluster)

iris_kmeans_SLPW$centers
#   Sepal.Length Petal.Width
# 1     5.005556   0.3037037
# 2     6.819565   1.9760870
# 3     5.850000   1.4520000

iris_kmeans_SLPW$totss # 188.7383
iris_kmeans_SLPW$withinss # 8.907593 14.536087  9.289800
iris_kmeans_SLPW$betweenss # 156.0048

iris_plot_SLPW <- ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Width, colour = Species)) +
  geom_point(shape = 19, size = 4) +
  ggtitle("iris data")

iris_plot_SLPW


# 컬럼 2개 : Sepal.Width와 Petal.Length
iris_kmeans_SWPL <- kmeans(iris[, c("Sepal.Width", "Petal.Length")], 3)

table(iris[, 5], iris_kmeans_SWPL$cluster)

iris_kmeans_SWPL$centers
#   Sepal.Width Petal.Length
# 1    3.428000     1.462000
# 2    2.750877     4.328070
# 3    3.032558     5.672093

iris_kmeans_SWPL$totss # 492.6323
iris_kmeans_SWPL$withinss # 8.51860 18.21754 14.00093
iris_kmeans_SWPL$betweenss # 451.8953

iris_plot_SWPL <- ggplot(data = iris, aes(x = Sepal.Width, y = Petal.Length, colour = Species)) +
  geom_point(shape = 19, size = 4) +
  ggtitle("iris data")

iris_plot_SWPL


# 컬럼 2개 : Sepal.Width와 Petal.Width
iris_kmeans_SWPW <- kmeans(iris[, c("Sepal.Width", "Petal.Width")], 3)

table(iris[, 5], iris_kmeans_SWPW$cluster)

iris_kmeans_SWPW$centers
#   Sepal.Width Petal.Width
# 1    3.451020    0.244898
# 2    2.707547    1.309434
# 3    3.041667    2.052083

iris_kmeans_SWPW$totss # 114.8769
iris_kmeans_SWPW$withinss # 6.283673 7.422264 6.896458
iris_kmeans_SWPW$betweenss # 94.27447

iris_plot_SWPW <- ggplot(data = iris, aes(x = Sepal.Width, y = Petal.Width, colour = Species)) +
  geom_point(shape = 19, size = 4) +
  ggtitle("iris data")

iris_plot_SWPW


# 컬럼 2개 : Petal.Length와 Petal.Width
iris_kmeans_PLPW <- kmeans(iris[, c("Petal.Length", "Petal.Width")], 3)

table(iris[, 5], iris_kmeans_PLPW$cluster)

iris_kmeans_PLPW$centers
#   Petal.Length Petal.Width
# 1     1.462000    0.246000
# 2     5.595833    2.037500
# 3     4.269231    1.342308

iris_kmeans_PLPW$totss # 550.8953
iris_kmeans_PLPW$withinss # 2.02200 16.29167 13.05769
iris_kmeans_PLPW$betweenss # 519.524

iris_plot_PLPW <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point(shape = 19, size = 4) +
  ggtitle("iris data")

iris_plot_PLPW


# 컬럼 3개 : Sepal.Length, Sepal.Width, Petal.Length
iris_kmeans_SLSWPL <- kmeans(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length")], 3)

table(iris[, 5], iris_kmeans_SLSWPL$cluster)

iris_kmeans_SLSWPL$centers
#   Sepal.Length Sepal.Width Petal.Length
# 1     6.835714    3.064286     5.654762
# 2     5.846552    2.732759     4.363793
# 3     5.006000    3.428000     1.462000

iris_kmeans_SLSWPL$totss # 594.8007
iris_kmeans_SLSWPL$withinss # 24.63690 30.18603 14.60680
iris_kmeans_SLSWPL$betweenss # 525.3709

# iris_plot_SLSWPL <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, z = Petal.Length, colour = Species)) +
#   geom_point(shape = 19, size = 4) +
#   ggtitle("iris data")
# 
# iris_plot_SLSWPL


# 컬럼 3개 : Sepal.Length, Sepal.Width, Petal.Width
iris_kmeans_SLSWPW <- kmeans(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Width")], 3)

table(iris[, 5], iris_kmeans_SLSWPW$cluster)

iris_kmeans_SLSWPW$centers
#   Sepal.Length Sepal.Width Petal.Width
# 1     5.006000    3.428000    0.246000
# 2     5.785185    2.696296    1.431481
# 3     6.821739    3.078261    1.963043

iris_kmeans_SLSWPW$totss # 217.0452
iris_kmeans_SLSWPW$withinss # 13.67320 17.06389 17.92370
iris_kmeans_SLSWPW$betweenss # 168.3844


# 컬럼 3개 : Sepal.Length, Petal.Length, Petal.Width
iris_kmeans_SLPLPW <- kmeans(iris[, c("Sepal.Length", "Petal.Length", "Petal.Width")], 3)

table(iris[, 5], iris_kmeans_SLPLPW$cluster)

iris_kmeans_SLPLPW$centers
#   Sepal.Length Petal.Length Petal.Width
# 1     5.901613     4.393548    1.433871
# 2     6.850000     5.742105    2.071053
# 3     5.006000     1.462000    0.246000

iris_kmeans_SLPLPW$totss # 653.0637
iris_kmeans_SLPLPW$withinss # 34.46613 20.76579  8.11020
iris_kmeans_SLPLPW$betweenss # 589.7215


# 컬럼 3개 : Sepal.Width, Petal.Length, Petal.Width
iris_kmeans_SWPLPW <- kmeans(iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")], 3)

table(iris[, 5], iris_kmeans_SWPLPW$cluster)

iris_kmeans_SWPLPW$centers
#   Sepal.Width Petal.Length Petal.Width
# 1    3.035000     1.495000   0.2550000
# 2    2.875758     4.925253   1.6818182
# 3    3.651613     1.490323   0.2677419

iris_kmeans_SWPLPW$totss # 579.2023
iris_kmeans_SWPLPW$withinss # 4.824500 92.155960  3.492258
iris_kmeans_SWPLPW$betweenss # 478.7295


# 컬럼 4개 : Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
iris_kmeans_SLSWPLPW <- kmeans(iris[, -5], 3)

table(iris[, 5], iris_kmeans_SLSWPLPW$cluster)

iris_kmeans_SLSWPLPW$centers
#   Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1     5.006000    3.428000     1.462000    0.246000
# 2     5.901613    2.748387     4.393548    1.433871
# 3     6.850000    3.073684     5.742105    2.071053

iris_kmeans_SLSWPLPW$totss # 681.3706
iris_kmeans_SLSWPLPW$withinss # 15.15100 39.82097 23.87947
iris_kmeans_SLSWPLPW$betweenss # 602.5192
