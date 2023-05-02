library(readr)
traffic_tosh1 <- read_csv("traffic_toshima.csv")

View(traffic_tosh1)

# 現状これはtibbleというやばいデータ型になっている。
# tibbleのままで作業して問題ないのであろうか。

traf_dataset <- data.frame(traffic_tosh1)

View(traf_dataset)

# 原則データセットを対象とした処理がおこなわれることは多く、
# 変なデータ型でやるべきではない。
# しかし、Viewで見る限り違いはない。

# データのよくわからない部分を切り取る
c_tra_data <- subset(traf_dataset, select = -c(6:8))

# 1から365ずつ大きくなる31列のデータセットを作成する
my_data <- data.frame(matrix(seq(1, 10981, by = 366), ncol = 1, nrow = 31))
# 年度が通し番号で判断されているため、年度を保存した上で大体同じ
tra_data <- cbind(my_data,c_tra_data)
# データセットの一部の列の名前を変更する
names(tra_data)[1] <- c("new_date")

# selectはベクトルを入れると、列を当たり前に決してくる
# View(c_tra_data)

tra_data$new_date <- as.Date(tra_data$new_date, format = "%Y-%m-%d", origin = "1988-01-01")


# フッティングに移る
degree <- 4 # 4次関数にフィッティングする例
tra_data$x <- as.numeric(tra_data$new_date)
# 日付を数字にする（通し番号）
# そして代入する。
tra_data$x2 <- tra_data$x^2
# 二乗して入れる
tra_data$x3 <- tra_data$x^3
# 三乗して入れる
tra_data$x4 <- tra_data$x^4
# 四乗して入れる


# 2次関数の当てはめ
# fit <- lm(距離 ~ x + x2 + x3 + x4, tra_data = tra_data)
# tra_dataからvalue, x, x2, x3, x4の列を抽出して、回帰モデルを作成する
fit <- lm(距離 ~ x + x2 + x3 + x4, data = tra_data[, c("距離", "x", "x2", "x3", "x4")])

summary(fit)
# どこまでの係数が有意かどうかまで判断してくれる。
# おそらくEstimateが係数。


# プロットの作成
library(ggplot2)
ggplot(tra_data, aes(x = new_date, y = 距離)) +
  geom_point() +
  geom_line(aes(y = predict(fit)), colour = "red") +
  labs(x = "Year", y = "Length [m]", title = "Length of the road widening in Toshima-ku")+
  theme(plot.title = element_text(hjust = 0.5))
  