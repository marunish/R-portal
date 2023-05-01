# 歴史なので、年代に対してデータがあるようなものが適している。
# 四時関数のプロットを実装。
# Example


# データの作成
set.seed(123)
# シード値を決めて
date <- seq(as.Date("2020-01-01"), as.Date("2022-01-01"), by = "day")
# 毎日の日付をdate関数にいれまして
value <- 10 + 5 * sin(seq(0, 2 * pi, length.out = length(date))) + rnorm(length(date), 0, 1)
# 基本的にはサイン関数に乗っかりながら、rnormでバラす
# シード値で決められてランダムを返す
data <- data.frame(date, value)
# データとして、二つをデータセットとしてくみこむ。


# 日付列を日付オブジェクトに変換
data$date <- as.Date(data$date, format = "%Y-%m-%d")
# 今、日付はシンプルに日付列でしかない。これを、フォーマット付きの
# ちゃんと扱えるような日付データへと変更するas.Date
# 日付オブジェクトとして厳密にデータ形式と、その書き方を統一している必要がある。


# 中身をチェック
View(data)


# データの準備
degree <- 4 # 4次関数にフィッティングする例
data$x <- as.numeric(data$date)
# 日付を数字にする（通し番号）
# そして代入する。
data$x2 <- data$x^2
# 二乗して入れる
data$x3 <- data$x^3
# 三乗して入れる
data$x4 <- data$x^4
# 四乗して入れる


# 中身をチェック
View(data)


# 2次関数の当てはめ
fit <- lm(value ~ x + x2 + x3 + x4, data = data)
# これが真髄にあたる。
# lm関数がすごくて、その二乗の和が小さくなるように、
# 自分で行ってくれる。
# 線型結合を行わせる関数なので、その係数を複数考えてくれるのだが、
# あらかじめ数値を（逆関数が値の範囲でのあるような関数でひっくり返した上で、
# ここにfitという係数をかませる。


summary(fit)
# どこまでの係数が有意かどうかまで判断してくれる。
# おそらくEstimateが係数。


# プロットの作成
library(ggplot2)
ggplot(data, aes(x = date, y = value)) +
  geom_point() +
  geom_line(aes(y = predict(fit))) +
  labs(x = "Date", y = "Value")
# ggplot(data, aes(x = date, y = value)): 
# データフレームdataを使用して、
# プロットを作成するための基本的な設定を行います。aes()関数により、date列をx軸、value列をy軸に設定します。
# geom_point(): ポイントプロットを追加します。これにより、データセットの各観測値が点で表示されます。
# geom_line(aes(y = predict(fit))): 多項式回帰の結果をプロットします。predict()関数を使用して、フィットしたモデルから予測値を取得し、geom_line()関数を使用して直線を引きます。
