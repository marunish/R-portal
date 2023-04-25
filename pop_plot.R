library(readr)

# 事前に用意したcsvファイルは、一列目に年齢
# 二列目に人数（全体に対する百分率*10）を記載したものです。
# *10は小数点以下の処理がPC苦手か＋人間視点でイメージしずらいかという配慮です。

big <- read_csv("osukina.csv", col_names  = FALSE)
#広い方（区単位）
small <- read_csv("osukina2.csv", col_names  = FALSE)
#狭い方（町単位）

#1 0 10.15660007
#2 1 10.13362133
#3 2 9.92106805
#4 3 10.11638728

View(big)

View(small)

colnames(big) <- c("年齢","人数割合")

#手動でCSVをつくるときは、タイトルを入れまくっておいた方がいいかもしれないが、
# 今回のCSVは数字しか入っておらず後でタグをつける。

View(big)

colnames(small) <- c("年齢","人数割合")

View(small)

barplot(big$年齢, names.arg = big$人数割合)
#これだと逆になるんです。


barplot(big$人数割合, names.arg = big$年齢, col = "#ff7f50",xlab = "Ages", 
        ylab = "pct*10",
        main = "Pop Plot by age")
barplot(small$人数割合, names.arg = small$年齢, col = "#000080",add = TRUE, density = 0.3,xlab = "Ages", 
        ylab = "pct*10",
        main = "Pop Plot by age")
legend("topright", c("chuo", "Tsukishima_3"), fill = c("#ff7f50", "#000080"))
# 二つの棒グラフを重ねる。

# 近似曲線が知りたい
x <- c(big[,1])
y <- c(big[,2])
# yとxをデータフレームにまとめる
df <- data.frame(y = y, x = x)

names(df)

# 回帰モデルを作成する
model <- lm(`人数割合` ~ 年齢, data = df)

model

# 回帰モデルを作成する
model <- lm(人数割合 ~ 年齢, data = df)

# 散布図と回帰直線をプロットする
plot(df$年齢, df$人数割合)
abline(model, col = "red")
