#最適化アルゴリズムを自分で書きたい。
# どうしても書きたい

library(readr)

big <- matrix(c(1, 2, 3, 4, 3, 5, 5, 9), ncol = 2)
#広い方（区単位）

x1 <- big[,1]
as.vector(x1)
y1 <- big[,2]
as.vector(y1)

# 傾きとy軸で、どれくらいの間隔で検証するかを考える。
dis_x <- 0.05
dis_y <- 0.5

a1 <- seq(0,1,dis_x)
b1 <- seq(0,10,dis_y)

#a1が、傾き候補として扱います。
# 大体の大きさを推量して行うことが重要
length(a1)
length(b1)

# View(x1)
length(x1)
# これはよこの長さをみているのか？

ncol(x1)
nrow(x1)
#これで横長の要素なのかなという事
# が理解できました。

# x_p <- c(1,2,3,4,5)
# ncol(x_p)
# nrow(x_p)
# length(x_p)

# View(y1)
#なぜ106にならないのか

# 以下計算式が動いているのか、
# ベクトルを触る上で不具合がないかを三回確かめるもの。
# 調査数が10より小さいとエラーを吐きそう。
calc1 <- y1 - a1[3]* x1 - rep(b1[5],times = length(y1))
sum(calc1^2)
# 1495031

calc1 <- y1 - a1[6]* x1 - rep(b1[2],times = length(y1))
sum(calc1^2)
# 9435021

calc1 <- y1 - a1[2]* x1 - rep(b1[10],times = length(y1))
sum(calc1^2)
# 412347
# まだ一番下がまともそう

Ans <- c(NULL)
for (n in 1:length(a1)){
  for (m in 1:length(b1)){
    calc1 <- y1 - a1[n]* x1 - rep(b1[m],times = length(y1))
    Ans <- c(Ans,sum(calc1^2))
  }
}

Ans

min_ans <- min(Ans)
min_ans
min_index <- which.min(Ans)
min_index

n_good <- (min_index %/% length(b1))*dis_x
m_good <- (min_index %% length(b1))*dis_y
#なんか11を入れた方がいい推定ができる。

n_good
m_good

#点のプロットと、線形回帰を確かめる。

# plot(x1,y1)
# これがエラーを吐く

class(x1)
class(y1)

# 全然ベクトルではなかったので、
# tbl_dfだった
# library(dplyr)
# plot(pull(x1),pull(y1))
# abline(m_good,n_good,col = "red")

# View(big)

library(dplyr)
df <- as_tibble(data.frame(x = x1, y = y1))

df <- as.data.frame(df)

df

library(ggplot2)

ggplot(data = df, aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = m_good, slope = n_good, col = "red")


# abline関数を使う練習
#abline(5,0.1,col = "blue", lwd=1, lty=2)

#どうも0.1くらいのところでやるべきっぽい