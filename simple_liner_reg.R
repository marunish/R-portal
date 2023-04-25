#最適化アルゴリズムを自分で書きたい。
# どうしても書きたい

library(readr)

big <- read_csv("osukina.csv", col_names  = FALSE)
#広い方（区単位）

x1 <- big[,1]
as.vector(x1)
y1 <- big[,2]
as.vector(y1)


a1 <- seq(0,10,1)
b1 <- seq(0,10,1)

length(a1)
length(b1)

View(x1)
length(x1)
# これはよこの長さをみているのか？

ncol(x1)
nrow(x1)
#これで横長の要素なのかなという事
# が理解できました。

x_p <- c(1,2,3,4,5)
ncol(x_p)
nrow(x_p)
length(x_p)

View(y)
#なぜ106にならないのか

calc1 <- y1 - a1[3]* x1 - rep(b1[5],times = 106)
sum(calc1^2)
# 1495031

calc1 <- y1 - a1[6]* x1 - rep(b1[2],times = 106)
sum(calc1^2)
# 9435021

calc1 <- y1 - a1[2]* x1 - rep(b1[10],times = 106)
sum(calc1^2)
# 412347
# まだ一番下がまともそう

Ans <- c(NULL)
for (n in 1:length(a1)){
  for (m in 1:length(b1)){
    calc1 <- y1 - a1[n]* x1 - rep(b1[m],times = 106)
    Ans <- c(Ans,sum(calc1^2))
  }
}

Ans

min_ans <- min(Ans)
min_ans
min_index <- which.min(Ans)
min_index

n_good <- min_index %/% 11
m_good <- min_index %% 11

n_good
m_good

#点のプロットと、線形回帰を確かめる。

plot(x1,y1)
# これがエラーを吐く

class(x1)
class(y1)

# 全然ベクトルではなかったので、
# tbl_dfだった
library(dplyr)
plot(pull(x1),pull(y1))
abline(m_good,n_good,col = "red")