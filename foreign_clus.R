# https://www.toukei.metro.tokyo.lg.jp/gaikoku/2021/ga21010000.htm
# 令和三年、一番右のcsv

library(readr)
tokyo_for <- read_csv("ga21ev0100.csv")

View(tokyo_for)

library(dplyr)
tokyo_cl <- tokyo_for %>% select(-1, -3)

View(tokyo_cl)

tokyo_cl <- tokyo_cl[-c(64:70), ]

View(tokyo_cl)

dd_t<-daisy(tokyo_cl)
#print(round(as.matrix(dd),2))
fviz_dist(dd_t)
# 距離行列を求めましたが。

tokyo_cl2 <- tokyo_cl %>% select(-1, -2)

tokyo_cl2

tokyo_cl3 <- tokyo_cl2[-c(1:2),]

dd_t<-daisy(tokyo_cl3)
#print(round(as.matrix(dd),2))
fviz_dist(dd_t)

View(tokyo_cl3)

tokyo_cl4 <- tokyo_cl3[-c(24),]

dd_t<-daisy(tokyo_cl4)
#print(round(as.matrix(dd),2))
fviz_dist(dd_t)

View(tokyo_cl4)

###

gt_t<-scale(tokyo_cl4)
dm_t<-daisy(gt_t)
fviz_dist(dm_t)
# ちょっとだけ右上が綺麗になりましたね。

#k平均法クラスタリングの推定
set.seed(561294)#毎同じ結果が出るようにseedを設定する（整数であれば何でもいいです）
kmgt<-kmeans(gt_t,centers=5,nstart=25) #centersはクラスター数を設定する

#最適なクラスター数の図(elbow method)
#xinterceptは垂直線の位置を設定する（適切なクラスター数は図を見ながら自分で決める）
fviz_nbclust(gt_t, kmeans, method = "wss") + geom_vline(xintercept = 5, linetype = 2)

#クラスタ－の記述
aggregate(gt_t,by=list(cluster=kmgt$cluster),mean)

kmgt2<-cbind(gt_t,cluster= kmgt$cluster)#クラスターの情報を元データと結合する
fviz_cluster(kmgt,data=gt_t
             , ellipse.type = "euclid"
             , star.plot = TRUE
             , repel = T
             , ggtheme = theme_bw())
