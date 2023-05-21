# 最終的なコード
get.netgen <- function(type,zone.id,purpose=0,gender="all"){
  
  ## ①条件文を記載します
  
  if (!(type %in% c("gross","net","ratio"))){stop(message="Warning! Please put gross or net or ratio")}
  # Put error if TYPE is included in gross net ratio.
  
  if (is.character(zone.id) == F | !zone.id %in% h30$計基ゾーン){stop(message="Warning! Please put character string")}
  # Put error if ZONE.ID is NOT character, if ZONE.ID is not included in "計器ゾーン".
  # これは例え、ちゃんと数字を入力していたとしても、適当な数字でbreakを通過するため、きちんとデータセットの中にある数字でなければ、止まらせるように書き換えた。
  # これは行単位でデータが増減しても、対応可能な柔軟性がある。
  
  if (!(purpose %in% c(0,1,2,3,4,5,6,7))|is.numeric(purpose)==F){stop(message="Warning! Please put number 1~7")}
  # Put error if ZONE.ID is NOT numeric,if purpose is not included in 1:7.
  
  if (!(gender %in% c("male","female","all"))){stop(message="Warning! Please put male or female or all")}
  # Put error if TYPE is included in male female all
  
  
  
  # 条件文
  
  if(purpose==0){ # データの並びから、全目的をpurpose=0から一行で指定しにくい。
    
    # 使う列が変わるため、typeで分岐させる。
    if (type == "gross"){
      h30_sub <- subset(h30,計基ゾーン == zone.id) #zone.idで、サブセットを取り出す
      h30_sub_ma <- subset(h30_sub,性別 == "男性") # 計算に際し、元々データセットが分かれていて欲しい
      h30_sub_fe <- subset(h30_sub,性別 == "女性") # 男女でデータセットを分ける。
      
      # 三つの条件（両性、女性、男性）で分岐させるために、ifelseを2回使用
      # 以下同様の構造のコードであるのでここで徹底解説
      # ifelse(条件,Tなら実行する動作,Fなら実行する動作)として使用できるため、
      # 三つの条件を分岐させることができる。ここで性別分岐をしたので以下では分岐先の話をする
      # やりたいことはそれぞれのサブセット（sone.idが指定されている）の中から、
      # 必要な列を抽出する。
      # 割るものが0ではエラーを吐いてしまうので、naと表示されたものを取り除きその要素の合計をとる
      # 具体的なデータを0010で確認すると、0でわられるものは0である場合が多い印象を受けたが
      # これはpurpose==0であるからである可能性が高く、検証が必要である。
      ifelse (gender == "all",result <- sum(h30_sub[,17]/h30_sub[,7],na.rm=T)/sum(!is.na(h30_sub[,17]/h30_sub[,7]))
              ,ifelse(gender == "male"
                      ,result <- sum(h30_sub_ma[,17]/h30_sub_ma[,7],na.rm=T)/sum(!is.na(h30_sub_ma[,17]/h30_sub_ma[,7]))
                      ,result <- sum(h30_sub_fe[,17]/h30_sub_fe[,7],na.rm=T)/sum(!is.na(h30_sub_fe[,17]/h30_sub_fe[,7]))))
    }
    
    # 使う列が変わるため、typeで分岐させる。
    if(type == "net"){
      h30_sub <- subset(h30,計基ゾーン == zone.id) #zone.idで、サブセットを取り出す
      h30_sub_ma <- subset(h30_sub,性別 == "男性") # 計算に際し、元々データセットが分かれていて欲しい
      h30_sub_fe <- subset(h30_sub,性別 == "女性") # 男女でデータセットを分ける。
      
      # 三つの条件（両性、女性、男性）で分岐させるために、ifelseを2回使用
      ifelse (gender == "all",result <- sum(h30_sub[,17]/h30_sub[,8],na.rm=T)/sum(!is.na(h30_sub[,17]/h30_sub[,8]))
              ,ifelse(gender == "male"
                      ,result <- sum(h30_sub_ma[,17]/h30_sub_ma[,8],na.rm=T)/sum(!is.na(h30_sub_ma[,17]/h30_sub_ma[,8]))
                      ,result <- sum(h30_sub_fe[,17]/h30_sub_fe[,8],na.rm=T)/sum(!is.na(h30_sub_fe[,17]/h30_sub_fe[,8]))))
    }
    
    # 使う列が変わるため、typeで分岐させる。
    if (type == "ratio"){
      h30_sub <- subset(h30,計基ゾーン == zone.id) #zone.idで、サブセットを取り出す
      h30_sub_ma <- subset(h30_sub,性別 == "男性") # 計算に際し、元々データセットが分かれていて欲しい
      h30_sub_fe <- subset(h30_sub,性別 == "女性") # 男女でデータセットを分ける。
      
      # 三つの条件（両性、女性、男性）で分岐させるために、ifelseを2回使用
      ifelse (gender == "all",result <- sum(h30_sub[,8]/h30_sub[,7],na.rm=T)/sum(!is.na(h30_sub[,8]/h30_sub[,7]))
              ,ifelse(gender == "male"
                      ,result <- sum(h30_sub_ma[,8]/h30_sub_ma[,7],na.rm=T)/sum(!is.na(h30_sub_ma[,8]/h30_sub_ma[,7]))
                      ,result <- sum(h30_sub_fe[,8]/h30_sub_fe[,7],na.rm=T)/sum(!is.na(h30_sub_fe[,8]/h30_sub_fe[,7]))))
    }
  }
  
  
  
  
  if(purpose!=0){ # データの並びから、目的別ならばpurposeを用いて列を取り出すことができる。
    
    # 使う列が変わるため、typeで分岐させる。
    if (type == "gross"){
      h30_sub <- subset(h30,計基ゾーン == zone.id) #zone.idで、サブセットを取り出す
      h30_sub_ma <- subset(h30_sub,性別 == "男性")
      h30_sub_fe <- subset(h30_sub,性別 == "女性")
      ifelse (gender == "all",result <- sum(h30_sub[,purpose+9]/h30_sub[,7],na.rm=T)/sum(!is.na(h30_sub[,17]/h30_sub[,7]))
              ,ifelse(gender == "male"
                      ,result <- sum(h30_sub_ma[,purpose+9]/h30_sub_ma[,7],na.rm=T)/sum(!is.na(h30_sub_ma[,17]/h30_sub_ma[,7]))
                      ,result <- sum(h30_sub_fe[,purpose+9]/h30_sub_fe[,7],na.rm=T)/sum(!is.na(h30_sub_fe[,17]/h30_sub_fe[,7]))))
    }
    
    # 使う列が変わるため、typeで分岐させる。
    if(type == "net"){
      h30_sub <- subset(h30,計基ゾーン == zone.id) #zone.idで、サブセットを取り出す
      h30_sub_ma <- subset(h30_sub,性別 == "男性")
      h30_sub_fe <- subset(h30_sub,性別 == "女性")
      ifelse (gender == "all",result <- sum(h30_sub[,purpose+9]/h30_sub[,8],na.rm=T)/sum(!is.na(h30_sub[,17]/h30_sub[,8]))
              ,ifelse(gender == "male"
                      ,result <- sum(h30_sub_ma[,purpose+9]/h30_sub_ma[,8],na.rm=T)/sum(!is.na(h30_sub_ma[,17]/h30_sub_ma[,8]))
                      ,result <- sum(h30_sub_fe[,purpose+9]/h30_sub_fe[,8],na.rm=T)/sum(!is.na(h30_sub_fe[,17]/h30_sub_fe[,8]))))
    }
    
    # 使う列が変わるため、typeで分岐させる。
    if (type == "ratio"){
      h30_sub <- subset(h30,計基ゾーン == zone.id) #zone.idで、サブセットを取り出す
      h30_sub_ma <- subset(h30_sub,性別 == "男性")
      h30_sub_fe <- subset(h30_sub,性別 == "女性")
      ifelse (gender == "all",result <- sum(h30_sub[,8]/h30_sub[,7],na.rm=T)/sum(!is.na(h30_sub[,8]/h30_sub[,7]))
              ,ifelse(gender == "male"
                      ,result <- sum(h30_sub_ma[,8]/h30_sub_ma[,7],na.rm=T)/sum(!is.na(h30_sub_ma[,8]/h30_sub_ma[,7]))
                      ,result <- sum(h30_sub_fe[,8]/h30_sub_fe[,7],na.rm=T)/sum(!is.na(h30_sub_fe[,8]/h30_sub_fe[,7]))))
    }
  }
  
  # 回答の出力
  return(result)
}