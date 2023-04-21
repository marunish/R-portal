#相当高い確率（99.5%）でGood Luck

A <- sample(1:6, 1, replace=TRUE, prob=c(995,rep(1,5)))

if(A==1) {
    print("大吉")
}else 
{
        print("凶")
    }


