#幅が4m、水面に垂直、もっとも高い辺が水深2m板自体が2m
#である場合、

fp2 <- function(h) 4*h*ρ*g
fpp <- function(h) 4*h^2*ρ*g

I <- integrate(fp2,2,4)
hg <- I$value/8/ρ/g
I

I0 <- integrate(fpp,2,4)
sma <- I0$value/8/ρ/g
#second moment of ares

hc <- hg + sma/hg/8
hc
