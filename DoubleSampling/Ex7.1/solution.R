## data
nh.hat = c(540, 320, 100, 40)
nh = c(80, 60, 40, 20)
yh.bar = c(2, 7, 15, 40)
## w_h'
wh.hat = nh.hat/sum(nh.hat)
ystD.bar = sum(wh.hat*yh.bar)


sh2 = c(1.01, 2.71, 15.38, 690.53)
yhj.sum = c(400, 3100, 9600, 45120)

N = 8000
n.hat = 1000

## Y.hat
Y.hat = N*ystD.bar

## v(ystD.bar)
ystD.bar.var = sum((1/nh-1/nh.hat)*wh.hat^2*sh2) + (1/n.hat-1/N)*sum(wh.hat*(yh.bar-ystD.bar)^2)

ystD.sd = sqrt(ystD.bar.var)
