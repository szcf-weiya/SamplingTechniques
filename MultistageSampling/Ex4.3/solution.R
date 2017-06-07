## data
n = 5; N = 100
m = 3; M = 30

f1 = n/N
f2 = m/M

y1 = c(57, 59, 64)
y2 = c(38, 41, 50)
y3 = c(51, 60, 63)
y4 = c(48, 53, 49)
y5 = c(62, 55, 54)

y = data.frame(y1, y2, y3, y4, y5)
y
ybar = colMeans(y)
y.var = apply(y, 2, var)

## 总体均值
ybar.bar = mean(ybar)
s12 = var(ybar)
s22 = mean(y.var)

## 方差估计
ybar.bar.var = (1-f1)/n*s12 + f1*(1-f2)/(n*m)*s22

## 总量估计
Y = N*M*ybar.bar
Y.var = N^2*M^2*ybar.bar.var
Y.sd = sqrt(Y.var)
Y + c(-1, 1)*Y.sd*qnorm(.975)

## 最优m n
s12.hat = s12 - (1-f2)/m*s22
s22.hat = s22
su2.hat = s12.hat - s22.hat/M

## c1/c2
c1c2 = 10
ybar.bar.var = 15

m.opt = sqrt(s22.hat/su2.hat*c1c2)

## 最优的m=2
n.opt = (s12.hat - s22.hat/M + s22.hat/2)/(ybar.bar.var + s12.hat/N)
## 最优的n=4
