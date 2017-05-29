n = 10
N = 33
M0 = 726
X = 30525
f = n/N
ybar = c(1.4667, 1.2667, 1.1615, 1.55, 1.265, 1.1143, 1.2381, 1.079, 1.0903, 1.3882)
x = c(800, 780, 1000, 700, 880, 1100, 850, 800, 1200, 830)
M = c(15, 18, 26, 14, 20, 28, 21, 19, 31, 17)
## 等概抽样，简单估计
ybar.bar.1 = mean(ybar)
Y.hat.1 = M0*ybar.bar.1
Y.var.1 = M0^2*(1-f)/n*sum((ybar - ybar.bar.1)^2)/(n-1)
Y.sd.1 = sqrt(Y.var.1)
print(paste0("ybar.bar.1 = ", ybar.bar.1, "; Y.hat.1 = ", Y.hat.1, "; Y.sd.1 = ", Y.sd.1))
## 等概抽样，加权估计
M.bar = mean(M)
## or ybar.bar.2 = sum(ybar*M)/(n*M.bar)
Y.hat.2 = N/n*sum(ybar*M)
## or Y.hat.2 = M0*ybar.bar.2
Y.var.2 = N^2*(1-f)/n*sum((ybar*M-mean(ybar*M))^2)/(n-1)
Y.sd.2 = sqrt(Y.var.2)
print(paste0("Y.hat.2 = ", Y.hat.2, "; Y.sd.2 = ", Y.sd.2))
## 等概抽样，比率估计
ybar.bar.3 = sum(ybar*M)/sum(M)
Y.hat.3 = M0*ybar.bar.3
Y.var.3 = N^2*(1-f)/n*sum((ybar*M-ybar.bar.3*M)^2)/(n-1)
Y.sd.3 = sqrt(Y.var.3)
print(paste0("Y.hat.3 = ", Y.hat.3, "; Y.sd.3 = ", Y.sd.3))
## 其他辅助变量的估计
R = sum(ybar*M)/sum(x)
Y.hat.4 = R*X
Y.var.4 = N^2*(1-f)/n*sum((ybar*M-R*x)^2)/(n-1)
Y.sd.4 = sqrt(Y.var.4)
print(paste0("Y.hat.4 = ", Y.hat.4, "; Y.sd.4 = ", Y.sd.4))







