# 各层样本总量
N = c(200, 400, 750, 1500)

# 层权
W = N/sum(N)
n = 10

# 抽样比
f = n/N

# 样本
y1 = c(10, 40, 0, 110, 15, 10, 40, 80, 90, 0)
y2 = c(50, 130, 60, 80, 100, 55, 160, 85, 160, 170)
y3 = c(180, 260, 110, 0, 140, 60, 200, 180, 300, 220)
y4 = c(50, 35, 15, 0, 20, 30, 25, 10, 30, 25)
y = data.frame(y1, y2, y3, y4)
y.bar = sapply(y, function(x) mean(x))
y.s2 = sapply(y, function(x) var(x))
list(y.bar = y.bar, y.s2 = y.s2)
#y1.bar = mean(y1)
#y1.s2 = var(y1)

# 总体均值
## \hat Y = \sum_{h=1}^4 N_h \bar y_h
Y.hat = sum(N * y.bar)

# 总体方差
## v(\hat Y) = N^2\sum_{h=1}^4 W_h^2 v(\bar y_h)
##           = \sum_{h=1}^4 N_h^2 \frac{1-f_h}{n_h}s_h^2
Y.var = sum(N^2 * y.s2 * (1 - f) / n)
