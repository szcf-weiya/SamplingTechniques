# 总体群数
N = 315

# 样本群数
n = 8

# 群体规模
M = 6

# 抽样比
f = n/N

# 样本的群均值
ybar = c(75.00, 89.00, 95.67, 104.67, 108.50, 106.53, 112.83, 93.33)

ybar.bar = mean(ybar)

sb2 = M * var(ybar)

ybar.bar.var = (1-f)/(n*M) * sb2

region = ybar.bar + c(1, -1) * sqrt(ybar.bar.var) * 1.96

region
