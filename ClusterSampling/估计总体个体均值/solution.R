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

region = ybar.bar + c(-1, 1) * sqrt(ybar.bar.var) * 1.96

print(paste("置信区域为", '[', region[1], ', ',region[2], ']'))

# 群内相关系数

s2 = c(125.60, 233.60, 299.07, 177.87, 287.50, 42.27, 72.57, 527.87)

sw2 = mean(s2)

rho.hat = (sb2 - sw2)/(sb2 + (M-1)*sw2)

deff = 1 + (M-1)*rho.hat

print(paste('deff = ', deff))

# 设计效应deff=2.74表明,在这项调查,为达到同样的估计精度,整群抽样的样量大约为简单随机抽样样本量的2.74倍

#n_srs 令为简单随机抽样的样本量

n.srs = n*M/deff
