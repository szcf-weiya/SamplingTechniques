a = c(4, 7, 1, 3, 3, 4, 4, 2, 3, 2, 1, 3, 2, 5, 4, 1, 4, 2, 3, 1, 3, 3, 4, 0, 3)
M = c(8, 12, 4, 5, 6, 6, 7, 5, 8, 3, 2, 6, 5, 10, 9, 3, 6, 5, 5, 4, 6, 8, 7, 3, 8)
N = 417
n = 25
f = n/N
## 群规模不等的比例估计
P.hat = sum(a)/sum(M)
m.bar = mean(M)

P.var = (1-f)/(n*m.bar^2*(n-1))*(sum(a^2)+P.hat^2*sum(M^2)-2*P.hat*sum(a*M))

## 置信区间
print(paste0("[", P.hat - sqrt(P.var), ", ", P.hat + sqrt(P.var),"]"))

## 随机抽样

P.srs.hat = sum(a)/sum(M)
P.srs.var = (1-f)/(sum(M)-1)*P.srs.hat*(1-P.srs.hat) ## 注意，随机抽样的n为M!!!

## 设计效应
deff = P.var/P.srs.var

print(paste0("deff = ", deff))

## 群内相关系数

rho = (deff-1)/(m.bar-1)

print(paste0("rho = ", rho))
