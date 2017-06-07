## 以单位为群，汽车为小单元
M0 = 186; n = 10
mi = c(5, 8, 5, 4, 6, 9, 5, 3, 7, 3)
yi = c(14230, 21336, 13650, 11568, 15216, 23094, 13650, 7443, 16723, 8391)
ybari = c(2846, 2667, 2730, 2892, 2536, 2566, 2730, 2481, 2389, 2797)
ybarbar = mean(ybari)

## 总运量的估计
YHH = M0*ybarbar
ybarbar.var = 1/n*var(ybari)
ybarbar.sd = sqrt(ybarbar.var)
YHH.sd = M0*ybarbar.sd
YHH.cv = YHH.sd/YHH

## 95%置信度下，最大相对误差
qnorm(0.975)*YHH.cv
