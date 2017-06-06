x = c(1500, 1200, 2000, 1800, 1300, 3000, 800, 1400, 1600, 1100)
y = c(2000, 1800, 2800, 2500, 1900, 5800, 1300, 2000, 2300, 1600)

ybar = mean(y)
xbar = mean(x)

Rhat = ybar/xbar

xbar.hat = 1500
nhat = 100
n = 10

sy2 = var(y)
sx2 = var(x)

syx = cov(x, y)

y.RD = Rhat*xbar.hat
y.RD.var = sy2/n + (1/n - 1/nhat)*(Rhat^2*sx2 - 2*Rhat*syx)
y.RD.s = sqrt(y.RD.var)




