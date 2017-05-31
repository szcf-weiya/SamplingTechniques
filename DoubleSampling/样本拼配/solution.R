## data
y1 = c(610, 580, 900, 820, 720, 650, 1000, 590, 830, 750)
y2 = c(660, 1050, 610, 820, 770, 630, 570, 620, 810, 920)
m = 5
u = 5
n = 10

## mean
y1n.bar = mean(y1)
y1m.bar = mean(y1[6:10])
y2m.bar = mean(y2[1:5])
y2u.bar = mean(y2[6:10])

## regression
rss = sum((y1[6:10]-y1m.bar)*(y2[1:5]-y2m.bar))
b = rss/sum((y1[6:10]-y1m.bar)^2)
r = rss/sqrt(sum((y1[6:10]-y1m.bar)^2)*sum((y2[1:5]-y2m.bar)^2))

## estimate
y2u.bar.e = y2u.bar
y2m.bar.e = y2m.bar + b*(y1n.bar - y1m.bar)
var2 = var(y2)
var.u = var2/u
var.m = var2*(1-r^2)/m+r^2*var2/n
phi = var.m/(var.u+var.m)
y2.bar.e = phi*y2u.bar.e + (1-phi)*y2m.bar.e
y2.bar.e.var = (n-u*r^2)/(n^2-u^2*r^2)*var2
y2.bar.e.sd = sqrt(y2.bar.e.var)

## optimism
u.opt = n/(1+sqrt(1-r^2))
v.opt = (1+sqrt(1-r^2))/(2*n)*var2
s.opt = sqrt(v.opt)
