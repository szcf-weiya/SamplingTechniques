## data
y = c(10, 8, 6, 5, 9, 8, 8, 5, 9, 9, 9, 10, 4, 3, 1, 2, 3, 4, 0, 6, 3, 5, 0, 3, 0, 0, 4, 0, 8, 0, 10, 5, 6, 1, 3, 3, 1, 5, 5, 4)

y.sy.bar = mean(y)
## estimate
n = 40; N = 4000
f = n/N
y.sy.bar.var.1 = (1-f)/n*var(y)
y.sy.bar.var.2 = (1-f)/(n*2*(n-1))*sum((y[-1]-y[-40])^2)
y.sy.bar.var.3 = (1-f)/n^2*sum((y[seq(2,40,by=2)] - y[seq(1,40,by=2)])^2)

