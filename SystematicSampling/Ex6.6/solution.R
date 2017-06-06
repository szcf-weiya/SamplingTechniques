## 耶茨方法
y = seq(1, 30, by = 2)
N = 15; n = 3;
k = N/n
## r = 1
r = 1
y.sample = c(1, 11, 21)
w1 = 1/3 + (2*r-k-1)/(2*(n-1)*k)
w3 = 1/3 - (2*r-k-1)/(2*(n-1)*k)
w2 = 1/3
w = c(w1, w2, w3)
y.bar = sum(w*y.sample)

## 贝尔豪斯与拉奥方法
y = c(1:28)
N = 28; n = 5
k = ceiling(N/n)

## case 1
r = 3
y.sample = y[seq(r, N, by = k)]
y.sample.mean = mean(y.sample)

## r+(n-1)k<=N
flag = r + (n-1)*k < N
flag

nw1 = 1/n + (2*r+(n-1)*k-(N+1))/(2*(n-1)*k)
nw5 = 1/n - (2*r+(n-1)*k-(N+1))/(2*(n-1)*k)

nw = c(nw1, rep(1/n, 3), nw5)
y.B.mean = sum(y.sample*nw)

## case 2
r = 15
y.sample = c(15, 21, 27, 5, 11)
y.sample.mean = mean(y.sample) # 15.8
n2 = 2 # after yn

nw1 = 1/n + (2*r+(n-1)*k-(N+1)-2*n2*N/n)/(2*(N-k))
nw5 = 1/n - (2*r+(n-1)*k-(N+1)-2*n2*N/n)/(2*(N-k))

nw = c(nw1, rep(1/n, 3), nw5)
y.B.mean = sum(y.sample*nw)

