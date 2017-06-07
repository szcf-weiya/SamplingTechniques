## data
n = 20
ybar = c(16000, 14033, 20325, 14150, 13567, 29333, 26783, 19650, 18508, 18317, 25808, 24125, 22300, 14675, 18725, 10167, 44950, 20750, 14708, 12567)
ybar.bar = mean(ybar)
ybar.bar.var = 1/n*var(ybar)
ybar.bar.sd = sqrt(ybar.bar.var)
ybar.bar + c(-1, 1)*qnorm(0.95)*ybar.bar.sd
