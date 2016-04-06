

pdf(file = '../img/ExampleOf4Houses.pdf',width = 10, height = 8)
par(mfrow=c(2,2), mar=c(5,5,2,1.5) + 0.1, cex.lab = 1.8, cex.main = 2.3, cex.axis=2.1)
plot(as.matrix(dat[day >= 1 & day <= 7,3, with = F]), type = 'l', xlab = 'HH', ylab = 'Energy', main = 'Household 1')
plot(as.matrix(dat[day >= 1 & day <= 7,4, with = F]), type = 'l', xlab = 'HH', ylab = 'Energy', main = 'Household 2')
plot(as.matrix(dat[day >= 1 & day <= 7,5, with = F]), type = 'l', xlab = 'HH', ylab = 'Energy', main = 'Household 3')
plot(as.matrix(dat[day >= 1 & day <= 7,25, with = F]), type = 'l', xlab = 'HH', ylab = 'Energy', main = 'Household 4')
dev.off()


# baseline stl
s <- as.vector(dat[day >= 3 & day <= 7,3, with = F])
s <-  ts(s[[1]], frequency = 48)
fit_stl <- stl(s, s.window=15)
fit_dec <- decompose(s,type = 'addit')
plot(fit_stl, range.bars = T)
plot(fit_dec)

plot(s)
lines(fit_stl$time.series[,1], col = 'red', lwd = 2)
lines(fit_dec$seasonal, col = 'blue', lwd = 2)

N_aggr = 500
aggr <- rowSums(as.matrix(dat[day >= 1 & day <= 7,c(3:N_aggr+3), with = F]))
aggr_test <- rowSums(as.matrix(dat[day >= 1+7 & day <= 7+7,c(3:N_aggr+3), with = F]))
s <-  ts(aggr, frequency = 48)
s_test <- ts(aggr_test, frequency = 48)
# forecasts
fit_stl <- stl(s, s.window=15)
fit_dec <- decompose(s,type = 'addit')
fit_hol <- HoltWinters(s, beta=FALSE, gamma=FALSE)

pdf(file = '../img/ExampleOfstl.pdf',width = 10, height = 8)
par(mfrow=c(2,1), mar=c(5,4,2,1.5) + 0.1, cex.lab = 1.3)
plot(fit_stl)
plot(s, col = 'black', type = 'l')
lines(fit_stl$time.series[,1] + fit_stl$time.series[,2], type = 'l', col = 'red')
dev.off()

plot(s, col = 'black', type = 'l')
#lines(fit_dec$seasonal + fit_dec$trend, type = 'l', col = 'blue', lwd = 2)
lines(fit_stl$time.series[,1] + fit_stl$time.series[,2], type = 'l', lty = 2, col = 'red', lwd = 2)
#lines(fit_hol$fitted[,1], type = 'l', col = 'green', lwd = 2)


fit_stl <- stl(s, s.window=7)
fit_dec <- decompose(s,type = 'addit')
plot(fit_stl, range.bars = T)
plot(fit_dec)
plot(s)
lines(fit_stl$time.series[,1], col = 'red', lwd = 2)
lines(fit_dec$seasonal, col = 'blue', lwd = 2)

plot(s)
lines(fit_stl$time.series[,1], col = 'red', lwd = 2)
lines(fit_dec$seasonal, col = 'blue', lwd = 2)

plot(s)
lines(fit$time.series[,2], col = 'red', lwd = 3)

s <- dat[day >= 151 & day < 242,`1032`][1:(48*7*5)]
s <-  ts(s, frequency=48*7)
f <- decompose(s)
plot(f)



fit <- arima(s, order=c(1,0,0), list(order=c(2,1,0), period=12))
fore <- predict(fit, n.ahead=54)
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
ts.plot(s, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"),
       col=c(1,2,4), lty=c(1,1,2))