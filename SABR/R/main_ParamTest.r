# main test routine for SABR
# this script tests the effect of adjusting different SABR parameters
source("SABRVol.r")

nu = .30; alpha = .03; rho = -.30; beta = .60; f = .025; maturity = 1;
K <- seq(.01, .06, by=.002)

f1 = .025
vols1 <- SABRVol(alpha, rho, nu, beta, f1, K, maturity)
f2 = .015
vols2 <- SABRVol(alpha, rho, nu, beta, f2, K, maturity)
f3 = .035
vols3 <- SABRVol(alpha, rho, nu, beta, f3, K, maturity)

png("shift in f.png", 800, 600)
plot(K, vols1, type = "l", col = 1, lty = 1, 
     xlim = c(0.008,0.06), ylim = c(0.1,0.3),
     xlab = "K", ylab = expression(sigma[B] ~ ""))
lines(K, vols2, col = 1, lty = 2)
lines(K, vols3, col = 1, lty = 3)
title(main = "Shift in f")
legend(0.045, .30, 
       c(expression("f = .025"), expression("f = .015"), expression("f = .035")), 
       col = c(1,1,1), text.col = "green4", lty = c(1,2,3),
       merge = FALSE, bg = 'gray90')
dev.off()

nu = .30; alpha = .03; rho = -.30; beta = .60; f = .025; maturity = 1;
K <- seq(.01, .06, by=.002)
nu = .30
vols1 <- SABRVol(alpha, rho, nu, beta, f, K, maturity)
nu = .10
vols2 <- SABRVol(alpha, rho, nu, beta, f, K, maturity)
nu = .50
vols3 <- SABRVol(alpha, rho, nu, beta, f, K, maturity)


png("shift in nu.png", 800, 600)
plot(K, vols1, type = "l", col = 1, lty = 1, 
     xlim = c(0.008,0.06), ylim = c(0.1,0.3),
     xlab = "K", ylab = expression(sigma[B] ~ ""))
lines(K, vols2, col = 1, lty = 2)
lines(K, vols3, col = 1, lty = 3)
title(main = expression("Shift in " ~ nu ~ ""))
legend(0.045, .30, 
       c(expression(nu ~ " = .30"), expression(nu ~ " = .10"), expression(nu ~ " = .50")), 
       col = c(1,1,1), text.col = "green4", lty = c(1,2,3),
       merge = FALSE, bg = 'gray90')
dev.off()

nu = .30; alpha = .03; rho = -.30; beta = .60; f = .025; maturity = 1;
K <- seq(.01, .06, by=.002)
alpha = .03
vols1 <- SABRVol(alpha, rho, nu, beta, f, K, maturity)
alpha = .01
vols2 <- SABRVol(alpha, rho, nu, beta, f, K, maturity)
alpha = .05
vols3 <- SABRVol(alpha, rho, nu, beta, f, K, maturity)

png("shift in alpha.png", 800, 600)
plot(K, vols1, type = "l", col = 1, lty = 1, 
     xlim = c(0.008,0.06), ylim = c(0.0,0.4),
     xlab = "K", ylab = expression(sigma[B] ~ ""))
lines(K, vols2, col = 1, lty = 2)
lines(K, vols3, col = 1, lty = 3)
legend(0.045, .40, 
       c(expression(alpha ~ " = .03"), expression(alpha ~ " = .01"), expression(alpha ~ " = .05")), 
       col = c(1,1,1), text.col = "green4", lty = c(1,2,3),
       merge = FALSE, bg = 'gray90')
title(main = expression("Shift in " ~ alpha ~ ""))
dev.off()


nu = .30; alpha = .03; rho = -.30; beta = .60; f = .025; maturity = 1;
K <- seq(.01, .06, by=.002)
rho = -.30
vols1 <- SABRVol(alpha, rho, nu, beta, f, K, maturity)
rho = -.80
vols2 <- SABRVol(alpha, rho, nu, beta, f, K, maturity)
rho = .80
vols3 <- SABRVol(alpha, rho, nu, beta, f, K, maturity)

png("shift in rho.png", 800, 600)
plot(K, vols1, type = "l", col = 1, lty = 1, 
     xlim = c(0.008,0.06), ylim = c(0.05,0.3),
     xlab = "K", ylab = expression(sigma[B] ~ ""))
lines(K, vols2, col = 1, lty = 2)
lines(K, vols3, col = 1, lty = 3)
legend(0.045, .30, 
       c(expression(rho ~ " = -.30"), expression(rho ~ " = -.80"), expression(rho ~ " = .80")), 
       col = c(1,1,1), text.col = "green4", lty = c(1,2,3),
       merge = FALSE, bg = 'gray90')
title(main = expression("Shift in " ~ rho ~ ""))
dev.off()

nu = .30; alpha = .03; rho = -.30; beta = .60; f = .025; maturity = 1;
K <- seq(.01, .06, by=.002)
beta = .60
vols1 <- SABRVol(alpha, rho, nu, beta, f, K, maturity)
beta = .40
vols2 <- SABRVol(alpha, rho, nu, beta, f, K, maturity)
beta = .80
vols3 <- SABRVol(alpha, rho, nu, beta, f, K, maturity)
beta = .0
vols4 <- SABRVol(alpha, rho, nu, beta, f, K, maturity)

png("shift in beta.png", 800, 600)
plot(K, vols1, type = "l", col = 1, lty = 1, 
     xlim = c(0.008,0.06), ylim = c(0.0,.5),
     xlab = "K", ylab = expression(sigma[B]))
lines(K, vols2, col = 1, lty = 2)
lines(K, vols3, col = 1, lty = 3)
legend(0.045, .50, 
       c(expression(beta ~ " = .60"), expression(beta ~ " = .40"), expression(beta ~ " = .80")), 
       col = c(1,1,1), text.col = "green4", lty = c(1,2,3),
       merge = FALSE, bg = 'gray90')
title(main = expression("Shift in " ~ beta ~ ""))
dev.off()