# main test routine for SABR
setwd("C:\\Users\\Qitian\\Documents\\R\\SABR")
source("SABRVol.r")

windows()

nu = .30; alpha = .03; rho = -.30; beta = .60; f = .025; T = 1;
K <- seq(.01, .06, by=.002)

f1 = .025
vols1 <- SABRVol(alpha, rho, nu, beta, f1, K, T)
f2 = .015
vols2 <- SABRVol(alpha, rho, nu, beta, f2, K, T)
f3 = .035
vols3 <- SABRVol(alpha, rho, nu, beta, f3, K, T)

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
savePlot(filename = "Shift in f",
         type = "tiff", device = dev.cur(), restoreConsole = TRUE)

nu = .30; alpha = .03; rho = -.30; beta = .60; f = .025; T = 1;
K <- seq(.01, .06, by=.002)
nu = .30
vols1 <- SABRVol(alpha, rho, nu, beta, f, K, T)
nu = .10
vols2 <- SABRVol(alpha, rho, nu, beta, f, K, T)
nu = .50
vols3 <- SABRVol(alpha, rho, nu, beta, f, K, T)


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
savePlot(filename = "Shift in nu",
         type = "tiff", device = dev.cur(), restoreConsole = TRUE)

nu = .30; alpha = .03; rho = -.30; beta = .60; f = .025; T = 1;
K <- seq(.01, .06, by=.002)
alpha = .03
vols1 <- SABRVol(alpha, rho, nu, beta, f, K, T)
alpha = .01
vols2 <- SABRVol(alpha, rho, nu, beta, f, K, T)
alpha = .05
vols3 <- SABRVol(alpha, rho, nu, beta, f, K, T)


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
savePlot(filename = "Shift in alpha",
         type = "tiff", device = dev.cur(), restoreConsole = TRUE)


nu = .30; alpha = .03; rho = -.30; beta = .60; f = .025; T = 1;
K <- seq(.01, .06, by=.002)
rho = -.30
vols1 <- SABRVol(alpha, rho, nu, beta, f, K, T)
rho = -.80
vols2 <- SABRVol(alpha, rho, nu, beta, f, K, T)
rho = .80
vols3 <- SABRVol(alpha, rho, nu, beta, f, K, T)

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

savePlot(filename = "Shift in rho",
         type = "tiff", device = dev.cur(), restoreConsole = TRUE)



nu = .30; alpha = .03; rho = -.30; beta = .60; f = .025; T = 1;
K <- seq(.01, .06, by=.002)
beta = .60
vols1 <- SABRVol(alpha, rho, nu, beta, f, K, T)
beta = .40
vols2 <- SABRVol(alpha, rho, nu, beta, f, K, T)
beta = .80
vols3 <- SABRVol(alpha, rho, nu, beta, f, K, T)
beta = .0
vols4 <- SABRVol(alpha, rho, nu, beta, f, K, T)


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
savePlot(filename = "Shift in beta",
         type = "tiff", device = dev.cur(), restoreConsole = TRUE)