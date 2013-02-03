
alpha4 <- 0.00677262
beta4 <- 0
nu4 <- 0.284698
rho4 <- 0.10002
vols4 <- SABRVol(alpha4, rho4, nu4, beta4, f, Kx, T)

windows()
plot(Kx, vols4,type="l",col=3, lty = 1, xlab = "K", ylab = "Implied Vols", ylim = c(0,0.5))
points(K, obs_vols, pch = 2, col = 4)
title("SABR Fitting")
legend(0.045, .38, c(expression("Fitted " ~ beta ~ " = 1"), "Observed"), col = c(3,4),
       text.col = "green4", lty = c(1,-1), pch = c(-1,2),
       merge = FALSE, bg = 'gray90')