# main test routine for SABR
# calibrating SABR model to swaption market implied vols
# this script uses 2nd approach to calibrate SABR parameters by recovering
# alpha from beta, rho, nu with at the money option by solving a cubic equation
# this removes one degree of freedom in optimization
rm(list=ls())
graphics.off()

source("SABRVol.r")
source("FitSABRImpA.r")
source("FitSABRImpA.r")
source("alpha0.r")
source("CubPoly.r")

# market data, implied vols of swaptions and the strike price respectively
obs_vols <- c(32.15, 24.80, 22.22, 20.40, 19.23, 18.67, 18.87)/100
K <- c(1.571, 2.571, 3.071, 3.571, 4.071, 4.571, 5.571)/100
# at the money quotes
f <- 3.571/100 # forward swap rate
volATM <-20.40/100; # at the money vol
maturity <- 10 # time to maturity of all the swaptions

# in SABR model, typically beta is set by the trader, depending on different
# market. popular choices are 0, 1/2, 1
# and thus we are trying to calibrate alpha, rho, nu
# we illustrate all three scenarios
beta1 <- 1
res1 <- FitSABRImpA(beta1, f, volATM, K, maturity, obs_vols)
alpha1 <- res1$parms[1]
rho1 <- res1$parms[2]
nu1 <- res1$parms[3]

beta2 <- .5
res2 <- FitSABRImpA(beta2, f, volATM, K, maturity, obs_vols)
alpha2 <- res2$parms[1]
rho2 <- res2$parms[2]
nu2 <- res2$parms[3]

beta3 <- 0
res3 <- FitSABRImpA(beta3, f, volATM, K, maturity, obs_vols)
alpha3 <- res3$parms[1]
rho3 <- res3$parms[2]
nu3 <- res3$parms[3]

# build vol curves from calibrated parameters
Kx <- seq(.01, .06, by=.002)
vols1 <- SABRVol(alpha1, rho1, nu1, beta1, f, Kx, maturity)
vols2 <- SABRVol(alpha2, rho2, nu2, beta2, f, Kx, maturity)
vols3 <- SABRVol(alpha3, rho3, nu3, beta3, f, Kx, maturity)

plot(Kx, vols1,type="l",col=3, lty = 1, xlab = "K", ylab = "Implied Vols")
lines(Kx, vols2, col=3, lty = 2)
lines(Kx, vols3, col=3, lty = 3)
points(K, obs_vols, pch = 2, col = 4)
title("SABR Fitting")
legend(0.045, .38, c(expression("Fitted " ~ beta ~ " = 1"), 
                     expression("Fitted " ~ beta ~ " = .5"), 
                     expression("Fitted " ~ beta ~ " = 0"), "Observed"), 
       col = c(3,3,3,4), text.col = "green4", lty = c(1,2,3,-1), 
       pch = c(-1,-1,-1,2), merge = FALSE, bg = 'gray90')

