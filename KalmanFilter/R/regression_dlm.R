rm(list=ls())
graphics.off()

library(dlm)
library(PerformanceAnalytics)

data(managers)
# extract HAM1 and SP500 excess returns
HAM1 = 100*(managers[,"HAM1", drop=FALSE] - managers[,"US 3m TR", drop=FALSE])
sp500 = 100*(managers[,"SP500 TR", drop=FALSE] - managers[,"US 3m TR", drop=FALSE])
colnames(sp500) = "SP500"
s2v = 1
s2a = 0.01
s2b = 0.01
tvp.dlm = dlmModReg(X=sp500, addInt=TRUE,
                    dV=s2v, dW=c(s2a, s2b))

# function to build TVP ss model
buildTVP <- function(parm, x.mat){
  parm <- exp(parm)
  return( dlmModReg(X=x.mat, dV=parm[1],
                    dW=c(parm[2], parm[3])) )
}
# maximize over log-variances
start.vals = c(0,0,0)
names(start.vals) = c("lns2v", "lns2a", "lns2b")
TVP.mle = dlmMLE(y=HAM1, parm=start.vals,
                 x.mat=sp500, build=buildTVP,
                 hessian=T)
# get sd estimates
se2 <- sqrt(exp(TVP.mle$par))
names(se2) = c("sv", "sa", "sb")
# fitted ss model
TVP.dlm <- buildTVP(TVP.mle$par, sp500)
# filtering
TVP.f <- dlmFilter(HAM1, TVP.dlm)
# smoothing
TVP.s <- dlmSmooth(TVP.f)

# extract smoothed states - intercept and slope coefs
alpha.s = xts(TVP.s$s[-1,1,drop=FALSE],
              as.Date(rownames(TVP.s$s[-1,])))
beta.s = xts(TVP.s$s[-1,2,drop=FALSE],
             as.Date(rownames(TVP.s$s[-1,])))
colnames(alpha.s) = "alpha"
colnames(beta.s) = "beta"
# extract std errors - dlmSvd2var gives list of MSE matrices
mse.list = dlmSvd2var(TVP.s$U.S, TVP.s$D.S)
se.mat = t(sapply(mse.list, FUN=function(x) sqrt(diag(x))))
se.xts = xts(se.mat[-1, ], index(beta.s))
colnames(se.xts) = c("alpha", "beta")
a.u = alpha.s + 1.96*se.xts[,"alpha"]
a.l = alpha.s - 1.96*se.xts[, "alpha"]
b.u = beta.s + 1.96*se.xts[,"beta"]
b.l = beta.s - 1.96*se.xts[, "beta"]
x11()
layout(matrix(c(1,1,2,2),2,2,byrow=T))
layout.show(2)
chart.TimeSeries(cbind(alpha.s, a.l, a.u), main="Smoothed estimates of alpha",
                 ylim=c(0,1), colorset=c(1,2,2), lty=c(1,2,2),ylab=expression(alpha),xlab="")
chart.TimeSeries(cbind(beta.s, b.l, b.u), main="Smoothed estimates of beta",
                 colorset=c(1,2,2), lty=c(1,2,2),ylab=expression(beta),xlab="")
# forecasting using dlmFilter
# add 10 missing values to end of sample
new.xts = xts(rep(NA, 10),
              seq.Date(from=end(HAM1), by="months", length.out=11)[-1])
HAM1.ext = merge(HAM1, new.xts)[,1]
TVP.ext.f = dlmFilter(HAM1.ext, TVP.dlm)
# extract h-step ahead forecasts of state vector
TVP.ext.f$m[as.character(index(new.xts)),]
