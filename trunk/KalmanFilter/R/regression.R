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