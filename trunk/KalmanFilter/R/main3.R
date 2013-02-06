rm(list=ls())

require(expm)
source("kalman.R")
read.csv("data.csv")->raw
raw<-log(raw)
Y=matrix(raw[,2])
p0=c(0,(100000),(1),(1))

#debug(kalman.ll)
build<-function(p) {
  pF=matrix(1); pH = matrix(1); 
  pX0=matrix(p[1]); 
  pV0=(matrix(p[2]));
  pR=(matrix(p[3])); 
  pQ=(matrix(p[4])); 
  
  kalman.ll2(pF, pH, pQ, pR, pX0, pV0, Y)->res
  return(-res)
}
#res1<-nlm(build,p0)
#print(c(res1$estimate[1],exp(res1$estimate[-1])))
res2<-optim(p0,build)
sm2<-kalman.smooth(res2$par,Y)
kalman.filter(res2$par,Y)->ff2
x11()
plot(sm2,type="l",ylim=c(0,2),main="Optim",ylab="State")
par(new=T)
plot(ff2$filtered,type="l",ylim=c(0,2),col="red",main="",ylab="")

x11()
plot(Y,type="l",xlim=c(0,350),ylim=c(-2.2,2.3),main="Y~filtered~forecast",ylab="State")
par(new=T)
plot(ff2$filtered,type="l",col="red", xlim=c(0,350),ylim=c(-2.2,2.3),main="",ylab="")
par(new=T)
plot(sm2,type="l",col="blue", xlim=c(0,350),ylim=c(-2.2,2.3),main="",ylab="")
par(new=T)
plot(x=2:341,y=ff2$forecasted,type="l",col="green",xlim=c(0,350),ylim=c(-2.2,2.3),main="",ylab="")
