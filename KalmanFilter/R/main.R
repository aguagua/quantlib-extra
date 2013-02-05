rm(list=ls())

require(dlm)
source("kalman.R")
read.csv("data.csv")->raw
raw<-log(raw)
Y=raw[,2]
p0=c(0,100000,1,1)

build<-function(p) {
  kalman.ll(p,Y)->res
  return(res)
}
res1<-nlm(build,p0)
sm1<-kalman.smooth(res1$estimate,Y)
kalman.filter(res1$estimate,Y)->ff1
x11()
plot(sm1,type="l",ylim=c(0,2),main="NLM",ylab="State")
par(new=T)
plot(ff1$filtered,type="l",ylim=c(0,2),col="red",main="",ylab="")

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
