# kalman filter likelihood function
# time invariant
kalman.ll<-function(pF, pH, pQ, pR, pX0, pV0, Y){
  N=dim(Y)[1] # number of observations
  #initial values
  Xp=pX0  # initial state prediction
  Vp=pV0  # initial state covariance
  ll = 0
  require(MASS)
  mI = diag(1,dim(pQ)[1],dim(pQ)[2])
  for(t in 1:N) {
    ResErr=Y[t]-pH%*%Xp # measurement residual error
    ResCov=pH%*%Vp%*%t(pH)+pR%*%pR        # measurement residual covariance
    print(ResCov)
    mInvResCov=ginv(ResCov)
    K=Vp%*%t(pH)%*%mInvResCov # kalman gain
    Xf=Xp+K%*%ResErr         # filtered state estimate
    Vf=(mI-K%*%pH)%*%Vp            # filtered state covariance
    ll = ll - 1/2*log(2*pi)-1/2*log(det(ResCov)) -1/2*t(ResErr)%*%mInvResCov%*%ResErr
    if (t<N) {
      Xp=pF%*%Xf;
      Vp=pF%*%Vf%*%pF+pQ%*%pQ;
    }
  }
  return(ll)
}