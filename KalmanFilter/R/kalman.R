# kalman filter likelihood function
# time invariant
kalman.ll<-function(pF, pH, pQ, pR, pX0, pV0, Y){
  N=length(Y)
  #initial value
  Xf=array(data=0,dim=N) # filtered state estimate (after measurement update)
  Vf=array(data=0,dim=N) # filtered state covariance
  Xp=array(data=0,dim=N) # predicted state estimate
  Vp=array(data=0,dim=N) # predicted state covariance
  Xp[1]=pX0    # initial state prediction
  Vp[1]=pV0  # initial state covariance
  ll = 0
  for(t in 1:N) {
    ResErr=Y[t]-pH%*%Xp[t] # measurement residual error
    ResCov=Vp[t]+pR        # measurement residual covariance
    K=Vp[t]%*%t(pH)%*%ginv(ResCov) # kalman gain
    Xf[t]=Xp[t]+K%*%ResErr         # filtered state estimate
    Vf[t]=(1-K)%*%Vp[t]            # filtered state covariance
    ll = ll - 1/2*log(2*pi)-1/2*log(ResCov) -1/2*ResErr^2/ResCov
    if (t<N) {
      Xp[t+1]=mu_filt[t];
      Vp[t+1]=sigma_filt[t]+sigma_iota^2;
    }
  }
  return(-ll)
}