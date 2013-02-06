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
    ResErr=Y[t]-pH%*%Xp             # measurement residual error
    ResCov=pH%*%Vp%*%t(pH)+pR  # measurement residual covariance
    mInvResCov=ginv(ResCov)
    K=Vp%*%t(pH)%*%mInvResCov # kalman gain
    Xf=Xp+K%*%ResErr         # filtered state estimate
    Vf=(mI-K%*%pH)%*%Vp            # filtered state covariance
    ll = ll - 1/2*log(2*pi)-1/2*log(det(ResCov)) -
      1/2*t(ResErr)%*%mInvResCov%*%ResErr
    if (t<N) {
      Xp=pF%*%Xf;
      Vp=pF%*%Vf%*%pF+pQ;
    }
  }
  return(ll)
}
# kalman filter likelihood function
# time invariant
kalman.ll2<-function(pF, pH, pQ, pR, pX0, pV0, Y){
  N=dim(Y)[1] # number of observations
  #initial values
  Xp=pX0  # initial state prediction
  Vp=pV0  # initial state covariance
  ll = 0
  require(MASS)
  require(expm)
  mI = diag(1,dim(pQ)[1],dim(pQ)[2])
  for(t in 1:N) {
    ResErr=Y[t]-pH%*%Xp             # measurement residual error
    ResCov=pH%*%Vp%*%t(pH)+pR%^%2  # measurement residual covariance
    mInvResCov=ginv(ResCov)
    K=Vp%*%t(pH)%*%mInvResCov # kalman gain
    Xf=Xp+K%*%ResErr         # filtered state estimate
    Vf=(mI-K%*%pH)%*%Vp            # filtered state covariance
    ll = ll - 1/2*log(2*pi)-1/2*log(det(ResCov)) -
      1/2*t(ResErr)%*%mInvResCov%*%ResErr
    if (t<N) {
      Xp=pF%*%Xf;
      Vp=pF%*%Vf%*%pF+pQ%^%2;
    }
  }
  return(ll)
}

kalman.smooth<-function(par,Y){
  mu0=par[1];
  sigma0=par[2];
  sigma_e=par[3];
  sigma_iota=par[4];
  N=length(Y);
  #initial value
  mu_filt=array(data=0,dim=N)
  sigma_filt=array(data=0,dim=N)
  mu_pred=array(data=0,dim=N)
  sigma_pred=array(data=0,dim=N)
  mu_pred[1]=mu0
  v=array(data=0,dim=N)
  V=array(data=0,dim=N)
  K=array(data=0,dim=N)
  sigma_pred[1]=sigma0
  for (t in 1:N) {
    v[t]=Y[t]-mu_pred[t];
    V[t]=sigma_pred[t]+sigma_e^2;
    K[t]=sigma_pred[t]/V[t];
    mu_filt[t]=mu_pred[t]+K[t]*v[t];
    sigma_filt[t]=sigma_pred[t]*(1-K[t]);
    if (t<N){
      mu_pred[t+1]=mu_filt[t];
      sigma_pred[t+1]=sigma_filt[t]+sigma_iota^2;
    }
  }
  #q
  q=array(data=0,dim=N)
  q[t]=v[t]/V[t];
  for(t in ((N-1):1)) {
    q[t]=q[t+1]*(1-K[t])+v[t]/V[t];
  }
  tmp = array(data=0,dim=N)
  for(t in 1:N) {
    tmp[t]=mu_pred[t]+sigma_pred[t]*q[t];
  }
  return(tmp)
}

kalman.filter<-function(par,Y){
  mu0=par[1];
  sigma0=par[2];
  sigma_e=par[3];
  sigma_iota=par[4];
  N=length(Y);
  #initial value
  mu_filt=array(data=0,dim=N)
  sigma_filt=array(data=0,dim=N)
  mu_pred=array(data=0,dim=N)
  sigma_pred=array(data=0,dim=N)
  mu_pred[1]=mu0
  v=array(data=0,dim=N)
  for(t in 1:N) {
    v[t]=Y[t]-mu_pred[t];
    Vt=sigma_pred[t]+sigma_e^2;
    K=sigma_pred[t]/Vt;
    mu_filt[t]=mu_pred[t]+K*v[t];
    sigma_filt[t]=sigma_pred[t]*(1-K);
    if(t<N) {
      mu_pred[t+1]=mu_filt[t];
      sigma_pred[t+1]=sigma_filt[t]+sigma_iota^2;
    }
  }
  state=mu_filt;
  onestep=mu_pred;
  return(list(filtered=state,forecasted=onestep))
}
