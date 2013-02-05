kalman.ll<-function(p, Y){
  mu0=p[1]; sigma0=p[2];
  sigma_e=p[3]; sigma_iota=p[4];
  c=0;
  N=length(Y)
  #initial value
  mu_filt=array(data=0,dim=N)
  sigma_filt=array(data=0,dim=N)
  mu_pred=array(data=0,dim=N)
  sigma_pred=array(data=0,dim=N)
  mu_pred[1]=mu0
  sigma_pred[1]=sigma0
  ll = 0
  for(t in 1:N) {
    vt=Y[t]-mu_pred[t]
    Vt=sigma_pred[t]+sigma_e^2
    K=sigma_pred[t]/Vt
    mu_filt[t]=mu_pred[t]+K*vt
    sigma_filt[t]=sigma_pred[t]*(1-K)
    ll = ll - 1/2*log(2*pi)-1/2*log(Vt) -1/2*vt^2/Vt
    if (t<N) {
      mu_pred[t+1]=mu_filt[t];
      sigma_pred[t+1]=sigma_filt[t]+sigma_iota^2;
    }
  }
  return(-ll)
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
