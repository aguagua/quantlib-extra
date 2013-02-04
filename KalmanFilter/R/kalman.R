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
# 
# kalman.filter<-function(p, X){
#   print(p)
#   bX=p[1]; bY=p[2]
#   vX=p[3]^2; vY=p[4]^2;
# #  vX=exp(p[3]); vY=exp(v[4])
#   m0=p[5]; v0=p[6]
# #  m0=0
# #  v0=vY/(1-bY^2)
#   N<-length(X)
#   m<-vector("double",N)
#   v<-vector("double",N)
#   ll<-0
#   for(t in 1:N){
#     #likelihood evaluation
#     m1=bX*m0
#     v1=bX^2*v0^2+vX;
#     if(v1<0) browser()
#     ll=ll+log(v1)+(X[t]-m1)/v1
#     #correction
#     K=v0*bX/(v0*bX^2+vX)
#     m[t]=m0+K*(X[t]-bX*m0)
#     v[t]=v0-K*v0*bX
#     # prediction step
#     m0=bY*m[t]
#     v0=v[t]*bY^2+vY
#   }
#   return(list(ll=ll,m=m,v=v));
# }
# 
# kalman.filter2<-function(p, X) {
# #   CX=p$CX; BX=p$BX; SE=p$SE #observations
# #   CY=p$CY; BY=p$BY; SH=p$SH #factors
# #   MC=p$M0; VC=p$V0
#   CX=p[1]; BX=p[2]; SE=p[3]
#   CY=p[4]; BY=p[5]; SH=p[6]
#   MC=p[7]; VC=p[8]
#   N=length(X)
#   M=matrix(data=0,nrow=N,ncol=length(SH));
#   S=M; L=0
#   for(t in 1:N) {
#     # prediction
#     MP=CY+BY*MC #update mean
#     VP=BY*VC*t(BY) #update variance
#     # likelihood eval
#     Mx=CX+BX*MP
#     Vx=BX*VP*t(BX)+SE
#     Xt=X[t]-Mx
#     L = L + log(Vx)+t(Xt)/Vx*Xt
#     #correction
#     K=VP*t(BX)/Vx #kalman gain
#     MC=MP+K*(X[t]-CX-BX*MP)#update mean
#     VC=VP-K*BX*VP#update variance
#     #stor
#     M[t,]=t(MC)
#     S[t,]=sqrt(VC)
#   }
#   return(list(ll=L,m=M))
# }