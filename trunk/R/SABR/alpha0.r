alpha0 <- function(b, r, v, vol, f, t){
  # Find root of CubPoly in [0,1]:
  uniroot(CubPoly, interval=c(0,1),
          # Additional parameters for ??CubPoly??:
          b=b, r=r, v=v, vol=vol, fwd=f, t=t)$root
}