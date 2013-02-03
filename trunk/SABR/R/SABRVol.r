# SABR volatility (eq. 2.17a in Hagan et al. [2002])
# based on: alpha (a), rho (r), nu (v), beta (b),
# ATM forward (f), strike (K) and expiry (t).
SABRVol <- function(a, r, v, b, f, K, t){
  # Defining "building blocks" for the bigger equation:
  x <- function(z,r){
    log((sqrt(1 - 2*r*z + z^2) + z - r)/(1 - r))
  }
  z <- v/a * (f*K)^((1 - b)/2)*log(f/K)
  Denom <- (f*K)^((1 - b)/2)*
    (1 +
       (1 - b)^2/24*log(f/K)^2 + # 0 if f==K
       (1 - b)^4/1920 *log(f/K)^4 # 0 if f==K
    )
  Term3 <- (1 + ((1 - b)^2/24*a^2/((f*K)^(1 - b)) +
                   (1/4)*(r*b*v*a)/((f*K)^((1 - b)/2)) +
                   (2 - 3*r^2)/24*v^2)*t
  )
  # If f==K then z/x(z,r) = 0/0 => set equal to 1.
  # Term2 <- if(f==K){
  # 1
  # }else{
  # z/x(z,r)
  # }
  # Rewritten to work with vectors of strikes (K??s):
  Term2 <- rowSums(cbind(is.na(z/x(z,r)),
                         (1 - is.na(z/x(z,r)))*(z/x(z,r))), na.rm=TRUE)
  # Putting the pieces together and returning the result
  return( (a/Denom) * Term2 * Term3 )
}