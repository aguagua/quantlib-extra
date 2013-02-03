# Function that fits the SABR model based on beta (b),
# ATM forward (fATM), observed volatilities (ObsVols)
# with corresponding strikes (strikes) and the
# maturity of the swaption (t).
# (Alpha is implied through rho, nu and ATM volatility.)
FitSABRImpA <- function(beta, fATM, volATM, strikes, t, ObsVols,
                        init.values = c(-0.3, 0.3),
                        lower.bound = c(-1, 0.001),
                        upper.bound = c(1, 1), ...){
  obj <- function(parm, beta, fATM, volATM, strikes, t, ObsVols){
    rho <- parm[1]
    nu <- parm[2]
    # Determining alpha based on rho, nu and ATM vol:
    alpha <- alpha0(b=beta, r=rho, v=nu, vol=volATM, f=fATM, t=t)
    EstVols <- SABRVol(a=alpha, r=rho, v=nu, b=beta, f=fATM,
                       K=strikes, t=t)
    return(sum((EstVols - ObsVols)^2))
  }
  
  opt <- nlminb(start=init.values,
                objective = obj,
                lower = lower.bound,
                upper = upper.bound,
                # Additional parameters for ??obj??:
                beta = beta,
                fATM = fATM,
                volATM = volATM,
                strikes = strikes,
                t = t,
                ObsVols = ObsVols)
  
  cat("Error in convergence: ", opt$convergence, "\n")
  cat(opt$message, "\n")
  cat("Iterations: ", opt$iterations, "\n\n")
  parms <- c(alpha0(b=beta, r=opt$par[1], v=opt$par[2],
                    vol=volATM, f=fATM, t=t),opt$par)
  names(parms) <- c("alpha", "rho", "nu")
  obj <- opt$objective
  estvols <- SABRVol(a=parms[1], r=parms[2], v=parms[3],
                     b=beta, f=fATM, K=strikes, t=t)
  # Returning results (in list form):
  return(list(parms=parms, estvols=estvols, obsvols=ObsVols, obj=obj))
}