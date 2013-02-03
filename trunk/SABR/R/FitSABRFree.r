# Function that fits the SABR model based on:
# beta (b), ATM forward (fATM), observed volatilities (ObsVols)
# with corresponding strikes (strikes) and maturity of the
# swaption (t).
# All parameters (alpha, rho and nu) are free.
FitSABRFree <- function(beta, fATM, strikes, t, ObsVols,
                        init.values = c(0.03, -0.3, 0.3),
                        lower.bound = c(0.00, -1, 0.001),
                        upper.bound = c(1, 1, 1), ...){
  # Defining function to be minimized:
  obj <- function(parm, beta, fATM, strikes, t, ObsVols){
    alpha <- parm[1]
    rho <- parm[2]
    nu <- parm[3]
    EstVols <- SABRVol(a=alpha, r=rho, v=nu,
                       b=beta, f=fATM, K=strikes, t=t)
    return(sum((EstVols - ObsVols)^2))
  }
  
  opt <- nlminb(start=init.values,
                objective = obj,
                lower = lower.bound,
                upper = upper.bound,
                # Additional parameters for ??obj??:
                beta = beta,
                fATM = fATM,
                strikes = strikes,
                t = t,
                ObsVols = ObsVols)
  
  # Printing information regarding optimization:
  cat("Error in convergence: ",opt$convergence, "\n")
  cat(opt$message, "\n")
  cat("Iterations: ",opt$iterations, "\n\n")
  # Storing optimization results:
  parms <- opt$par
  names(parms) <- c("alpha", "rho", "nu")
  obj <- opt$objective
  estvols <- SABRVol(a=parms[1], r=parms[2], v=parms[3],
                     b=beta, f=fATM, K=strikes, t=t)
  # Returning results (in list form):
  return(list(parms=parms, estvols=estvols, obsvols=ObsVols, obj=obj))
}