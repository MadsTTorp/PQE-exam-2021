fn_fastCauchy <- function(iN, iT, dSigma){
  
  size <- iT
  
  U <- matrix(runif(iN*iT), nrow = iT, ncol = iN)
  
  mY <- matrix(iN*iT, nrow = iT, ncol = iN)
  
  mY[ , 1:iN] <- tan( pi*(U[ , 1:iN] - 0.5)) * dSigma
  
  return(mY)
  
}

