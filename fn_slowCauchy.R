fn_slowCauchy <- function(iN, iT, dSigma){
  
  mX <- matrix(iN*iT, nrow = iT, ncol = iN)
  
  for(i in 1:iN){
    for(j in 1:iT){
      U <- runif(1)
      mX[j, i] <- tan( pi*(U - 0.5) ) * dSigma
    }
  }
  
  return(mX)
  
}