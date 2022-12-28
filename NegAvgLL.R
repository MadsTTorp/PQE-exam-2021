NegAvgLL <- function(vPar, mX){
  
  if( dim(mX)[2] != 2 ){
    warning("matrix has a column dimensiion different from 2")
  }
  
  # Initialize inputs 
  dOmegaA <- vPar[1]
  dAlphaA <- vPar[2]
  dBetaA  <- vPar[3]
  dOmegaB <- vPar[4]
  dAlphaB <- vPar[5]
  dBetaB  <- vPar[6]
  dRho    <- vPar[7]
  
  # Initiate setup
  iT          <- dim(mX)[1]
  mSigma      <- matrix(NA, iT, 2)
  mSigma[1,1] <- dOmegaA / ( 1 - dAlphaA - dBetaA )
  mSigma[1,2] <- dOmegaB / ( 1 - dAlphaB - dBetaB )
  
  vLLK <- matrix(NA, iT-1)
  
  for(t in 2:iT){
    mSigma[t, 1] <- dOmegaA + dAlphaA * mX[t-1 , 1]^2 + dBetaA * mSigma[t-1, 1]
    mSigma[t, 2] <- dOmegaB + dAlphaB * mX[t-1 , 2]^2 + dBetaB * mSigma[t-1, 2]
    
    
    SIGMA      <- matrix(c(mSigma[t, 1], dRho*sqrt(mSigma[t, 1]*mSigma[t, 2]), 
                           dRho*sqrt(mSigma[t, 1]*mSigma[t, 2]), mSigma[t, 2]), 
                         2, 2)
    
    vLLK[t-1]       <- -0.5 * (log(det( abs(SIGMA) )) + mX[t, ] %*% solve(SIGMA) %*% mX[t, ])
  }
  
  dOut <- -sum(vLLK) / iT
  return(dOut)
  
}


