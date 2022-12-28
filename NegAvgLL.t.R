NegAvgLL.t <- function(vPar, mX){
  
  if( dim(mX)[2] != 2 ){
    warning("matrix has a column dimensiion different from 2")
  }

  # Initialize inputs 
  dOmegaA.t <- exp(vPar[1])                             # > 0
  dAlphaA.t <- exp(vPar[2]) / (1 + exp(vPar[2]))        # (0,1)
  dBetaA.t  <- exp(vPar[3]) / (1 + exp(vPar[3]))        # (0,1)
  dOmegaB.t <- exp(vPar[4])                             # > 0
  dAlphaB.t <- exp(vPar[5]) / (1 + exp(vPar[5]))        # (0,1)
  dBetaB.t  <- exp(vPar[6]) / (1 + exp(vPar[6]))        # (0,1)
  dRho.t    <- 2 * (exp(vPar[7]) / (1 + exp(vPar[7])))  - 1  # (-1,1)
  
  # Initiate setup
  iT          <- dim(mX)[1]
  mSigma      <- matrix(0, iT, 2)
  mSigma[1,1] <- dOmegaA.t / ( 1 - dAlphaA.t - dBetaA.t )
  mSigma[1,2] <- dOmegaB.t / ( 1 - dAlphaB.t - dBetaB.t )
  
  vLLK <- matrix(0, iT-1)
  
  for(t in 2:iT){
    mSigma[t, 1] <- dOmegaA.t + dAlphaA.t * mX[t-1 , 1]^2 + dBetaA.t * mSigma[t-1, 1]
    mSigma[t, 2] <- dOmegaB.t + dAlphaB.t * mX[t-1 , 2]^2 + dBetaB.t * mSigma[t-1, 2]
    
    
    SIGMA      <- matrix(c(mSigma[t, 1], dRho.t*sqrt(mSigma[t, 1]*mSigma[t, 2]), 
                           dRho.t*sqrt(mSigma[t, 1]*mSigma[t, 2]), mSigma[t, 2]), 
                           2, 2)
    
    vLLK[t-1]       <- -0.5 * log( det( SIGMA ) ) - 0.5 * (mX[t, ]) %*% solve(SIGMA) %*% (mX[t, ]) 
  }
  
  return(- sum(vLLK) / iT)
  
}


