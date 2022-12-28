fn_estimateScale <- function(mX){
  
  
  vSigma <- rep(NA, dim(mX)[2])
  
  # Find absolute values 
  mX[mX < 0] <- -mX[mX < 0]
  mX <- apply(mX, 2, sort)
  
  if(dim(mX)[1] %% 2 == FALSE){ # Even number
    vSigma <- (mX[dim(mX)[1]/2, ] + mX[dim(mX)[1]/2 + 1, ]) / 2
  } else{ # Odd number
    vSigma <- mX[dim(mX)[1]/2, ]
  }

  return(vSigma)
  
}

