fn_findMax <- function(fg, fgp, dTol = 0.00001){
  
  # fg: function g(x)
  # fgp: function g(x) derivative (g.prime)
  # dTol: Tolerance level in terms of optimum
  
  dX <- 0
  iter <- 0
  gX <- fgp(dX)
  
  while( abs(gX) > dTol ){
 
  dX <- dX + sign(fgp(dX)) / sqrt(iter + 1)
  gX <- fgp(dX)
  iter <- iter + 1
  
  }
  
  lOut <- list(  "x"          = dX,
                 "gx"         = fg(dX),
                 "iterations" = iter )
  
  return(lOut)
}