// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp; 
using namespace arma; 

// [[Rcpp::export]] 
NumericVector TransformC(NumericVector Par){
  
  double OmegaA = exp(Par(0));
  double AlphaA = exp(Par(1))/(1 + exp(Par(1)));
  double BetaA  = exp(Par(2))/(1 + exp(Par(2)));
  double OmegaB = exp(Par(3));
  double AlphaB = exp(Par(4))/(1 + exp(Par(4)));
  double BetaB  = exp(Par(5))/(1 + exp(Par(5)));
  double Rho    = exp(Par(6)) / (1 + exp(Par(6))) * 2 - 1;
  
  NumericVector out(Par.size());
  out(0) = OmegaA;
  out(1) = AlphaA;
  out(2) = BetaA;
  out(3) = OmegaB;
  out(4) = AlphaB;
  out(5) = BetaB;
  out(6) = Rho;
  
  return out;
  
}