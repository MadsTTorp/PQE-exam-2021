## -----------------------------------------------------------------------
## Filename:                      Problem2.R
## 
## Title:                         Examination in PQE 2021 pt 2
##
## Description:                   In the following you find my answer
##                                in regards of the second part of the 
##                                written exam in PQE 2021. I will make
##                                use of different scripts when defining
##                                functions.
##                               
## Flow No:                        xXx
##
## Date (created):                04/08/21
## -----------------------------------------------------------------------
## Housekeeping

rm(list = ls()) # Clear Environment
cat("\014")     # Clear Console
graphics.off()  # Clear Plots

## -----------------------------------------------------------------------
## Library

## library(microbenchmark)
   library(Rcpp)
   library(RcppArmadillo)

## -----------------------------------------------------------------------
## Work directory
## Direction to functions
wdSET <- "/Applications/Oecon/6. Semester/Programmering/Old exams/Exam2021Solution"
## Initiate
setwd(wdSET)
##
## -----------------------------------------------------------------------
## Problem 1
df <- readRDS("mData.R")

## -----------------------------------------------------------------------
## Problem 2
source("NegAvgLL.R")

## -----------------------------------------------------------------------
## Problem 3
sA <- sd(df[ , 1])
sB <- sd(df[ , 2])
vPar <- c(sA * 0.05, 0.05, 0.90,
          sB * 0.05, 0.05, 0.90,
          0)
optim(vPar, NegAvgLL, mX = df, method = "BFGS")
# Cannot run due to missing constraints

## -----------------------------------------------------------------------
## Problem 4
source("NegAvgLL.t.R")

## -----------------------------------------------------------------------
## Problem 5
theta_trans <- function(x){
  return(c(log(x[1]), log(x[2:3]/(1-x[2:3])),log(x[4]), log(x[5:6]/(1-x[5:6])), log((1+x[7])/(1-x[7])) ) )
}

Fit.t <- optim(theta_trans(vPar), NegAvgLL.t, mX = df, method = "BFGS")
print(Fit.t$par)
# Estimates are:
# OmegaA = -13.07
# AlphaA = -3.01
# BetaA  = 2.91
# OmegaB = -11.66
# AlphaB = -3.01
# BetaB  = 2.44
# Rho    = 0.28

## -----------------------------------------------------------------------
## Problem 6
sourceCpp("TransformC.cpp")
round(TransformC(Fit.t$par),2) # Estimate of rho is 0.14



