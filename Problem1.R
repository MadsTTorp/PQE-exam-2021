## -----------------------------------------------------------------------
## Filename:                      Problem1.R
## 
## Title:                         Examination in PQE 2021 pt 1
##
## Description:                   In the following you find my answer
##                                in regards of the first part of the 
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
## library(Rcpp)
## library(RcppArmadillo)

## -----------------------------------------------------------------------
## Work directory
## Direction to functions
wdSET <- "/Applications/Oecon/6. Semester/Programmering/Old exams/Exam2021Solution"
## Initiate
setwd(wdSET)
##
## -----------------------------------------------------------------------
## Problem 1
source("fn_slowCauchy.R")
iN <- 5000
iT <- 1000
dSigma <- 2.0
set.seed(134)
mX <- fn_slowCauchy(iN, iT, dSigma)

## -----------------------------------------------------------------------
## Problem 2
source("fn_fastCauchy.R")
set.seed(134)
mY <- fn_fastCauchy(iN, iT, dSigma)

all.equal(mX, mY)

## -----------------------------------------------------------------------
## Problem 3
source("fn_estimateScale.R")
vSigmaHat <- fn_estimateScale(mX)

## -----------------------------------------------------------------------
## Problem 4
hist(vSigmaHat, breaks = 50, freq = F, main = "Distribution of estimates")
lines(seq(range(vSigmaHat)[1], range(vSigmaHat)[2], length.out = length(vSigmaHat)), 
      dnorm(seq(range(vSigmaHat)[1], range(vSigmaHat)[2], length.out = length(vSigmaHat)),
            mean = mean(vSigmaHat), sd = sd(vSigmaHat)))

## -----------------------------------------------------------------------
## Problem 5
source("fn_findMax.R")

g  <- function(dX) -0.2*dX^3 + 3*dX^2 + 5*dX - 3
gp <- function(dX) -0.6*dX^2 + 6*dX + 5

fn_findMax(g, gp)






