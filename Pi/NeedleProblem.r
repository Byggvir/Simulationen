#!/usr/bin/env Rscript
# Thomas Arend
# 2021-07-03
#
# Needle problem to estimate the number pi
# See https://de.wikipedia.org/wiki/Monte-Carlo-Simulation#Probabilistische_Bestimmung_der_Zahl_Pi

set.seed(42)

n <- 10000
xy <- data.frame (
    x = runif(n)
  , y = runif(n)
)

h <- xy$x^2 + xy$y^2

pi <- length(h[h<=1])*4/n

print (pi)
png( 'NeedleProblem.png', width = 1080, height = 1080)

plot(  xy
     , xlim= c(0,1)
     , ylim= c(0,1)
     , main = "Estimate of Pi"
     , sub = paste("π =",pi)
     )
par ( new=TRUE)
curve( sqrt(1-x^2)
       , from=0
       , to=1
       , col = "red"
       , xlab=""
       , ylab="")

dev.off()
