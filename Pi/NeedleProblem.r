#!/usr/bin/env Rscript
# Thomas Arend
# 2021-07-03
#
# Needle problem to estimate the number pi
# See https://de.wikipedia.org/wiki/Monte-Carlo-Simulation#Probabilistische_Bestimmung_der_Zahl_Pi

# Change seed for different fix results 
# or comment next line for random results
set.seed(42)

n <- 1000
xy <- data.frame (
    x = runif(n)
  , y = runif(n)
  , r2 = rep(0,n)
  , estimated = rep(0,n)
)
xy$r2 <- xy$x^2 + xy$y^2

s = 0
for (i in 1:n) {
  
  if (xy$r2[i] <= 1) {
    s = s + 1
  }
  xy$estimated[i] = s*4/i
}

pi <- xy$estimate[length(xy[,1])]

print (pi)

png( 'NeedleProblem-1.png', width = 1080, height = 1080)
par(  mar = c(10,10,10,10) 
      , bg = rgb(0.95,0.95,0.95,1)
)

plot(  xy[xy$r2<=1,1:2]
     , xlim = c(0,1)
     , ylim = c(0,1)
     , main = "Estimate of Pi"
     , cex.main = 5
     , cex.sub = 3
     , cex.lab = 2
     , col = "red"
     )
par (new=TRUE)

plot(  xy[xy$r2>1,1:2]
       , xlim = c(0,1)
       , ylim = c(0,1)
       , main = ""
       , xlab =""
       , ylab = ""
       , col = "black"
)

title ( sub = paste("π ≈",pi)
        , line = 6
        , cex.sub = 3
)

par ( new=TRUE)

curve( sqrt(1-x^2)
       , from=0
       , to=1
       , col = "green"
       , xlab=""
       , ylab="")

dev.off()

png( 'NeedleProblem-2.png', width = 1080, height = 1080)
par(  mar = c(10,10,10,10) 
      , bg = rgb(0.95,0.95,0.95,1)
)


plot(  1:n
       , xy$estimated
       , type = 'l'
       , xlab = 'Round'
       , ylab = 'Estimate after round n'
       , xlim= c(0,n)
       , ylim= c(0,ceiling(max(xy$estimated)))
       , main = "Estimate of Pi"
       , cex.main = 5
       , cex.sub = 3
       , cex.lab = 2
)
title ( sub = paste("π ≈",pi)
        , line = 6
        , cex.sub = 3
)

dev.off()
