# install packages:
library(crossrun)
library(Rmpfr)

# Introduction, figure 1
seqfig <- function(n=24, midline=0, low=.5, high=1.5,
                   figseed=83938487) {
  set.seed(figseed)
  x <- seq(n)
  up <- sample(1:n, size=floor(n/2))
  y <- runif(n, -high, -low)
  y[up] <- runif(length(up), low, high)
  par(mar = c(bottom = 0, left   = 0, top    = 0, right  = 0))
  plot(x, y,
       axes = FALSE,
       type = "b",
       pch  = '',
       lwd  = 1.5,
       ylab = '',
       xlab = '')
  lines(x, rep(midline, n), col = 'grey40')
  text(x, y)
} # end function seqfig
seqfig(n=20, low=.3, high=1)
# save as fig1.pdf, width=5, height=2

# figure 2:
seqfig2 <- function(n=24, f1=15, f2=9, midline=0, low=.5, high=1.5,
                    cex1=3, colf="red", yout=.25,
                    figseed=83938487) {
  x <- seq(n)
  set.seed(figseed+1)
  up1 <- c(1:(f1-1),sample((f1+1):n, size=floor((n+1-f1)/2)))
  y1 <- runif(n, -high, -low)
  y1[up1] <- runif(length(up1), low, high)
  set.seed(figseed+1)
  up2 <- c(1:(f2-1),sample((f2+1):n, size=floor((n+1-f2)/2)))
  y2 <- runif(n, -high, -low)
  y2[up2] <- runif(length(up2), low, high)
  par(mar = c(bottom = 0, left = 0, top = 0, right = 0))
  par(mfrow=c(2,1))
  plot(x, y1,
       axes = FALSE,
       type = "b",
       pch  = '',
       lwd  = 1.5,
       ylab = '', ylim=c(-high-yout, high+yout),
       xlab = '')
  points(x[f1:n], y1[f1:n],
         type = "b",
         pch  = '',
         lwd  = 1.5,
         col=colf)
  text(x=1, y=high+yout/2, pos=4, labels="Case 1")
  points(1,y1[1], pch=1, cex=cex1)
  points(f1,y1[f1], pch=1, cex=cex1, col=colf)
  lines(x, rep(midline, n), col = 'grey40')
  text(x[1:(f1-1)], y1[1:(f1-1)])
  text(x[f1:n], y1[f1:n], labels=f1:n, col=colf)
  plot(x, y2, 
       axes = FALSE,
       type = "b",
       pch  = '',
       lwd  = 1.5,
       ylab = '', ylim=c(-high-yout, high+yout),
       xlab = '')
  text(x=1, y=high+yout/2, pos=4, labels="Case 2")
  points(x[f2:n], y2[f2:n],
         type = "b",
         pch  = '',
         lwd  = 1.5,
         col=colf)
  points(1,y2[1], pch=1, cex=cex1)
  points(f2,y2[f2], pch=1, cex=cex1, col=colf)
  lines(x, rep(midline, n), col = 'grey40')
  text(x[1:(f2-1)], y2[1:(f2-1)])
  text(x[f2:n], y2[f2:n], labels=f2:n, col=colf)
  par(mfrow=c(1,1))
} # end function seqfig2
seqfig2(n=24, f1=15, low=.3, high=1)
# save as fig2.pdf, width=5, height=4

# Joint distribution conditional on starting position 
# and first crossing. The basis for the tables:
cr100symm <- crossrunsymm(nmax=16, printn=TRUE)$pt
cr100.6 <- crossrunbin(nmax=16, prob=.6, printn=TRUE)$pt
asNumeric(cr100symm[[16]]) # the first table
round(asNumeric(cr100.6[[16]]), 1) # the second table
