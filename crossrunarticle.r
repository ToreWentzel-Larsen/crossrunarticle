# install packages:
library(grDevices)
library(crossrun)
library(Rmpfr)

# Figure 1
seqfig <- function(n=24, midline=0, low=.5, high=1.5,
                   figseed=83938487) {
  set.seed(figseed)
  x <- seq(n)
  up <- sample(1:n, size=floor(n/2))
  y <- runif(n, -high, -low)
  y[up] <- runif(length(up), low, high)
  op <- par(mar = c(bottom = 0, left   = 0, top    = 0, right  = 0))
  plot(x, y,
       axes = FALSE,
       type = "b",
       pch  = '',
       lwd  = 1.5,
       ylab = '',
       xlab = '')
  lines(x, rep(midline, n), col = 'grey40')
  text(x, y)
  par(op)
} # end function seqfig
seqfig(n=20, low=.3, high=1)
# save as fig1.pdf, width=5, height=2
