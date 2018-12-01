# Figure 1
pdf('fig1.pdf', w = 5, h = 2, pointsize = 8)
set.seed(32)
n <- 24
y <- rnorm(n)
x <- seq(n)

op <- par(mar = c(bottom = 0, left   = 0, top    = 0, right  = 0))
plot(x, y,
     axes = FALSE,
     type = "b",
     pch  = '',
     lwd  = 1.5,
     ylab = '',
     xlab = '')

lines(x, rep(median(y), n), col = 'grey40')
text(x, y)
par(op)
dev.off()

# Figure 2
