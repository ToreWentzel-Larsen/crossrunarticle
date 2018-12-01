# preparations for figures run24 and run24 case, if cr100 not already in workspace:
# cr100 <- crossrunsymm(100, printn=TRUE)
cr100 <- readRDS('cr100.rds')

# Figure 1
# pdf('fig1.pdf', w = 5, h = 2, pointsize = 8)
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
# dev.off()

# figure run24 ----
# plot to include in article for n=24, f=16 (case 1):
cr24matr <- matrix(as.numeric(cr100$pt[[24]]), ncol=24)
rownames(cr24matr) <- 0:23
colnames(cr24matr) <- 1:24
cr24matr
# runs plot for n=24:
set.seed(83938487)
values <- c(runif(15,.5,1),runif(1,-1,-.5),2*rbinom(8,1,.5)-1+runif(8,-.3,.3))
xright <- 70
ytop <- 7
ybottom <- -1.2
ytimes <- -1.4
cextimes <- .8
cexc <- .5
x24 <- 30
xincr <- 1.5
y24 <- ytop -.5
yincr <- .2
x9 <- 5
y9 <- 6
par(mar=c(bottom=3,left=3,top=0,right=0)+.1)
plot(x=1:24, y=values, axes=FALSE, type="o", pch=19, xlab="", ylab="", 
     xlim=c(1,xright), ylim=c(-2,ytop))
lines(x=c(1,24), y=c(0,0), col="blue")
lines(x=c(1,24), y=c(ybottom,ybottom))
points(x=1, y=values[1], col="red", pch=19)
points(x=15:16, y=values[15:16], col="red", pch=19, type="o")
points(x=17:24, y=values[17:24], col="blue", pch=19, type="o")
text(x=c(1,16,24),y=ytimes, labels=c(1,16,24), cex=cextimes)
text(x=12,y=ytimes-.4, labels="Time", cex=cextimes)
lines(x=c(1,1), y=c(ybottom,max(values)), lty="dotted")
lines(x=c(16,16), y=c(ybottom,max(values)), lty="dotted")
lines(x=c(24,24), y=c(ybottom,values[24]), lty="dotted")
text(x=1,y=max(values), labels="s=1", col="red", pos=3)
text(x=16,y=max(values), labels="f=16", col="red", pos=3)
for (c1 in 0:23) {
  print(lines(x=c(x24,x24+24*xincr), 
              y=c(y24-c1*yincr,y24-c1*yincr), lty="dotted"))
}
for (c1 in c(0,10:23))
  print(text(x=x24+xincr,y=y24-c1*yincr-yincr/2, labels=c1, cex=cexc, pos=2))
for (c1 in 1:9)
  print(text(x=x24+xincr,y=y24-c1*yincr-yincr/2, labels=c1, 
             cex=cexc, pos=2, col="red"))
lines(x=c(x24,x24+24*xincr), y=c(y24-24*yincr,y24-24*yincr), lty="dotted")
text(x=x24-xincr,y=y24-12*yincr, labels="C", pos=2)
for (l1 in 1:24) {
  print(lines(x=c(x24+l1*xincr,x24+l1*xincr), 
              y=c(y24-24*yincr,y24), lty="dotted"))
}
for (l1 in c(1:7,9:12)*2)
  print(text(x=x24+l1*xincr-xincr/2,y=y24-yincr, labels=l1, cex=cexc, pos=3))
text(x=x24+16*xincr-xincr/2,y=y24-yincr, labels=16, 
     cex=cexc, pos=3, col="red")
for (l1 in c(c(1:12)*2-1))
  print(text(x=x24+l1*xincr-xincr/2,
             y=y24-24*yincr+yincr, labels=l1, cex=cexc, pos=1))
text(x=x24+12*xincr,y=y24+yincr, labels="L", pos=3)
text(x=x24+12*xincr,y=y24-24*yincr-yincr/2, labels="L", pos=1)
rect(xleft=x24+15*xincr, xright=x24+16*xincr, border=NA,
     ybottom=y24-10*yincr, ytop=y24-1*yincr, col="red")
for (c1 in 0:9) {
  print(lines(x=c(x9,x9+9*xincr), 
              y=c(y9-c1*yincr,y9-c1*yincr), lty="dotted"))
}
for (c1 in 0:8) {
  print(text(x=x9+xincr,y=y9-c1*yincr-yincr/2, 
             labels=c1, cex=cexc, pos=2))
}
for (l1 in 0:9) {
  print(lines(x=c(x9+l1*xincr,x9+l1*xincr), 
              y=c(y9-9*yincr,y9), lty="dotted"))
}
for (l1 in 1:9) {
  print(text(x=x9+l1*xincr-xincr/2,
             y=y9-yincr, labels=l1, cex=cexc, pos=3))
}
text(x=x9+5*xincr,y=y9+yincr, labels="Last 9 (time 16-24)",  
     pos=3, cex=.7)
arrows(x0=x9, x1=x9+9*xincr, y0=y9-11*yincr,y1=y9-11*yincr,
       col="red")
text(x=x9-xincr,y=y9-14*yincr, labels="Cumutative sums\nin each row",  
     pos=4, cex=.6, col="red")
rect(xleft=x9+8*xincr, xright=x9+9*xincr, border=NA,
     ybottom=y9-9*yincr, ytop=y9, col="red")
arrows(x0=x9+9*xincr, x1=x24+15*xincr, col="red",
       y0=y9-4*yincr,y1=y9-4*yincr)
par(mar=c(bottom=5,left=4,top=4,right=2)+.1)

# figure run24 case ----
# plot to include in article for n=24, f=9 (case 2):
set.seed(83938487)
values2 <- c(runif(8,.5,1),runif(1,-1,-.5),2*rbinom(15,1,.5)-1+runif(15,-.3,.3))
xright <- 70
ytop <- 7
ybottom2 <- -1.4
ytimes2 <- -1.7
cextimes2 <- .7
cexc <- .5
x24 <- 35.5
xincr <- 1.5
y24 <- ytop -.5
yincr <- .2
x16 <- 1
y16 <- 6
par(mar=c(bottom=3,left=3,top=0,right=0)+.1)
plot(x=1:24, y=values2, axes=FALSE, type="o", pch=19, xlab="", ylab="", 
     xlim=c(1,xright), ylim=c(-2.2,ytop))
points(x=10:24, y=values2[10:24], type="o", pch=19, col="blue")
lines(x=c(1,24), y=c(0,0), col="blue")
lines(x=c(1,24), y=c(ybottom2,ybottom2))
points(x=1, y=values[1], col="red", pch=19)
points(x=8:9, y=values2[8:9], col="red", pch=19, type="o")
text(x=c(1,9,24),y=ytimes2, labels=c(1,9,24), cex=cextimes2)
text(x=12,y=ytimes2-.5, labels="Time", cex=cextimes)
lines(x=c(1,1), y=c(ybottom2,max(values2)+.15), lty="dotted")
lines(x=c(9,9), y=c(ybottom2,max(values2)+.4), lty="dotted")
lines(x=c(24,24), y=c(ybottom2,values2[24]), lty="dotted")
text(x=3,y=max(values2[1:8]), labels="s=1", 
     col="red", cex=cextimes2 ,pos=3)
text(x=9,y=max(values2), labels="f=9", 
     col="red", cex=cextimes2, pos=3)
rect(xleft=x24+7*xincr, xright=x24+8*xincr, border=NA,
     ybottom=y24-17*yincr, ytop=y24-1*yincr, col="red")
rect(xleft=x24+8*xincr, xright=x24+16*xincr, border=NA,
     ybottom=y24-17*yincr, ytop=y24-1*yincr, col="blue")
for (c1 in 0:23) {
  print(lines(x=c(x24,x24+24*xincr), 
              y=c(y24-c1*yincr,y24-c1*yincr), lty="dotted"))
}
for (c1 in c(0,17:23))
  print(text(x=x24+xincr,y=y24-c1*yincr-yincr/2, labels=c1, cex=cexc, pos=2))
for (c1 in 1:16)
  print(text(x=x24+xincr,y=y24-c1*yincr-yincr/2, labels=c1, 
             cex=cexc, pos=2, col="red"))
lines(x=c(x24,x24+24*xincr), y=c(y24-24*yincr,y24-24*yincr), lty="dotted")
text(x=x24-xincr,y=y24-yincr, cex=.6, labels="C", pos=2)
text(x=x24-xincr,y=y24-22*yincr, cex=.6, labels="C", pos=2)
for (l1 in 1:24) {
  print(lines(x=c(x24+l1*xincr,x24+l1*xincr), 
              y=c(y24-24*yincr,y24), lty="dotted"))
}
for (l1 in c(1:3,9:12)*2)
  print(text(x=x24+l1*xincr-xincr/2,y=y24-yincr, labels=l1, cex=cexc, pos=3))
for (l1 in c(5:8)*2)
  print(text(x=x24+l1*xincr-xincr/2,y=y24-yincr, 
             labels=l1, cex=cexc, pos=3, col="blue"))
text(x=x24+8*xincr-xincr/2,y=y24-yincr, labels=8, 
     cex=cexc, pos=3, col="red")
for (l1 in c(c(1:4,9:12)*2-1))
  print(text(x=x24+l1*xincr-xincr/2,
             y=y24-24*yincr+yincr, labels=l1, cex=cexc, pos=1))
for (l1 in c(c(5:8)*2-1))
  print(text(x=x24+l1*xincr-xincr/2, col="blue",
             y=y24-24*yincr+yincr, labels=l1, cex=cexc, pos=1))
text(x=x24+12*xincr,y=y24+yincr, labels="L", pos=3)
text(x=x24+12*xincr,y=y24-24*yincr-yincr/2, labels="L", pos=1)
rect(xleft=x16+7*xincr, xright=x16+8*xincr, border=NA,
     ybottom=y16, ytop=y16-16*yincr, col="red")
rect(xleft=x16+8*xincr, xright=x16+16*xincr, border=NA,
     ybottom=y16-16*yincr, ytop=y16, col="blue")
for (c1 in 0:15) {
  print(lines(x=c(x16,x16+16*xincr), 
              y=c(y16-c1*yincr,y16-c1*yincr), lty="dotted"))
}
for (c1 in (c(1:8)*2-1)) {
  print(text(x=x16+xincr+xincr/2,y=y16-c1*yincr+yincr/2, 
             labels=c1-1, cex=cexc, pos=2))
}
for (c1 in (c(1:8)*2)) {
  print(text(x=x16+15*xincr-xincr/2,y=y16-c1*yincr+yincr/2, 
             labels=c1-1, cex=cexc, pos=4))
}
for (l1 in 0:15) {
  print(lines(x=c(x16+l1*xincr,x16+l1*xincr), 
              y=c(y16-16*yincr,y16), lty="dotted"))
}
for (l1 in c(1:3)*2) {
  print(text(x=x16+l1*xincr-xincr/2,
             y=y16-yincr, labels=l1, cex=cexc, pos=3))
}
for (l1 in (c(1:4)*2-1)) {
  print(text(x=x16+l1*xincr-xincr/2,
             y=y16-15*yincr+yincr/2, labels=l1, cex=cexc, pos=1))
}
print(text(x=x16+8*xincr-xincr/2, col="red",
           y=y16-yincr, labels=8, cex=cexc, pos=3))
for (l1 in (c(5:8)*2)) {
  print(text(x=x16+l1*xincr-xincr/2, col="blue",
             y=y16-yincr, labels=l1, cex=cexc, pos=3))
}
for (l1 in c(5:8)*2-1) {
  print(text(x=x16+l1*xincr-xincr/2, col="blue",
             y=y16-20*yincr, labels=l1, cex=cexc, pos=3))
}
text(x=x16+5*xincr,y=y9+yincr, labels="Last 16 (time 9-24)",  
     pos=3, cex=.5)
arrows(x0=x16, x1=x16+7*xincr, y0=y16-7*yincr,y1=y16-7*yincr,
       col="red", lwd=2)
par(mar=c(bottom=5,left=4,top=4,right=2)+.1)
