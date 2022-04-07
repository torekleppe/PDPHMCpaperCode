load("Computations")
require(tikzDevice)
require(rstan)
require(latex2exp)
#tikz(file="stan_funnel_plot_v2.tex",width=12,height=6,standAlone = TRUE)
pdf("stan_funnel_plot_v2.pdf",width=12,height=4)
par(mfrow=c(1,5))
#par(mfrow=c(1,5))


ecdf.kern <- function(x){
  n <- length(x)
  xx <- sort(x)
  xx <- sort(c(xx,xx))
  y <- (1:n)/n
  yy <- sort(c(1.0e-100,y,y[1:(n-1)]))
  return(list(x=xx,y=yy))
}




xgrid <- seq(from=-4.5,to=0.1,length.out = 1000)

ref.low <- numeric(1000)
ref.high <- numeric(1000)

for(i in 1:length(xgrid)){
  ref.low[i] <- pmax(qbinom(0.05,size=50000,prob=pnorm(xgrid[i]))/50000,1.0e-14)
  ref.high[i] <- qbinom(0.95,size=50000,prob=pnorm(xgrid[i]))/50000
}

ref.poly.x <- c(xgrid,rev(xgrid))
ref.poly.y <- c(ref.low,rev(ref.high))


ff <- ecdf.kern(extract(out8)$x1)
plot(NA,NA,log="y",xlim=c(-4,0),ylim=c(1.0e-5,0.5),main=TeX('Stan, $\\delta = 0.8$'),ylab="",xlab=TeX("$q_1$"),cex.lab=2,cex.main=2)
polygon(ref.poly.x,ref.poly.y,col="grey",border=NA)
lines(xgrid,pnorm(xgrid),col="red",lty=2,lwd=2)
lines(ff$x,ff$y)
#hist(extract(out8)$x1,probability = TRUE,breaks=29,xlim=c(-4,4),ylim=c(0,0.5),main="Stan, $\\delta=0.8$",ylab="",xlab="$q_1$",cex.lab=2,cex.main=2)



ff <- ecdf.kern(extract(out9)$x1)
plot(NA,NA,log="y",xlim=c(-4,0),ylim=c(1.0e-5,0.5),main=TeX("Stan, $\\delta = 0.9$"),ylab="",xlab=TeX("$q_1$"),cex.lab=2,cex.main=2)
polygon(ref.poly.x,ref.poly.y,col="grey",border=NA)
lines(xgrid,pnorm(xgrid),col="red",lty=2,lwd=2)
lines(ff$x,ff$y)
#hist(extract(out9)$x1,probability = TRUE,breaks=29,xlim=c(-4,4),ylim=c(0,0.5),main="Stan, $\\delta=0.9$",ylab="",xlab="$q_1$",cex.lab=2,cex.main=2)

ff <- ecdf.kern(extract(out99)$x1)
plot(NA,NA,log="y",xlim=c(-4,0),ylim=c(1.0e-5,0.5),main=TeX("Stan, $\\delta = 0.99$"),ylab="",xlab=TeX("$q_1$"),cex.lab=2,cex.main=2)
polygon(ref.poly.x,ref.poly.y,col="grey",border=NA)
lines(xgrid,pnorm(xgrid),col="red",lty=2,lwd=2)
lines(ff$x,ff$y)
#hist(extract(out99)$x1,probability = TRUE,breaks=29,xlim=c(-4,4),ylim=c(0,0.5),main="Stan, $\\delta=0.99$",ylab="",xlab="$q_1$",cex.lab=2,cex.main=2)

ff <- ecdf.kern(extract(out999)$x1)
plot(NA,NA,log="y",xlim=c(-4,0),ylim=c(1.0e-5,0.5),main=TeX("Stan, $\\delta = 0.999$"),ylab="",xlab=TeX("$q_1$"),cex.lab=2,cex.main=2)
polygon(ref.poly.x,ref.poly.y,col="grey",border=NA)
lines(xgrid,pnorm(xgrid),col="red",lty=2,lwd=2)
lines(ff$x,ff$y)
#hist(extract(out999)$x1,probability = TRUE,breaks=29,xlim=c(-4,4),ylim=c(0,0.5),main="Stan, $\\delta=0.999$",ylab="",xlab="$q_1$",cex.lab=2,cex.main=2)


ff <- ecdf.kern(s[1:50000])
plot(NA,NA,log="y",xlim=c(-4,0),ylim=c(1.0e-5,0.5),main=TeX("Proposed method"),ylab="",xlab=TeX("$q_1$"),cex.lab=2,cex.main=2)
polygon(ref.poly.x,ref.poly.y,col="grey",border=NA)
lines(xgrid,pnorm(xgrid),col="red",lty=2,lwd=2)
lines(ff$x,ff$y)
#hist(s,probability = TRUE,breaks=30,xlim=c(-4,4),ylim=c(0,0.5),main="Proposed method",ylab="",xlab="$q_1$",cex.lab=2,cex.main=2)
legend(-3.6,7.0e-5,bty="n",
       legend=c("Empirical CDF","True CDF",TeX("90% interval")),
       col=c("black","red","grey"),
       lty=c(1,2,1),
       lwd=c(1,2,6),
       cex=1.2)


dev.off()
#system("pdflatex stan_funnel_plot_v2.tex")
system("pdfcrop stan_funnel_plot_v2.pdf")
