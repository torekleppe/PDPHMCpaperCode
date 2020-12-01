load("Computations")
require(tikzDevice)

tikz(file="stan_funnel_plot.tex",width=12,height=6,standAlone = TRUE)
par(mfrow=c(2,5))


xgrid <- seq(from=-4.0,to=4.0,length.out = 1000)
hist(extract(out8)$x1,probability = TRUE,breaks=29,xlim=c(-4,4),ylim=c(0,0.5),main="Stan, $\\delta=0.8$",ylab="",xlab="$q_1$",cex.lab=2,cex.main=2)
lines(xgrid,dnorm(xgrid),col="red")

hist(extract(out9)$x1,probability = TRUE,breaks=29,xlim=c(-4,4),ylim=c(0,0.5),main="Stan, $\\delta=0.9$",ylab="",xlab="$q_1$",cex.lab=2,cex.main=2)
lines(xgrid,dnorm(xgrid),col="red")



hist(extract(out99)$x1,probability = TRUE,breaks=29,xlim=c(-4,4),ylim=c(0,0.5),main="Stan, $\\delta=0.99$",ylab="",xlab="$q_1$",cex.lab=2,cex.main=2)
lines(xgrid,dnorm(xgrid),col="red")



hist(extract(out999)$x1,probability = TRUE,breaks=29,xlim=c(-4,4),ylim=c(0,0.5),main="Stan, $\\delta=0.999$",ylab="",xlab="$q_1$",cex.lab=2,cex.main=2)
lines(xgrid,dnorm(xgrid),col="red")

hist(s,probability = TRUE,breaks=30,xlim=c(-4,4),ylim=c(0,0.5),main="Proposed method",ylab="",xlab="$q_1$",cex.lab=2,cex.main=2)
lines(xgrid,dnorm(xgrid),col="red")

thin.grid <- seq(from=1,to=50000,by=10) 

plot(thin.grid,extract(out8)$x1[thin.grid],type="l",ylab="$q_1$",ylim=c(-4,4),xlab="iteration \\#",cex.lab=2,cex.main=2)
for(i in 1:9){lines(x=c(i*5000,i*5000),y=c(-5,5),col="red")}

plot(thin.grid,extract(out9)$x1[thin.grid],type="l",ylab="$q_1$",ylim=c(-4,4),xlab="iteration \\#",cex.lab=2,cex.main=2)
for(i in 1:9){lines(x=c(i*5000,i*5000),y=c(-5,5),col="red")}

plot(thin.grid,extract(out99)$x1[thin.grid],type="l",ylab="$q_1$",ylim=c(-4,4),xlab="iteration \\#",cex.lab=2,cex.main=2)
for(i in 1:9){lines(x=c(i*5000,i*5000),y=c(-5,5),col="red")}

plot(thin.grid,extract(out999)$x1[thin.grid],type="l",ylab="$q_1$",ylim=c(-4,4),xlab="iteration \\#",cex.lab=2,cex.main=2)
for(i in 1:9){lines(x=c(i*5000,i*5000),y=c(-5,5),col="red")}

plot(thin.grid,s[thin.grid],type="l",ylab="$q_1$",ylim=c(-4,4),xlab="iteration \\#",cex.lab=2,cex.main=2)
for(i in 1:9){lines(x=c(i*5000,i*5000),y=c(-5,5),col="red")}



dev.off()
system("pdflatex stan_funnel_plot.tex")
system("pdfcrop stan_funnel_plot.pdf")
