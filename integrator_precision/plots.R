library("tikzDevice")

r <- read.table("table.txt")
rmse <- read.table("tableRMSE.txt")

Tmaxs <- c(100,1000,10000,100000)
ltols <- (seq(from=log(1.0e-6),to=log(0.5),length.out = 16))

tikz(file="pathwise_RMSE.tex",standAlone = TRUE,width=14,height = 7)

par(mfrow=c(1,5))
# \mu_1
id <- r$"T"==Tmaxs[1]
idr <- rmse$"T"==Tmaxs[1]
plot(exp(r$tol[id]),r$q1[id],log="xy",col=1,ylim=c(1.0e-12,10),type="o",
     xlab="$tol_a=tol_r$",ylab="numerical error",cex.lab=2,cex.axis=2,cex.main=2,pch=1,
     lwd=2,main="$E(\\mathbf q_1)$")
lines(c(1.0e-100,10),rep(rmse$q1[idr],2),col=1,lwd=3,type="c")

for(i in 2:length(Tmaxs)){
  id <- r$"T"==Tmaxs[i]
  idr <- rmse$"T"==Tmaxs[i]
  points(exp(r$tol[id]),r$q1[id],col=i,type="o",lwd=2,pch=i)
  lines(c(1.0e-100,10),rep(rmse$q1[idr],2),col=i,lwd=3,type="c")
}




# \mu_2
id <- r$"T"==Tmaxs[1]
idr <- rmse$"T"==Tmaxs[1]
plot(exp(r$tol[id]),r$q2[id],log="xy",col=1,ylim=c(1.0e-12,10),type="o",
     xlab="$tol_a=tol_r$",ylab="",cex.lab=2,cex.axis=2,cex.main=2,pch=1,
     lwd=2,main="$E(\\mathbf q_2)$")
lines(c(1.0e-100,10),rep(rmse$q2[idr],2),col=1,lwd=3,type="c")

for(i in 2:length(Tmaxs)){
  id <- r$"T"==Tmaxs[i]
  idr <- rmse$"T"==Tmaxs[i]
  points(exp(r$tol[id]),r$q2[id],col=i,type="o",lwd=2,pch=i)
  lines(c(1.0e-100,10),rep(rmse$q2[idr],2),col=i,lwd=3,type="c")
}



#\Sigma_{1,1}
id <- r$"T"==Tmaxs[1]
idr <- rmse$"T"==Tmaxs[1]
plot(exp(r$tol[id]),r$q1sq[id],log="xy",col=1,ylim=c(1.0e-12,10),type="o",pch=1,
     xlab="$tol_a=tol_r$",ylab="",lwd=2,cex.lab=2,cex.axis=2,cex.main=2,main="$E(\\mathbf q_1^2)$")
lines(c(1.0e-100,10),rep(rmse$q1sq[idr],2),col=1,lwd=3,type="c")

for(i in 2:length(Tmaxs)){
  id <- r$"T"==Tmaxs[i]
  idr <- rmse$"T"==Tmaxs[i]
  points(exp(r$tol[id]),r$q1sq[id],col=i,type="o",lwd=2,pch=i)
  lines(c(1.0e-100,10),rep(rmse$q1sq[idr],2),col=i,lwd=3,type="c")
}

legend(list(x=1.0e-5,y=1.0e-9),legend=c("$T=100$","$T=1000$","$T=10000$","$T=100000$"),
       lty=c(1,1,1,1),lwd=c(2,2,2,2),col=c(1,2,3,4),cex=2,pch=c(1,2,3,4))

#\Sigma_{1,2}
id <- r$"T"==Tmaxs[1]
idr <- rmse$"T"==Tmaxs[1]
plot(exp(r$tol[id]),r$q1q2[id],log="xy",col=1,ylim=c(1.0e-12,10),type="o",pch=1,
     xlab="$tol_a=tol_r$",ylab="",lwd=2,cex.lab=2,cex.axis=2,cex.main=2,main="$E(\\mathbf q_1 \\mathbf q_2)$")
lines(c(1.0e-100,10),rep(rmse$q1q2[idr],2),col=1,lwd=3,type="c")

for(i in 2:length(Tmaxs)){
  id <- r$"T"==Tmaxs[i]
  idr <- rmse$"T"==Tmaxs[i]
  points(exp(r$tol[id]),r$q1q2[id],col=i,type="o",lwd=2,pch=i)
  lines(c(1.0e-100,10),rep(rmse$q1q2[idr],2),col=i,lwd=3,type="c")
}

#\Sigma_{2,2}
id <- r$"T"==Tmaxs[1]
idr <- rmse$"T"==Tmaxs[1]
plot(exp(r$tol[id]),r$q2sq[id],log="xy",col=1,ylim=c(1.0e-12,10),type="o",pch=1,
     xlab="$tol_a=tol_r$",ylab="",lwd=2,cex.lab=2,cex.axis=2,cex.main=2,main="$E(\\mathbf q_2^2)$")
lines(c(1.0e-100,10),rep(rmse$q2sq[idr],2),col=1,lwd=3,type="c")

for(i in 2:length(Tmaxs)){
  id <- r$"T"==Tmaxs[i]
  idr <- rmse$"T"==Tmaxs[i]
  points(exp(r$tol[id]),r$q2sq[id],col=i,type="o",lwd=2,pch=i)
  lines(c(1.0e-100,10),rep(rmse$q2sq[idr],2),col=i,lwd=3,type="c")
}


dev.off()

system("pdflatex pathwise_RMSE.tex")
