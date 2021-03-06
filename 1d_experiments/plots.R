require(tikzDevice)

r <- as.matrix(read.table("results.txt"))

tikz(file="mean_sd.tex",width=12,height=6,standAlone = TRUE)
par(mfrow=c(1,4))
beta <- r[21,]
mains <- c("$N(0,1)$","standardized $t_{20}$","standardized $\\chi^2_{50}$","standardized $\\chi^2_{30}$")
ylab <- "Monte Carlo RMSE of mean estimate"
for(m in 1:4){
  plot(beta,r[m*5,],xlab="$\\beta$",main=mains[m],type="l",lwd=2,
       ylab=ylab,cex.lab=1.5,cex.axis=1.8,cex.main=2,ylim=c(0,0.14))
  points(beta,r[(m-1)*5+1,],pch=1,cex=2)
  points(beta,r[(m-1)*5+2,],pch=4,cex=2,col="red")
  points(beta,r[(m-1)*5+3,],pch=0,cex=2,col="blue")
  points(beta,r[(m-1)*5+4,],pch=17,cex=2,col="green")
  ylab <- ""
}

legend(x="topright",
       legend=c("iid RMSE","Integrated","Discrete, high freq.","Discrete, low freq.","Randomized HMC"),
       cex=1.7,
       lwd=c(2,NaN,NaN,NaN,NaN),
       pch=c(NaN,1,4,0,17),
       col=c("black","black","red","blue","green"))


dev.off()
system("pdflatex mean_sd.tex")
# 
# r <- as.matrix(read.table("results_arclength.txt"))
# 
# tikz(file="arc_mean_sd.tex",width=12.2,height=6,standAlone = TRUE)
# par(mfrow=c(1,4))
# beta <- r[17,]
# mains <- c("$N(0,1)$","standardized $t_{20}$","standardized $\\chi^2_{50}$","standardized $\\chi^2_{30}$")
# ylab <- "Monte Carlo standard deviation of mean estimate"
# for(m in 1:4){
#   plot(beta,r[m*4,],xlab="$\\beta$",main=mains[m],type="l",lwd=2,
#        ylab=ylab,cex.lab=1.8,cex.axis=1.8,cex.main=2,ylim=c(0,0.14))
#   points(beta,r[(m-1)*4+2,],pch=4,cex=2)
#   points(beta,r[(m-1)*4+3,],pch=0,cex=2)
#   points(beta,r[(m-1)*4+1,],pch=1,cex=2)
#   ylab <- ""
# }
# 
# 
# dev.off()
# system("pdflatex arc_mean_sd.tex")
