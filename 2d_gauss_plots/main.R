rm(list=ls())
set.seed(123)
#require(pdphmc)
library(deSolve)
require(tikzDevice)
Tm <- 100.0
s <- 0
init <- c(0.5,0.0)

beta <- 5.0

#state: c(q,p,Lambda,u)
#ode: c(p,-q,lambda,0)

# event when Lambda=u
root.fun <- function(t,state,parms){return(state[5]-state[6])}

# ode, event spec 1
ode.1 <- function(t,state,parms){
  return(list(c(state[3:4],-state[1:2],1.0/beta,0.0)))
}
# at event, spec 1
at.event.1 <- function(t,state,parms){
  newState <- c(state[1:2], # q unchanged
                parms[1]*state[3:4]+sqrt(1-parms[1]^2)*rnorm(2), # p refreshed
                0.0,# reset Lambda
                rexp(1)) # new u  
}
# ode, event spec 2
ode.2 <- function(t,state,parms){
  return(list(c(state[3:4],-state[1:2],sqrt(sum(state[3:4]^2))/beta,0.0)))
}

# at event, spec 1
at.event.2 <- function(t,state,parms){
  z <- rnorm(2)
  rsq <- rchisq(1,df=3)
  newState <- c(state[1:2], # q unchanged
                z*sqrt(rsq/sum(z^2)), # p refreshed
                0.0,# reset Lambda
                rexp(1)) # new u  
}

ode.3 <- function(t,state,parms){
  basis <- c(1,1)
  q <- state[1:2]
  res.sq.norm <- sum((q - sum(basis*q)/sum(basis^2)*basis)^2)
  return(list(c(state[3:4],-state[1:2],exp(-10.0*(res.sq.norm)),0)))
}


y0 <- c(init,rnorm(2),0.0,rexp(1))



sim.out.1 <- deSolve::lsodar(y=y0,
                           func=ode.1,
                           times=seq(from=0,to=Tm,length.out = 20000), rootfunc = root.fun,
                           events = list(func=at.event.1,root=TRUE),
                           parms=c(0.0))
event.q.1 <- cbind(init,attr(sim.out.1,"valroot")[1:2,])

sim.out.2 <- deSolve::lsodar(y=y0,
                             func=ode.1,
                             times=seq(from=0,to=Tm,length.out = 20000), rootfunc = root.fun,
                             events = list(func=at.event.1,root=TRUE),
                             parms=c(0.7))
event.q.2 <- cbind(init,attr(sim.out.2,"valroot")[1:2,])

sim.out.3 <- deSolve::lsodar(y=y0,
                             func=ode.2,
                             times=seq(from=0,to=Tm,length.out = 20000), rootfunc = root.fun,
                             events = list(func=at.event.2,root=TRUE),
                             )
event.q.3 <- cbind(init,attr(sim.out.3,"valroot")[1:2,])

sim.out.4 <- deSolve::lsodar(y=y0,
                             func=ode.3,
                             times=seq(from=0,to=Tm,length.out = 20000), rootfunc = root.fun,
                             events = list(func=at.event.1,root=TRUE),
                             parms=c(0.0)
)
event.q.4 <- cbind(init,attr(sim.out.4,"valroot")[1:2,])



tikz(file="trajectories.tex",width=12,height=6,standAlone = TRUE)
par(mfrow=c(1,4))
tr <- sim.out.1[,2:3] #fit.c@pointSamples[,1,1:2]
ee <- event.q.1 #fit.c@eventSamples[[1]]
plot(tr[,1],tr[,2],type="l",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),main="Event Spec. 1, $\\beta=5.0 $, $\\phi=0.0$ ",
     xlab = "$q_1$",ylab="$q_2$",cex.lab=2.5,cex.axis=2,cex.main=2)
points(ee[1,],ee[2,],col="red",cex=2.5,lwd=2)
points(ee[1,1],ee[2,1],pch=4,col="green",cex=3,lwd=2)

tr <- sim.out.2[,2:3]#fit.p@pointSamples[,1,1:2]
ee <- event.q.2 #fit.p@eventSamples[[1]]
plot(tr[,1],tr[,2],type="l",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),main="Event Spec. 1, $\\beta=5.0$, $\\phi=0.7$ ",
     xlab = "$q_1$",ylab="$q_2$",cex.lab=2.5,cex.axis=2,cex.main=2)
points(ee[1,],ee[2,],col="red",cex=2.5,lwd=2)
points(ee[1,1],ee[2,1],pch=4,col="green",cex=3,lwd=2)

tr <- sim.out.3[,2:3] #fit.a@pointSamples[,1,1:2]
ee <- event.q.3 #fit.a@eventSamples[[1]]
plot(tr[,1],tr[,2],type="l",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),main="Event Spec. 2, $\\beta=5.0 $ ",
     xlab = "$q_1$",ylab="$q_2$",cex.lab=2.5,cex.axis=2,cex.main=2)
points(ee[1,],ee[2,],col="red",cex=2.5,lwd=2)
points(ee[1,1],ee[2,1],pch=4,col="green",cex=3,lwd=2)

tr <- sim.out.4[,2:3] #fit.a@pointSamples[,1,1:2]
ee <- event.q.4 #fit.a@eventSamples[[1]]
plot(tr[,1],tr[,2],type="l",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),main="$\\lambda(\\mathbf{q}) = \\exp(-10\\parallel \\mathbf{q}-\\frac{\\mathbf{q}^T\\mathbf{v}}{\\mathbf{v}^T\\mathbf{v}} \\mathbf{v}  \\parallel^2_2),$ $\\mathbf{v}=(1,1)^T,$ \n $K_{\\mathbf{q}}(\\mathbf{p} | \\mathbf{p}^\\prime)=\\mathcal{N}(\\mathbf{p} | \\mathbf{0}_d,\\mathbf{M}) $",
     xlab = "$q_1$",ylab="$q_2$",cex.lab=2.5,cex.axis=2,cex.main=1.6)
x.g <- c(-10,10)
lines(x.g,x.g,col="gray",lwd=4)
points(ee[1,],ee[2,],col="red",cex=2.5,lwd=2)
points(ee[1,1],ee[2,1],pch=4,col="green",cex=3,lwd=2)




dev.off()

system("pdflatex trajectories.tex")

