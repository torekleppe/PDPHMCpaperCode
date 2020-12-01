


dz <- function(z){ # log of chi-square-1
  return(0.5*z-0.5*exp(z) -0.5*log(2*pi))
}

dv <- Vectorize(function(v){
  zhat <- log(v / 2 - 0.1e1 / 0.8e1 + sqrt(16 * v ^ 2 - 8 * v + 17) / 8)
  scale <- sqrt(6 / (4 * v - 1 + sqrt(16 * v ^ 2 - 8 * v + 17)) * (16 * v ^ 2 - 8 * v + 17) ^ (-0.1e1 / 0.2e1))
  return(integrate(function(z){
        return(exp(dz(z)+dnorm(v,mean=exp(z),sd=sqrt(dta$vr),log=TRUE)))},
            lower=zhat-50*scale,upper = zhat+50*scale)$value)
})

vg <- seq(from=-3,to=10,by=0.001)
plot(vg,dv(vg),type="l")


s <- c()
for(i in 1:fit.cl@chains){
  s <- c(s,fit.cl@pointSamples[1002:2001,i,"x(0)"])
}


hist(s,probability = TRUE,breaks=23)
vg<-seq(from=min(s),to=max(s),by=0.001)
lines(vg,dnorm(vg))