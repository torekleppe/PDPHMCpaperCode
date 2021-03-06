
require(rstan)


#if(!exists("raw")){
raw <- read.table("DATA_CAW.txt")
raw <- t(raw)
#}
q <- 5
T <- dim(raw)[1]
rr <- array(dim=c(q,q,T))
k <- 1

for(j in 1:q){
  for(i in j:q){
    rr[i,j,] <- raw[,k]
    rr[j,i,] <- rr[i,j,]
    k <- k+1
  }
}



yinv <- matrix(0.0,nrow=T*q,ncol=q)
yy <- matrix(0.0,nrow=T*q,ncol=q)
ldets <- 0.0
for(t in 1:T){
  yy[((t-1)*q+1):(t*q),] <- rr[,,t]
  yinv[((t-1)*q+1):(t*q),] <- solve(rr[,,t])
  ldets <- ldets + 2.0*sum(log(diag(chol(rr[,,t]))))
}



chains <- 10

h.0 <- c(0.39035784,
         0.29355181,
         0.29500684,
         0.22925192,
         0.19641877,
         0.16663467,
         0.12474880,
         0.21586103,
         0.18400667,
         0.10879085)

lnu.0 <- -0.06935434
lphi.0 <- c(4.13009026, 4.53550637,  3.91026493,  3.49787375,  3.91649845)
mu.0 <- c(4.15074632,  4.11688525,  3.71114757, 4.10998291,  3.52874206)
lsigmaSq.0 <- c(-2.35665621, -2.66023710, -2.45270714, -2.55785135, -2.73708414)




initfun <- function(chain_id){
  return(list(nu=34.0,delta=exp(lphi.0)/(1+exp(lphi.0)),mu=mu.0,hts=h.0,sigmaSq=exp(lsigmaSq.0)))
}
rstan_options(auto_write=FALSE)
fit.s.d <- stan(file="realVol.stan",pars=c("mu","sigma","delta","hts","nu","firstx","firstz","lastz"),
                chains=chains,data=list(T=T,yy=yy,yinv=yinv,ldets=ldets),init=initfun,verbose=TRUE)

save.image(file="Computations_stan")

