rm(list=ls())
require(rstan)
require(tikzDevice)

require(pdphmc)
rstan_options(auto_write = FALSE)

nchains <- 10
niter <- 10000
table <- matrix(0.0,nrow=4,ncol=6)


  
  
out8 <- stan(file="model.stan",seed=1,iter=niter,chains=nchains,control=list(adapt_delta=0.8,metric="unit_e")) #,diagnostic_file = "diag8.txt")
out9 <- stan(file="model.stan",seed=1,iter=niter,chains=nchains,control=list(adapt_delta=0.9,metric="unit_e")) #,diagnostic_file = "diag9.txt")
out99 <- stan(file="model.stan",seed=1,iter=niter,chains=nchains,control=list(adapt_delta=0.99,metric="unit_e")) #,diagnostic_file = "diag99.txt")
out999 <- stan(file="model.stan",seed=1,iter=niter,chains=nchains,control=list(adapt_delta=0.999,metric="unit_e")) #,diagnostic_file = "diag999.txt")


mdl <- build(file="funnel.cpp", lambda="lambda_constant",massMatrix = "identityMass")
fit <- run(mdl,Tmax=100000,chains=nchains,samples=niter,seed=1)
s <- as.vector(fit@pointSamples[(fit@warmup+1):(fit@samples+1),,"v(0)"])

save.image("Computations")



