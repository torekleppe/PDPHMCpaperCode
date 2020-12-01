rm(list=ls())

require(rstan)
library(pdphmc)
## load the salamander data
load("salam.RData")

## organize data into a form suitable for logistic regression
dat0=data.frame("y"=c(salam$y),
"fW"=as.integer(salam$x[,"W/R"]==1 | salam$x[,"W/W"]==1),
"mW"=as.integer(salam$x[,"R/W"]==1 | salam$x[,"W/W"]==1),
"WW"=as.integer(salam$x[,"W/W"]==1 ) )

## add salamander id (female, male)
id = t( apply(salam$z, 1, function(x) {
    tmp = which (x==1)
    tmp[2] = tmp[2] - 20
    tmp
}) )

## index for the experiment group
dat0$group=as.factor(c(rep(c(rep(1,5),rep(2,10),rep(1,5)),6),
rep(c(rep(3,5),rep(4,10),rep(3,5)),6),
rep(c(rep(5,5),rep(6,10),rep(5,5)),6)))
## index for the experiment
dat0$experiment=as.factor(rep(1:3, each=120))

## set the indices for the first two experiments modeled as iid2d,
## (The indices for the third experiment are set to NA)
fid_iid2_e1e2 <- c(id[,1],id[,1] + 20, rep(NA, 120))
mid_iid2_e1e2 <- c(id[,2],id[,2] + 20, rep(NA, 120))

## set the indices for third experiment
## (The indices for the first two experiments are set to NA)
fid_id_e3 <- c(rep(NA,240),id[,1])
mid_id_e3 <- c(rep(NA,240),id[,2])

## Indicator for fall
fall <- c(rep(0, 120), rep(1,240))

## generate the dataset
data <- data.frame(dat0, fid_iid2_e1e2, mid_iid2_e1e2, fid_id_e3, mid_id_e3, fall)

pdpdta <- list(y=as.integer(data$y),
               fW=data$fW,
               mW=data$mW,
               WW=data$WW,
               fall=data$fall,
               f1=as.integer((data$fid_iid2_e1e2)[1:240]),
               m1=as.integer((data$mid_iid2_e1e2)[1:240]),
               f2=as.integer((data$fid_id_e3)[241:360]),
               m2=as.integer((data$mid_id_e3)[241:360])
               )

Tmax <- 2000.0
seed <- 1
pmdl<-build("salamander_dr_wish.cpp",lambda="lambda_constant",massMatrix = "diagMassVARI")
fit.c <- run(pmdl,Tmax=Tmax,data=pdpdta,pars=c("beta"),
             control=list(absTol=1.0e-3,relTol=1.0e-3,lambdaAdapt=c(1.0,1.0,3.0)),
             seed=seed,chains=10,cores=1)

fit.cl <- run(pmdl,Tmax=Tmax,data=pdpdta,pars=c("beta"),
              control=list(absTol=1.0e-3,relTol=1.0e-3,lambdaAdapt=c(1.0,1.0,10.0)),
              seed=seed,chains=10,cores=1)

pmdl.a<-build("salamander_dr_wish.cpp",lambda = "lambda_arclength",massMatrix="diagMassVARI")

fit.a <- run(pmdl.a,Tmax=Tmax,data=pdpdta,pars=c("beta"),
             control=list(absTol=1.0e-3,relTol=1.0e-3,lambdaAdapt=c(1.0,1.0,3.0)),
             seed=seed,chains=10,cores=1)
fit.al <- run(pmdl.a,Tmax=Tmax,data=pdpdta,pars=c("beta"),
              control=list(absTol=1.0e-3,relTol=1.0e-3,lambdaAdapt=c(1.0,1.0,10.0)),
              seed=seed,chains=10,cores=1)


standta <- list(y=data$y,fW=data$fW,mW=data$mW,WW=data$WW,fall=data$fall,f1=(data$fid_iid2_e1e2)[1:240],
m1=(data$mid_iid2_e1e2)[1:240],f2=(data$fid_id_e3)[241:360],m2=(data$mid_id_e3)[241:360],
lamFmean=c(0.0,0.0),lamMmean=c(0.0,0.0),V1Fmean=0.0,V1Mmean=0.0,logkappaFmean=0.0,logkappaMmean=0.0);
stan.fit<-stan(file="salamander_drhmc_wish.stan",seed=seed,data=standta,chains=10,control=list(adapt_delta=0.95))

save.image(file="Computations")



