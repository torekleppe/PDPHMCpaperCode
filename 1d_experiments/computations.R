require(pdphmc)
nrep <- 10000

lams <- 0.5*pi*c(0.1,0.2,0.5,seq(from=1,to=7,by=0.6))

results <- matrix(0.0,4*5+1,length(lams))

models <- c("gauss.cpp","tdistr.cpp","chi2_50.cpp","chi2_30.cpp")

for(m in 1:length(models)){
  
  mdl <- build(file = models[m],massMatrix = "identityMass",include.flags = " -D __STORE_EVENT_Q__")
  
  res <- vector(mode="numeric",nrep)
  
  pres <- res
  pres2 <- res
  pres3 <- res
 
  for(i in 1:length(lams)){
    for(rep in 1:nrep){
      fit.g <- run(mdl,control=list(lambdaAdapt=c(0,1.0/lams[i],0.0)),Tmax=2*1000*0.5*pi,chains=1,seed=rep)
      res[rep] <- mean(fit.g@intSamples[1002:2001,1,1])
      pres[rep] <- mean(fit.g@pointSamples[1002:2001,1,1])
      pres2[rep] <- mean(fit.g@pointSamples[seq(from=1002,to=2001,by=2),1,1])
      # get also samples at events; correspond to randomized HMC
      
      esamples <- read.csv(paste0(mdl@file.name.base,"_1_events.csv"),header = FALSE)$V1
      sam <- as.logical(c(0,1-fit.g@diagnostics[[1]][,"warmup"]))
      pres3[rep] <- mean(esamples[sam])
    }
    results[(m-1)*5 + 1,i] <- sqrt(mean(res^2))
    results[(m-1)*5 + 2,i] <- sqrt(mean(pres^2))
    results[(m-1)*5 + 3,i] <- sqrt(mean(pres2^2))
    results[(m-1)*5 + 4,i] <- sqrt(mean(pres3^2))
  }
  
  # simulate for iid samples
  iires <- vector(mode="numeric",10000)
  if(models[m] == "gauss.cpp"){
    for(rep in 1:10000){
      iires[rep] <- mean(rnorm(n=1000))
    }
  } else if(models[m] == "tdistr.cpp"){
    for(rep in 1:10000){
      iires[rep] <- mean(rt(n=1000,df=20)*(1.0/sqrt(20.0/18.0)))
    }
  } else if(models[m] == "chi2_50.cpp"){
    for(rep in 1:10000){
      iires[rep] <- mean((rchisq(n=1000,df=50)-50)*(1.0/sqrt(2.0*50)))
    }
  } else {
    for(rep in 1:10000){
      iires[rep] <- mean((rchisq(n=1000,df=30)-30)*(1.0/sqrt(2.0*30)))
    }
  }
  results[m*5,] <- sqrt(mean(iires^2))
}

results[21,] <- lams

write.table(results,file="results.txt")


# 
# par(mfrow=c(1,1))
# plot(lams,results[1,])
# points(lams,results[2,])
# points(lams,results[3,])
# lines(lams,results[4,1]+0*lams)


