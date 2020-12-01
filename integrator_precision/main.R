exact_traj <- function(t,z0){ # see Maple work sheet
  t1 <- sqrt(13)
  t2 <- sqrt(5)
  t3 <- t / 4
  t4 <- t3 * t2
  t5 <- sin(t4)
  t4 <- cos(t4)
  t6 <- 0.2e1 / 0.3e1 * z0[4]
  t7 <- z0[3] + t6
  t3 <- t3 * t1
  t8 <- sin(t3)
  t3 <- cos(t3)
  t9 <- 2 * z0[4]
  t10 <- z0[3] - t9
  t11 <- z0[3] + 5 * z0[4]
  t12 <- z0[3] + 3 * z0[4]
  t13 <- z0[3] + t9
  t14 <- 2 * z0[2]
  t15 <- 0.2e1 / 0.3e1 * z0[2]
  t16 <- z0[3] + 18 * z0[4]
  t17 <- z0[3] + 4 * z0[4]
  t18 <- z0[3] + 0.32e2 / 0.9e1 * z0[4]
  t19 <- t / 2
  t20 <- t19 * t2
  t21 <- sin(t20)
  t22 <- z0[1] ^ 2
  t23 <- z0[3] ^ 2
  t20 <- cos(t20)
  t24 <- z0[4] ^ 2
  t25 <- -t23 + t24
  t26 <- z0[2] ^ 2
  t27 <- -t26 + t22
  t19 <- t19 * t1
  t28 <- sin(t19)
  t19 <- cos(t19)
  t29 <- -260
  t30 <- 3770 * z0[3]
  t31 <- 1040 * z0[2]
  t32 <- 0.1e1 / 0.16900e5
  t33 <- 0.29e2 / 0.130e3
  t6 <- (0.11e2 / 0.260e3 * (0.4e1 / 0.11e2 * z0[2] * (-z0[1] + z0[2]) - 0.7e1 / 0.11e2 * t23 + (-0.12e2 / 0.11e2 * z0[3] - 0.28e2 / 0.11e2 * z0[4]) * z0[4] + t22) * t20 + 0.16e2 / 0.845e3 * t25 + 0.8e1 / 0.845e3 * t27 + 0.3e1 / 0.130e3 * t2 * ((z0[3] - t6) * z0[1] - t15 * t13) * t21 - 0.56e2 / 0.845e3 * z0[3] * z0[4] + 0.28e2 / 0.845e3 * z0[1] * z0[2]) * t1 * t28
  t6 <- (((0.36e2 / 0.29e2 * z0[4] + 0.4e1 / 0.29e2 * z0[3]) * z0[4] + t23) * t33 + 0.57e2 / 0.130e3 * t22 + 0.4e1 / 0.65e2 * t26) * t + t32 * (t19 * (t20 * ((t29 * z0[4] - t30) * z0[1] + t29 * z0[2] * t16) - 2080 * t12 * z0[1] - t31 * t10) + t20 * (2080 * t11 * z0[1] - 3120 * t7 * z0[2]) + (-3640 * t * z0[2] + t30 - 3900 * z0[4]) * z0[1] + (4420 * z0[3] + 4680 * z0[4]) * z0[2]) + 0.17e2 / 0.260e3 * t2 * (-0.64e2 / 0.85e2 * t25 + 0.32e2 / 0.85e2 * t27 + (0.44e2 / 0.17e2 * t24 - 0.4e1 / 0.17e2 * t26 + t22 - 0.5e1 / 0.17e2 * t23 - 0.12e2 / 0.17e2 * z0[1] * z0[2] + 0.28e2 / 0.17e2 * z0[3] * z0[4]) * t19 + 0.224e3 / 0.85e2 * z0[3] * z0[4] + 0.112e3 / 0.85e2 * z0[1] * z0[2]) * t21 + t6
  t29 <- 6
  t30 <- 2 * t2
  t32 <- 2340 * t18
  t33 <- -0.1e1 / 0.130e3
  t34 <- 0.1e1 / 0.8450e4
  t31 <- t34 * (t19 * (t20 * (-130 * t16 * z0[1] - t32 * z0[2]) - 1820 * t12 * z0[1] - 910 * z0[2] * t10) + t20 * (1820 * t11 * z0[1] - 2730 * t7 * z0[2]) + (t31 * t + 130 * z0[3] - 1300 * z0[4]) * z0[1] + (5980 * z0[3] + 8320 * z0[4]) * z0[2]) - 0.7e1 / 0.65e2 * t * t27 + t33 * (t20 * (-t26 * t29 - 4 * z0[1] * z0[2] + 28 * z0[3] * z0[4] + t22 + 3 * t23 + 52 * t24) - 0.56e2 / 0.13e2 * t25 - 0.28e2 / 0.13e2 * t27 + t30 * (t13 * z0[1] + t14 * t17) * t21 - 0.98e2 / 0.13e2 * z0[1] * z0[2] + 0.196e3 / 0.13e2 * z0[3] * z0[4]) * t1 * t28 - 0.3e1 / 0.130e3 * t2 * (t19 * (0.10e2 / 0.3e1 * t26 + t22 - 0.7e1 / 0.3e1 * t23 - 28 * t24 - 0.44e2 / 0.3e1 * z0[3] * z0[4] + 0.4e1 / 0.3e1 * z0[1] * z0[2]) + 0.56e2 / 0.15e2 * t25 - 0.28e2 / 0.15e2 * t27 - 0.98e2 / 0.15e2 * z0[1] * z0[2] - 0.196e3 / 0.15e2 * z0[3] * z0[4]) * t21 + t * ((36 * z0[3] + 64 * z0[4]) * z0[4] + t23) / 65
  t33 <- 0.23e2 / 0.2e1
  t35 <- 0.18e2 / 0.65e2
  t19 <- (((0.121e3 / 0.9e1 * z0[4] + 0.64e2 / 0.9e1 * z0[3]) * z0[4] + t23) * t35 + 0.57e2 / 0.130e3 * t26 + 0.4e1 / 0.65e2 * t22) * t + t34 * (t19 * (t20 * (-t32 * z0[1] - (8320 * z0[3] + 31460 * z0[4]) * z0[2]) + 1040 * t12 * z0[1] + 520 * z0[2] * t10) + t20 * (-1040 * t11 * z0[1] + 1560 * t7 * z0[2]) + (1820 * t * z0[2] + 2340 * z0[3] + 10400 * z0[4]) * z0[1] + (6240 * z0[3] + 31460 * z0[4]) * z0[2]) + t1 * (t20 * (t29 * z0[1] * z0[2] + t26 * t33 - 52 * z0[3] * z0[4] + t22 - 7 * t23 - 98 * t24) - 0.16e2 / 0.13e2 * t25 - 0.8e1 / 0.13e2 * t27 - t30 * (t17 * z0[1] + (4 * z0[3] + 15 * z0[4]) * z0[2]) * t21 - 0.28e2 / 0.13e2 * z0[1] * z0[2] + 0.56e2 / 0.13e2 * z0[3] * z0[4]) * t28 / 65 - t2 * (t19 * (t22 - 11 * t23 - 158 * t24 + 0.37e2 / 0.2e1 * t26 + 10 * z0[1] * z0[2] - 84 * z0[3] * z0[4]) - 0.16e2 / 0.5e1 * t25 + 0.8e1 / 0.5e1 * t27 + 0.56e2 / 0.5e1 * z0[3] * z0[4] + 0.28e2 / 0.5e1 * z0[1] * z0[2]) * t21 / 65
  cg <- matrix(c(t3 * (t2 * t10 * t5 / 5 + t4 * z0[1]) - 0.7e1 / 0.65e2 * t1 * (t2 * (z0[1] - 0.4e1 / 0.7e1 * z0[2]) * t5 - 0.15e2 / 0.7e1 * t4 * t7) * t8,t3 * (-0.2e1 / 0.5e1 * t2 * t12 * t5 + t4 * z0[2]) + (0.4e1 / 0.65e2 * t2 * (z0[1] + 0.7e1 / 0.4e1 * z0[2]) * t5 + 0.2e1 / 0.13e2 * t4 * t11) * t1 * t8,z0[3] + t9 + (0.3e1 / 0.13e2 * t4 * (z0[1] + t15) - t2 * t5 * t16 / 65) * t1 * t8 + (-t13 * t4 + t2 * t5 * (z0[1] - t14) / 5) * t3,0.2e1 / 0.13e2 * t1 * (t4 * (z0[1] + 5 * z0[2]) - 0.9e1 / 0.5e1 * t18 * t2 * t5) * t8 + (-2 * t17 * t4 - 0.2e1 / 0.5e1 * t2 * t5 * (z0[1] + 3 * z0[2])) * t3 + 2 * z0[3] + 8 * z0[4],t6,t31,t19),ncol=1)
  return(as.vector(cg))
}

library("pdphmc")

Tmaxs <- c(100,1000,10000,100000)
ltols <- (seq(from=log(1.0e-6),to=log(0.5),length.out = 16)) 
truem <- c(0,0,1,2,8)
nrep <- 50

lambda <- 10.0
nsamples <- 1000

res <- matrix(0.0,length(ltols)*length(Tmaxs),7)
res.rmse <- matrix(0.0,5,length(Tmaxs))
rc <- 1
mdl <- build("mdl.cpp",massMatrix="identityMass",include.flags = " -D __STORE_RNGS__")


for(i1 in 1:length(Tmaxs)){
  for(i2 in 1:length(ltols)){
    
    diffs <- matrix(0.0,5,nrep)
    
    for(i3 in 1:nrep){
      # run numerical simulation
      fit  <- run(mdl,chains=1,seed=i3+100*i1-13*i2,
                  samples=nsamples,Tmaxs[i1],
                  warmupFrac=0.0,
                  control=list(absTol=exp(ltols[i2]),
                               relTol=exp(ltols[i2]),
                               lambdaAdapt=c(0.0,lambda,1.0),
                               maxInitTraj=0),
                  out.files = list(csv.prec = 14)  ,clean=FALSE)
      
      # replicate using exact dynamics
      f <- jsonlite::fromJSON(readLines(paste0(mdl@file.name.base,"_1_misc.json")))
      rngn <- f$storeRngs_n
      rngu <- f$storeRngs_u 
      
      z0 <- c(rngn[1:2],rngn[5:6]) # initial configurations
      nc <- 7
      
      t <- 0.0  # initial time
      
      tinc <- min(Tmaxs[i1],-log(rngu[1])*lambda)
      nu <- 2
      moments <- numeric(5)
      while(TRUE){
        dyn.out <- exact_traj(tinc,z0) # new q, then moments
        moments <- moments + dyn.out[3:7]
        t <- t+tinc
        if(t >= Tmaxs[i1]) break
        z0 <- c(dyn.out[1:2],rngn[nc:(nc+1)])
        nc <- nc+2
        tinc <- -log(rngu[nu])*lambda
        nu <- nu+1
        tinc <- min(Tmaxs[i1]-t,tinc)
      }
      moments <- moments/Tmaxs[i1]
      nt <- colMeans(fit@intSamples[2:(nsamples+1),1,])
      diffs[,i3] <- nt-moments
      res.rmse[,i1] <- res.rmse[,i1] + (moments-truem)^2
    }
    
    res[rc,1] <- Tmaxs[i1]
    res[rc,2] <- ltols[i2]
    res[rc,3:7] <- sqrt(rowMeans(diffs^2))
    rc <- rc+1
  }
  res.rmse[,i1] <- sqrt(res.rmse[,i1]/(length(ltols)*nrep))
}




df <- data.frame(res)
colnames(df) <- c("T","tol","q1","q2","q1sq","q1q2","q2sq")
write.table(df,"table.txt")


df2 <- data.frame(Tmaxs,t(res.rmse))
colnames(df2) <- c("T","q1","q2","q1sq","q1q2","q2sq")
write.table(df2,"tableRMSE.txt")


