library(pdphmc)
library(xtable)
#load("Computations")



tab <- matrix(0.0,nrow=19,ncol=11)
k <- 1

nms <- c("tau1f","tau2f","rhof","tau1m","tau2m","rhom","tau3f","tau3m")
nmsBeta <- c("gbeta(0)","gbeta(1)","gbeta(2)","gbeta(3)","gbeta(4)")
nmsRE <- vector(mode="character",length = 120)
for(i in 1:40) nmsRE[i] <- paste0("gb1f(",i-1,")")
for(i in 1:40) nmsRE[i+40] <- paste0("gb1m(",i-1,")")
for(i in 1:20) nmsRE[i+80] <- paste0("gb3f(",i-1,")")
for(i in 1:20) nmsRE[i+100] <- paste0("gb3m(",i-1,")")

# stan part of table
tab.stan <-(summary(stan.fit,c("tau1f","tau2f","rhof","tau1m","tau2m","rhom","kappaF","kappaM","beta"))$summary)
RE.stan <- (summary(stan.fit,c("b1f","b1m","b3f","b3m"))$summary)
stan.time <- sum(get_elapsed_time(stan.fit)[,"sample"])
tab[k,1:8] <- round(tab.stan[1:8,"mean"],2)
tab[k,11] <- round(stan.time,2)
k <- k+1
tab[k,1:8] <- round(tab.stan[1:8,"sd"],2)
k <- k+1

tab[k,1:8] <- round(tab.stan[1:8,"n_eff"]/stan.time)
tab[k,9] <- round(min(RE.stan[,"n_eff"])/stan.time)
tab[k,10] <- round(min(tab.stan[9:13,"n_eff"])/stan.time)
k <- k+1

# pdp part of table
#
f <- fit.c
m <- as.matrix(getMonitor(f,print=F)[[1]])
mi <- as.matrix(getIntMonitor(f,print=F)[[1]])
cpt <- sum(get_CPU_time(f)[,"sampling"])
tab[k,1:8] <- round(m[nms,"mean"],2)
tab[k,11] <- round(cpt,2)
k <- k+1
tab[k,1:8] <- round(m[nms,"sd"],2)
k <- k+1
tab[k,1:8] <- round(m[nms,"n_eff"]/cpt)
tab[k,9] <- round(min(m[nmsRE,"n_eff"])/cpt)
tab[k,10] <- round(min(m[nmsBeta,"n_eff"])/cpt)
k <- k+1 
tab[k,1:8] <- round((mi[nms,"n_eff"]*(m[nms,"sd"]/mi[nms,"sd"])^2)/cpt)
tab[k,9] <- round(min((mi[nmsRE,"n_eff"]*(m[nmsRE,"sd"]/mi[nmsRE,"sd"])^2))/cpt)
tab[k,10] <- round(min((mi[nmsBeta,"n_eff"]*(m[nmsBeta,"sd"]/mi[nmsBeta,"sd"])^2))/cpt)
k <- k+1

#
f <- fit.cl
m <- as.matrix(getMonitor(f,print=F)[[1]])
mi <- as.matrix(getIntMonitor(f,print=F)[[1]])
cpt <- sum(get_CPU_time(f)[,"sampling"])
tab[k,1:8] <- round(m[nms,"mean"],2)
tab[k,11] <- round(cpt,2)
k <- k+1
tab[k,1:8] <- round(m[nms,"sd"],2)
k <- k+1
tab[k,1:8] <- round(m[nms,"n_eff"]/cpt)
tab[k,9] <- round(min(m[nmsRE,"n_eff"])/cpt)
tab[k,10] <- round(min(m[nmsBeta,"n_eff"])/cpt)
k <- k+1 
tab[k,1:8] <- round((mi[nms,"n_eff"]*(m[nms,"sd"]/mi[nms,"sd"])^2)/cpt)
tab[k,9] <- round(min((mi[nmsRE,"n_eff"]*(m[nmsRE,"sd"]/mi[nmsRE,"sd"])^2))/cpt)
tab[k,10] <- round(min((mi[nmsBeta,"n_eff"]*(m[nmsBeta,"sd"]/mi[nmsBeta,"sd"])^2))/cpt)
k <- k+1

#
f <- fit.a
m <- as.matrix(getMonitor(f,print=F)[[1]])
mi <- as.matrix(getIntMonitor(f,print=F)[[1]])
cpt <- sum(get_CPU_time(f)[,"sampling"])
tab[k,1:8] <- round(m[nms,"mean"],2)
tab[k,11] <- round(cpt,2)
k <- k+1
tab[k,1:8] <- round(m[nms,"sd"],2)
k <- k+1
tab[k,1:8] <- round(m[nms,"n_eff"]/cpt)
tab[k,9] <- round(min(m[nmsRE,"n_eff"])/cpt)
tab[k,10] <- round(min(m[nmsBeta,"n_eff"])/cpt)
k <- k+1 
tab[k,1:8] <- round((mi[nms,"n_eff"]*(m[nms,"sd"]/mi[nms,"sd"])^2)/cpt)
tab[k,9] <- round(min((mi[nmsRE,"n_eff"]*(m[nmsRE,"sd"]/mi[nmsRE,"sd"])^2))/cpt)
tab[k,10] <- round(min((mi[nmsBeta,"n_eff"]*(m[nmsBeta,"sd"]/mi[nmsBeta,"sd"])^2))/cpt)
k <- k+1
#
f <- fit.al
m <- as.matrix(getMonitor(f,print=F)[[1]])
mi <- as.matrix(getIntMonitor(f,print=F)[[1]])
cpt <- sum(get_CPU_time(f)[,"sampling"])
tab[k,1:8] <- round(m[nms,"mean"],2)
tab[k,11] <- round(cpt,2)
k <- k+1
tab[k,1:8] <- round(m[nms,"sd"],2)
k <- k+1
tab[k,1:8] <- round(m[nms,"n_eff"]/cpt)
tab[k,9] <- round(min(m[nmsRE,"n_eff"])/cpt)
tab[k,10] <- round(min(m[nmsBeta,"n_eff"])/cpt)
k <- k+1 
tab[k,1:8] <- round((mi[nms,"n_eff"]*(m[nms,"sd"]/mi[nms,"sd"])^2)/cpt)
tab[k,9] <- round(min((mi[nmsRE,"n_eff"]*(m[nmsRE,"sd"]/mi[nmsRE,"sd"])^2))/cpt)
tab[k,10] <- round(min((mi[nmsBeta,"n_eff"]*(m[nmsBeta,"sd"]/mi[nmsBeta,"sd"])^2))/cpt)
k <- k+1



print(mean(get_elapsed_time(stan.fit)[,"sample"]))
print(mean(get_CPU_time(fit.a)[,"sampling"]))
print(mean(get_CPU_time(fit.al)[,"sampling"]))
print(mean(get_CPU_time(fit.c)[,"sampling"]))
print(mean(get_CPU_time(fit.cl)[,"sampling"]))


print(xtable(tab),file="table.tex")
print(tab)

