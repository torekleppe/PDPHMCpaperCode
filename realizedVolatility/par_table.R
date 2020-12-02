


load("Computations_stan")

m <- as.matrix(rstan::monitor(fit.s.d))



nms <- paste0("mu[",1:5,"]")
nmp <- paste0("mu_g(",0:4,")")
nms <- c(nms,paste0("sigma[",1:5,"]"))
nmp <- c(nmp,paste0("sigma_g(",0:4,")"))
nms <- c(nms,paste0("delta[",1:5,"]"))
nmp <- c(nmp,paste0("phi_g(",0:4,")"))
nms <- c(nms,paste0("hts[",1:10,"]"))
nmp <- c(nmp,paste0("h_g(",0:9,")"))
nms <- c(nms,"nu")
nmp <- c(nmp,"nu_g")
tab <- cbind(round(m[nms,"mean"],2),
             round(m[nms,"sd"],3))

load("Computations_vari")
fit <- fit.p.c
mp <- as.matrix(pdphmc::getMonitor(fit)[[1]])
mpi <- as.matrix(pdphmc::getIntMonitor(fit)[[1]])

tab <- cbind(tab,
             round(mp[nmp,"mean"],2),
             round(mpi[nmp,"mean"],2),
             round(mp[nmp,"sd"],3)
             )

fit <- fit.p.cl
mp <- as.matrix(pdphmc::getMonitor(fit)[[1]])
mpi <- as.matrix(pdphmc::getIntMonitor(fit)[[1]])

tab <- cbind(tab,
             round(mp[nmp,"mean"],2),
             round(mpi[nmp,"mean"],2),
             round(mp[nmp,"sd"],3)
)

print(tab)