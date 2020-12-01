
ess.table <- matrix(NaN,20,10)

# stan part
load("Computations_stan")

m <- rstan::monitor(fit.s.d)
t.s <- sum(rstan::get_elapsed_time(fit.s.d)[,"sample"])
t.t <- sum(rstan::get_elapsed_time(fit.s.d))

cc <- 1


nms <- rownames(m)
id <- grep("mu[",nms,fixed=TRUE)

ess.table[1,cc] <- t.s
ess.table[1,cc+1] <- t.t
ess.table[2,cc] <- min(m[id,"n_eff"])
ess.table[2,cc+1] <- max(m[id,"n_eff"])
ess.table[3,cc] <- ess.table[2,cc]/t.s
ess.table[3,cc+1] <- ess.table[2,cc+1]/t.s
ess.table[4,cc] <- ess.table[2,cc]/t.t
ess.table[4,cc+1] <- ess.table[2,cc+1]/t.t

id <- grep("sigma[",nms,fixed=TRUE)
ess.table[5,cc] <- min(m[id,"n_eff"])
ess.table[5,cc+1] <- max(m[id,"n_eff"])
ess.table[6,cc] <- ess.table[5,cc]/t.s
ess.table[6,cc+1] <- ess.table[5,cc+1]/t.s
ess.table[7,cc] <- ess.table[5,cc]/t.t
ess.table[7,cc+1] <- ess.table[5,cc+1]/t.t

id <- grep("delta[",nms,fixed=TRUE)
ess.table[8,cc] <- min(m[id,"n_eff"])
ess.table[8,cc+1] <- max(m[id,"n_eff"])
ess.table[9,cc] <- ess.table[8,cc]/t.s
ess.table[9,cc+1] <- ess.table[8,cc+1]/t.s
ess.table[10,cc] <- ess.table[8,cc]/t.t
ess.table[10,cc+1] <- ess.table[8,cc+1]/t.t



load("Computations_vari")
fit <- fit.p.cl
m <- pdphmc::getMonitor(fit)[[1]]
t.s <- sum(pdphmc::get_CPU_time(fit)[,"sampling"])
t.t <- sum(pdphmc::get_CPU_time(fit))
cc <- 3 
nms <- rownames(m)
id <- grep("mu(",nms,fixed=TRUE)

ess.table[1,cc] <- t.s
ess.table[1,cc+1] <- t.t
ess.table[2,cc] <- min(m[id,"n_eff"])
ess.table[2,cc+1] <- max(m[id,"n_eff"])
ess.table[3,cc] <- ess.table[2,cc]/t.s
ess.table[3,cc+1] <- ess.table[2,cc+1]/t.s
ess.table[4,cc] <- ess.table[2,cc]/t.t
ess.table[4,cc+1] <- ess.table[2,cc+1]/t.t

id <- grep("sigma_g(",nms,fixed=TRUE)
ess.table[5,cc] <- min(m[id,"n_eff"])
ess.table[5,cc+1] <- max(m[id,"n_eff"])
ess.table[6,cc] <- ess.table[5,cc]/t.s
ess.table[6,cc+1] <- ess.table[5,cc+1]/t.s
ess.table[7,cc] <- ess.table[5,cc]/t.t
ess.table[7,cc+1] <- ess.table[5,cc+1]/t.t

id <- grep("phi_g(",nms,fixed=TRUE)
ess.table[8,cc] <- min(m[id,"n_eff"])
ess.table[8,cc+1] <- max(m[id,"n_eff"])
ess.table[9,cc] <- ess.table[8,cc]/t.s
ess.table[9,cc+1] <- ess.table[8,cc+1]/t.s
ess.table[10,cc] <- ess.table[8,cc]/t.t
ess.table[10,cc+1] <- ess.table[8,cc+1]/t.t

print(round(ess.table,2))