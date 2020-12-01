
#load("Computations")
library("xtable")
tab <- matrix(0.0,9,9)
m <- monitor(fit.stan)
t.stan <- sum(get_elapsed_time(fit.stan)[,"sample"])
tab[1,1] <- min(m[1:25,"n_eff"])
tab[1,2] <- round(tab[1,1]/t.stan)
tab[1,3] <- median(m[1:25,"n_eff"])
tab[1,4] <- round(tab[1,3]/t.stan)
tab[1,5] <- max(m[1:25,"n_eff"])
tab[1,6] <- round(tab[1,5]/t.stan)
tab[1,7] <- round(t.stan,2)

r <- 1

fit <- fit.c

r <- r+1
t <- sum(get_CPU_time(fit)[,"sampling"])
m <- getMonitor(fit)[[1]]
tab[r,1] <- min(m[1:25,"n_eff"])
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- median(m[1:25,"n_eff"])
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- max(m[1:25,"n_eff"])
tab[r,6] <- round(tab[r,5]/t)
tab[r,7] <- round(t,2)

r <- r+1
mi <- getIntMonitor(fit)[[1]]
NE <- round(mi[1:25,"n_eff"]*(m[1:25,"sd"]/mi[1:25,"sd"])^2)
tab[r,1] <- min(NE)
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- median(NE)
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- max(NE)
tab[r,6] <- round(tab[r,5]/t)
#tab[r,7] <- t
tab[r,8] <- round(min((m[1:25,"sd"]/mi[1:25,"sd"])^2),2)
tab[r,9] <- round(max((m[1:25,"sd"]/mi[1:25,"sd"])^2),2)

fit <- fit.cl

r <- r+1
t <- sum(get_CPU_time(fit)[,"sampling"])
m <- getMonitor(fit)[[1]]
tab[r,1] <- min(m[1:25,"n_eff"])
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- median(m[1:25,"n_eff"])
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- max(m[1:25,"n_eff"])
tab[r,6] <- round(tab[r,5]/t)
tab[r,7] <- round(t,2)

r <- r+1
mi <- getIntMonitor(fit)[[1]]
NE <- round(mi[1:25,"n_eff"]*(m[1:25,"sd"]/mi[1:25,"sd"])^2)
tab[r,1] <- min(NE)
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- median(NE)
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- max(NE)
tab[r,6] <- round(tab[r,5]/t)
#tab[r,7] <- round(t,2)
tab[r,8] <- round(min((m[1:25,"sd"]/mi[1:25,"sd"])^2),2)
tab[r,9] <- round(max((m[1:25,"sd"]/mi[1:25,"sd"])^2),2)

fit <- fit.a

r <- r+1
t <- sum(get_CPU_time(fit)[,"sampling"])
m <- getMonitor(fit)[[1]]
tab[r,1] <- min(m[1:25,"n_eff"])
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- median(m[1:25,"n_eff"])
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- max(m[1:25,"n_eff"])
tab[r,6] <- round(tab[r,5]/t)
tab[r,7] <- round(t,2)

r <- r+1
mi <- getIntMonitor(fit)[[1]]
NE <- round(mi[1:25,"n_eff"]*(m[1:25,"sd"]/mi[1:25,"sd"])^2)
tab[r,1] <- min(NE)
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- median(NE)
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- max(NE)
tab[r,6] <- round(tab[r,5]/t)
#tab[r,7] <- t
tab[r,8] <- round(min((m[1:25,"sd"]/mi[1:25,"sd"])^2),2)
tab[r,9] <- round(max((m[1:25,"sd"]/mi[1:25,"sd"])^2),2)

fit <- fit.al

r <- r+1
t <- sum(get_CPU_time(fit)[,"sampling"])
m <- getMonitor(fit)[[1]]
tab[r,1] <- min(m[1:25,"n_eff"])
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- median(m[1:25,"n_eff"])
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- max(m[1:25,"n_eff"])
tab[r,6] <- round(tab[r,5]/t)
tab[r,7] <- round(t,2)

r <- r+1
mi <- getIntMonitor(fit)[[1]]
NE <- round(mi[1:25,"n_eff"]*(m[1:25,"sd"]/mi[1:25,"sd"])^2)
tab[r,1] <- min(NE)
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- median(NE)
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- max(NE)
tab[r,6] <- round(tab[r,5]/t)
#tab[r,7] <- t
tab[r,8] <- round(min((m[1:25,"sd"]/mi[1:25,"sd"])^2),2)
tab[r,9] <- round(max((m[1:25,"sd"]/mi[1:25,"sd"])^2),2)


print(xtable(tab),file="table.tex")
print(tab)

