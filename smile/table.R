#load("Computations")
library("xtable")
tab <- matrix(0.0,9,9)
m <- monitor(s.fit)
t.stan <- sum(get_elapsed_time( s.fit)[,"sample"])
tab[1,1] <- m["x[1]","n_eff"]
tab[1,2] <- round(tab[1,1]/t.stan)
tab[1,3] <- min(m[2:11,"n_eff"])
tab[1,4] <- round(tab[1,3]/t.stan)
tab[1,5] <- max(m[2:11,"n_eff"])
tab[1,6] <- round(tab[1,5]/t.stan)
tab[1,7] <- round(m["x[1]","mean"],2)
tab[1,8] <- round(m["x[2]","mean"],2)
tab[1,9] <- round(t.stan,2)



r <- 2
m <- getMonitor(fit.c,print=FALSE)[[1]]
t <- sum(get_CPU_time(fit.c)[,"sampling"])
tab[r,1] <- round(m["x(0)","n_eff"])
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- min(m[2:11,"n_eff"])
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- max(m[2:11,"n_eff"])
tab[r,6] <- round(tab[r,5]/t)
tab[r,7] <- round(m["x(0)","mean"],2)
tab[r,8] <- round(m["x(1)","mean"],2)
tab[r,9] <- round(t,2)

r <- 3
mi <- getIntMonitor(fit.c,print=FALSE)[[1]]
t <- sum(get_CPU_time(fit.c)[,"sampling"])
tab[r,1] <- round(mi["gx(0)","n_eff"]*(m["x(0)","sd"]/mi["gx(0)","sd"])^2)
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- round(min(mi[2:11,"n_eff"]*(m[2:11,"sd"]/mi[2:11,"sd"])^2))
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- round(max(mi[2:11,"n_eff"]*(m[2:11,"sd"]/mi[2:11,"sd"])^2))
tab[r,6] <- round(tab[r,5]/t)
tab[r,7] <- round(mi["gx(0)","mean"],2)
tab[r,8] <- round(mi["gx(1)","mean"],2)



r <- 4
m <- getMonitor(fit.cl,print=FALSE)[[1]]
t <- sum(get_CPU_time(fit.c)[,"sampling"])
tab[r,1] <- round(m["x(0)","n_eff"])
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- min(m[2:11,"n_eff"])
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- max(m[2:11,"n_eff"])
tab[r,6] <- round(tab[r,5]/t)
tab[r,7] <- round(m["x(0)","mean"],2)
tab[r,8] <- round(m["x(1)","mean"],2)
tab[r,9] <- round(t,2)

r <- 5
mi <- getIntMonitor(fit.cl,print=FALSE)[[1]]
t <- sum(get_CPU_time(fit.c)[,"sampling"])
tab[r,1] <- round(mi["gx(0)","n_eff"]*(m["x(0)","sd"]/mi["gx(0)","sd"])^2)
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- round(min(mi[2:11,"n_eff"]*(m[2:11,"sd"]/mi[2:11,"sd"])^2))
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- round(max(mi[2:11,"n_eff"]*(m[2:11,"sd"]/mi[2:11,"sd"])^2))
tab[r,6] <- round(tab[r,5]/t)
tab[r,7] <- round(mi["gx(0)","mean"],2)
tab[r,8] <- round(mi["gx(1)","mean"],2)

r <- 6
m <- getMonitor(fit.a,print=FALSE)[[1]]
t <- sum(get_CPU_time(fit.c)[,"sampling"])
tab[r,1] <- round(m["x(0)","n_eff"])
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- min(m[2:11,"n_eff"])
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- max(m[2:11,"n_eff"])
tab[r,6] <- round(tab[r,5]/t)
tab[r,7] <- round(m["x(0)","mean"],2)
tab[r,8] <- round(m["x(1)","mean"],2)
tab[r,9] <- round(t,2)

r <- 7
mi <- getIntMonitor(fit.a,print=FALSE)[[1]]
t <- sum(get_CPU_time(fit.c)[,"sampling"])
tab[r,1] <- round(mi["gx(0)","n_eff"]*(m["x(0)","sd"]/mi["gx(0)","sd"])^2)
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- round(min(mi[2:11,"n_eff"]*(m[2:11,"sd"]/mi[2:11,"sd"])^2))
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- round(max(mi[2:11,"n_eff"]*(m[2:11,"sd"]/mi[2:11,"sd"])^2))
tab[r,6] <- round(tab[r,5]/t)
tab[r,7] <- round(mi["gx(0)","mean"],2)
tab[r,8] <- round(mi["gx(1)","mean"],2)


r <- 8
m <- getMonitor(fit.al,print=FALSE)[[1]]
t <- sum(get_CPU_time(fit.c)[,"sampling"])
tab[r,1] <- round(m["x(0)","n_eff"])
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- min(m[2:11,"n_eff"])
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- max(m[2:11,"n_eff"])
tab[r,6] <- round(tab[r,5]/t)
tab[r,7] <- round(m["x(0)","mean"],2)
tab[r,8] <- round(m["x(1)","mean"],2)
tab[r,9] <- round(t,2)

r <- 9
mi <- getIntMonitor(fit.al,print=FALSE)[[1]]
t <- sum(get_CPU_time(fit.c)[,"sampling"])
tab[r,1] <- round(mi["gx(0)","n_eff"]*(m["x(0)","sd"]/mi["gx(0)","sd"])^2)
tab[r,2] <- round(tab[r,1]/t)
tab[r,3] <- round(min(mi[2:11,"n_eff"]*(m[2:11,"sd"]/mi[2:11,"sd"])^2))
tab[r,4] <- round(tab[r,3]/t)
tab[r,5] <- round(max(mi[2:11,"n_eff"]*(m[2:11,"sd"]/mi[2:11,"sd"])^2))
tab[r,6] <- round(tab[r,5]/t)
tab[r,7] <- round(mi["gx(0)","mean"],2)
tab[r,8] <- round(mi["gx(1)","mean"],2)



print(xtable(tab),file="table.tex")
print(tab)

