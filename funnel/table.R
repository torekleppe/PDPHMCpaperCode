load("Computations")


stan.min <- min(extract(out999)$x1)
print(stan.min)
print(pnorm(stan.min)*50000)
print(sum(s<stan.min))




tab <- matrix(0.0,3,5) 
tab[1,1] <- sum(get_elapsed_time( out8)[,"sample"])
tab[1,2] <- sum(get_elapsed_time( out9)[,"sample"])
tab[1,3] <- sum(get_elapsed_time( out99)[,"sample"])
tab[1,4] <- sum(get_elapsed_time( out999)[,"sample"])
tab[1,5] <- sum(get_CPU_time(fit)[,"sampling"])

m <- summary(out8)$summary
tab[2,1] <- round(m[1,"n_eff"])
m <- summary(out9)$summary
tab[2,2] <- round(m[1,"n_eff"])
m <- summary(out99)$summary
tab[2,3] <- round(m[1,"n_eff"])
m <- summary(out999)$summary
tab[2,4] <- round(m[1,"n_eff"])
m <- getMonitor(fit,print=FALSE)[[1]]
tab[2,5] <- round(m[1,"n_eff"])
tab[3,] <- round(tab[2,]/tab[1,])

print(xtable::xtable(tab),file="table.tex")
print(tab)

betas <- vector(mode="numeric",length = 10)
sam <- betas
for(i in 1:10){
  id <- fit@diagnostics[[i]][,"warmup"]==0
  sam[i] <- mean(fit@diagnostics[[i]][id,"nsamples"])
tmp <-tail(fit@diagnostics[[i]],n=1L)
betas[i] <- tmp[1,"lambdaPar0"]
}
print(min(betas))
print(max(betas))
print(min(sam))
print(max(sam))
