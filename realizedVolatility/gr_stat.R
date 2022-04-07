rm(list=ls())
load("Computations_stan")

s <- rstan::monitor(fit.s.d)$Rhat
print( max(s[1:(length(s)-1)]),digits = 4)


rm(list=ls())
load("Computations_b1")
fit <- fit.p.cl
print(max(getMonitor(fit)[[1]][,"Rhat"]),digits=4)

rm(list=ls())
load("Computations_b2")
fit <- fit.p.al

print(max(getMonitor(fit)[[1]][,"Rhat"]),digits=4)
