load("Computations")

s <- rstan::monitor(s.fit)$Rhat
print( max(s[1:(length(s)-1)]),digits = 4)

print(max(getMonitor(fit.a)[[1]][,"Rhat"]),digits=4)
print(max(getMonitor(fit.al)[[1]][,"Rhat"]),digits=4)


print(max(getMonitor(fit.c)[[1]][,"Rhat"]),digits=4)
print(max(getMonitor(fit.cl)[[1]][,"Rhat"]),digits=4)