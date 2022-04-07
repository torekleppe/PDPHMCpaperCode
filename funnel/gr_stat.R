load("Computations")

s <- rstan::monitor(out999)$Rhat
print( max(s[1:(length(s)-1)]))