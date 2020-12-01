

# stan part
load("Computations_stan")

m <- rstan::monitor(fit.s.d)
t.s <- sum(rstan::get_elapsed_time(fit.s.d)[,"sample"])
t.t <- sum(rstan::get_elapsed_time(fit.s.d)[,"sample"])

