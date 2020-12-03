
rm(list=ls())

ess.table <- matrix(NaN,22,10)

# stan part
load("Computations_stan")

m <- rstan::monitor(fit.s.d)
t.s <- sum(rstan::get_elapsed_time(fit.s.d)[,"sample"])
t.t <- sum(rstan::get_elapsed_time(fit.s.d))

cc <- 1


nms <- rownames(m)
id <- grep("mu[",nms,fixed=TRUE)

ess.table[1,cc] <- round(t.s)
ess.table[1,cc+1] <- round(t.t)
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

id <- grep("hts[",nms,fixed=TRUE)
ess.table[11,cc] <- min(m[id,"n_eff"])
ess.table[11,cc+1] <- max(m[id,"n_eff"])
ess.table[12,cc] <- ess.table[11,cc]/t.s
ess.table[12,cc+1] <- ess.table[11,cc+1]/t.s
ess.table[13,cc] <- ess.table[11,cc]/t.t
ess.table[13,cc+1] <- ess.table[11,cc+1]/t.t

id <- grep("nu",nms,fixed=TRUE)
ess.table[14,cc] <- min(m[id,"n_eff"])
ess.table[14,cc+1] <- max(m[id,"n_eff"])
ess.table[15,cc] <- ess.table[14,cc]/t.s
ess.table[15,cc+1] <- ess.table[14,cc+1]/t.s
ess.table[16,cc] <- ess.table[14,cc]/t.t
ess.table[16,cc+1] <- ess.table[14,cc+1]/t.t

id <- grep("firstz",nms,fixed=TRUE)
ess.table[17,cc] <- min(m[id,"n_eff"])
ess.table[17,cc+1] <- max(m[id,"n_eff"])
ess.table[18,cc] <- ess.table[17,cc]/t.s
ess.table[18,cc+1] <- ess.table[17,cc+1]/t.s
ess.table[19,cc] <- ess.table[17,cc]/t.t
ess.table[19,cc+1] <- ess.table[17,cc+1]/t.t

id <- grep("firstx",nms,fixed=TRUE)
ess.table[20,cc] <- min(m[id,"n_eff"])
ess.table[20,cc+1] <- max(m[id,"n_eff"])
ess.table[21,cc] <- ess.table[20,cc]/t.s
ess.table[21,cc+1] <- ess.table[20,cc+1]/t.s
ess.table[22,cc] <- ess.table[20,cc]/t.t
ess.table[22,cc+1] <- ess.table[20,cc+1]/t.t


load("Computations_b1")
# discrete samples
fit <- fit.p.cl
m <- pdphmc::getMonitor(fit)[[1]]
t.s <- sum(pdphmc::get_CPU_time(fit)[,"sampling"])
t.t <- sum(pdphmc::get_CPU_time(fit))
cc <- 3 
nms <- rownames(m)
id <- grep("mu_g(",nms,fixed=TRUE)

ess.table[1,cc] <- round(t.s)
ess.table[1,cc+1] <- round(t.t)
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

id <- grep("h_g(",nms,fixed=TRUE)
ess.table[11,cc] <- min(m[id,"n_eff"])
ess.table[11,cc+1] <- max(m[id,"n_eff"])
ess.table[12,cc] <- ess.table[11,cc]/t.s
ess.table[12,cc+1] <- ess.table[11,cc+1]/t.s
ess.table[13,cc] <- ess.table[11,cc]/t.t
ess.table[13,cc+1] <- ess.table[11,cc+1]/t.t

id <- grep("nu_g",nms,fixed=TRUE)
ess.table[14,cc] <- min(m[id,"n_eff"])
ess.table[14,cc+1] <- max(m[id,"n_eff"])
ess.table[15,cc] <- ess.table[14,cc]/t.s
ess.table[15,cc+1] <- ess.table[14,cc+1]/t.s
ess.table[16,cc] <- ess.table[14,cc]/t.t
ess.table[16,cc+1] <- ess.table[14,cc+1]/t.t

id <- grep("firstz(",nms,fixed=TRUE)
ess.table[17,cc] <- min(m[id,"n_eff"])
ess.table[17,cc+1] <- max(m[id,"n_eff"])
ess.table[18,cc] <- ess.table[17,cc]/t.s
ess.table[18,cc+1] <- ess.table[17,cc+1]/t.s
ess.table[19,cc] <- ess.table[17,cc]/t.t
ess.table[19,cc+1] <- ess.table[17,cc+1]/t.t

id <- grep("firstx(",nms,fixed=TRUE)
ess.table[20,cc] <- min(m[id,"n_eff"])
ess.table[20,cc+1] <- max(m[id,"n_eff"])
ess.table[21,cc] <- ess.table[20,cc]/t.s
ess.table[21,cc+1] <- ess.table[20,cc+1]/t.s
ess.table[22,cc] <- ess.table[20,cc]/t.t
ess.table[22,cc+1] <- ess.table[20,cc+1]/t.t

#integrated samples
cc <- cc+2
mi <- pdphmc::getIntMonitor(fit)[[1]]
inms <- rownames(mi)
fac <- (m[inms,"sd"]/mi[,"sd"])^2
m <- mi
m[,"n_eff"]<-round(m[,"n_eff"]*fac)
nms <- rownames(m)
id <- grep("mu_g(",nms,fixed=TRUE)

ess.table[1,cc] <- round(t.s)
ess.table[1,cc+1] <- round(t.t)
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

id <- grep("h_g(",nms,fixed=TRUE)
ess.table[11,cc] <- min(m[id,"n_eff"])
ess.table[11,cc+1] <- max(m[id,"n_eff"])
ess.table[12,cc] <- ess.table[11,cc]/t.s
ess.table[12,cc+1] <- ess.table[11,cc+1]/t.s
ess.table[13,cc] <- ess.table[11,cc]/t.t
ess.table[13,cc+1] <- ess.table[11,cc+1]/t.t

id <- grep("nu_g",nms,fixed=TRUE)
ess.table[14,cc] <- min(m[id,"n_eff"])
ess.table[14,cc+1] <- max(m[id,"n_eff"])
ess.table[15,cc] <- ess.table[14,cc]/t.s
ess.table[15,cc+1] <- ess.table[14,cc+1]/t.s
ess.table[16,cc] <- ess.table[14,cc]/t.t
ess.table[16,cc+1] <- ess.table[14,cc+1]/t.t

id <- grep("firstz(",nms,fixed=TRUE)
ess.table[17,cc] <- min(m[id,"n_eff"])
ess.table[17,cc+1] <- max(m[id,"n_eff"])
ess.table[18,cc] <- ess.table[17,cc]/t.s
ess.table[18,cc+1] <- ess.table[17,cc+1]/t.s
ess.table[19,cc] <- ess.table[17,cc]/t.t
ess.table[19,cc+1] <- ess.table[17,cc+1]/t.t

id <- grep("firstx(",nms,fixed=TRUE)
ess.table[20,cc] <- min(m[id,"n_eff"])
ess.table[20,cc+1] <- max(m[id,"n_eff"])
ess.table[21,cc] <- ess.table[20,cc]/t.s
ess.table[21,cc+1] <- ess.table[20,cc+1]/t.s
ess.table[22,cc] <- ess.table[20,cc]/t.t
ess.table[22,cc+1] <- ess.table[20,cc+1]/t.t



load("Computations_b2")

fit <- fit.p.al
m <- pdphmc::getMonitor(fit)[[1]]
t.s <- sum(pdphmc::get_CPU_time(fit)[,"sampling"])
t.t <- sum(pdphmc::get_CPU_time(fit))
cc <- cc+2 
nms <- rownames(m)
id <- grep("mu_g(",nms,fixed=TRUE)

ess.table[1,cc] <- round(t.s)
ess.table[1,cc+1] <- round(t.t)
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

id <- grep("h_g(",nms,fixed=TRUE)
ess.table[11,cc] <- min(m[id,"n_eff"])
ess.table[11,cc+1] <- max(m[id,"n_eff"])
ess.table[12,cc] <- ess.table[11,cc]/t.s
ess.table[12,cc+1] <- ess.table[11,cc+1]/t.s
ess.table[13,cc] <- ess.table[11,cc]/t.t
ess.table[13,cc+1] <- ess.table[11,cc+1]/t.t

id <- grep("nu_g",nms,fixed=TRUE)
ess.table[14,cc] <- min(m[id,"n_eff"])
ess.table[14,cc+1] <- max(m[id,"n_eff"])
ess.table[15,cc] <- ess.table[14,cc]/t.s
ess.table[15,cc+1] <- ess.table[14,cc+1]/t.s
ess.table[16,cc] <- ess.table[14,cc]/t.t
ess.table[16,cc+1] <- ess.table[14,cc+1]/t.t

id <- grep("firstz(",nms,fixed=TRUE)
ess.table[17,cc] <- min(m[id,"n_eff"])
ess.table[17,cc+1] <- max(m[id,"n_eff"])
ess.table[18,cc] <- ess.table[17,cc]/t.s
ess.table[18,cc+1] <- ess.table[17,cc+1]/t.s
ess.table[19,cc] <- ess.table[17,cc]/t.t
ess.table[19,cc+1] <- ess.table[17,cc+1]/t.t

id <- grep("firstx(",nms,fixed=TRUE)
ess.table[20,cc] <- min(m[id,"n_eff"])
ess.table[20,cc+1] <- max(m[id,"n_eff"])
ess.table[21,cc] <- ess.table[20,cc]/t.s
ess.table[21,cc+1] <- ess.table[20,cc+1]/t.s
ess.table[22,cc] <- ess.table[20,cc]/t.t
ess.table[22,cc+1] <- ess.table[20,cc+1]/t.t

#integrated samples
cc <- cc+2
mi <- pdphmc::getIntMonitor(fit)[[1]]
inms <- rownames(mi)
fac <- (m[inms,"sd"]/mi[,"sd"])^2
m <- mi
m[,"n_eff"]<-round(m[,"n_eff"]*fac)
nms <- rownames(m)
id <- grep("mu_g(",nms,fixed=TRUE)

ess.table[1,cc] <- round(t.s)
ess.table[1,cc+1] <- round(t.t)
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

id <- grep("h_g(",nms,fixed=TRUE)
ess.table[11,cc] <- min(m[id,"n_eff"])
ess.table[11,cc+1] <- max(m[id,"n_eff"])
ess.table[12,cc] <- ess.table[11,cc]/t.s
ess.table[12,cc+1] <- ess.table[11,cc+1]/t.s
ess.table[13,cc] <- ess.table[11,cc]/t.t
ess.table[13,cc+1] <- ess.table[11,cc+1]/t.t

id <- grep("nu_g",nms,fixed=TRUE)
ess.table[14,cc] <- min(m[id,"n_eff"])
ess.table[14,cc+1] <- max(m[id,"n_eff"])
ess.table[15,cc] <- ess.table[14,cc]/t.s
ess.table[15,cc+1] <- ess.table[14,cc+1]/t.s
ess.table[16,cc] <- ess.table[14,cc]/t.t
ess.table[16,cc+1] <- ess.table[14,cc+1]/t.t

id <- grep("firstz(",nms,fixed=TRUE)
ess.table[17,cc] <- min(m[id,"n_eff"])
ess.table[17,cc+1] <- max(m[id,"n_eff"])
ess.table[18,cc] <- ess.table[17,cc]/t.s
ess.table[18,cc+1] <- ess.table[17,cc+1]/t.s
ess.table[19,cc] <- ess.table[17,cc]/t.t
ess.table[19,cc+1] <- ess.table[17,cc+1]/t.t

id <- grep("firstx(",nms,fixed=TRUE)
ess.table[20,cc] <- min(m[id,"n_eff"])
ess.table[20,cc+1] <- max(m[id,"n_eff"])
ess.table[21,cc] <- ess.table[20,cc]/t.s
ess.table[21,cc+1] <- ess.table[20,cc+1]/t.s
ess.table[22,cc] <- ess.table[20,cc]/t.t
ess.table[22,cc+1] <- ess.table[20,cc+1]/t.t



rownames(ess.table) <- c("cputime",
                         rep("mu",3),
                         rep("sigma",3),
                         rep("delta",3),
                         rep("h",3),
                         rep("nu",3),
                         rep("firstz",3),
                         rep("firstx",3))

#print(xtable::xtable(ess.table),file="table.tex")

tab.str <- "\\begin{table} \n \\begin{tabular}{ccccc}\n"
for(i in 1:dim(ess.table)[1]){
  for(j in 1:5){
    tab.str <- paste0(tab.str, "(",round(ess.table[i,2*j-1],2),",",round(ess.table[i,2*j],2),")")
    if(j==5 ){
      if(i<dim(ess.table)[1]) tab.str <- paste0(tab.str,"\\\\")
      tab.str <- paste0(tab.str,"\n ")
      
    } else {
      tab.str <- paste0(tab.str," & ")
    }
  }
}
tab.str <- paste0(tab.str,"\\end{tabular} \n \\end{table} \n")
cat(tab.str,file="table.tex")
print(round(ess.table,2))