require(pdphmc)
require(rstan)
l <- read.table("set2.dat")

y <- as.vector(l[,1])
X <- as.matrix(l[,3:26])
glmfit<- glm.fit(x=l[,2:26],y=y,family = binomial())  # for reference only
n <- dim(X)[1]
p <- dim(X)[2]

X2 <- matrix(0.0,nrow=n,ncol=p+round(p*(p-1)/2))
X2[,1:p] <- X
k <- p+1
for(i in 1:(p-1)){
  for(j in (i+1):p){
    X2[,k] <- X[,i]*X[,j]
    k <- k+1
  }
}

p2 <- dim(X2)[2]

X3 <- matrix(0.0,nrow=n,ncol=2*p)
X3[,1:p] <- X
X3[,(p+1):(2*p)] <- X^2

Xused <- X2

fit.stan <- stan("logistic.stan",data=list(n=dim(Xused)[1],p=dim(Xused)[2],y=as.integer(y),X=Xused))

mdl <- pdphmc.build("logistic.cpp",lambda="arclength")
fit <- run(mdl,data=list(y=as.integer(y),X=Xused),Tmax=20000.0)


