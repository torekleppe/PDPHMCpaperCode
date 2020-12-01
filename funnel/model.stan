
parameters{
	real x1;
	real x2;
}

model{
	target += normal_lpdf(x1|0.0,1.0);
	target += normal_lpdf(x2|0.0,exp(1.5*x1));
}




