data{
  int n;
  int p;
  int y[n];
  matrix[n,p] X;
}

parameters{
  real alpha;
  vector[p] beta;
}

transformed parameters{
 
}

model{
  
  target += -(0.5/100.0)*square(alpha);
  target += -(0.5/100.0)*dot_self(beta);
  target += bernoulli_logit_glm_lpmf( y | X, alpha, beta);
}

