INCLUDE_BLOCK{
  
}

DATA_BLOCK{
    
  DATA_IVECTOR(y);
  DATA_VECTOR(fW);
  DATA_VECTOR(mW);
  DATA_VECTOR(WW);
  DATA_VECTOR(fall);
  DATA_IVECTOR(f1);
  DATA_IVECTOR(m1);
  DATA_IVECTOR(f2);
  DATA_IVECTOR(m2);
  
    
    Eigen::MatrixXd fPri(2,2);
}
SETUP_BLOCK{
    
  fPri(0,0) = 1.0/1.244;
  fPri(1,0) = 0.0;
  fPri(0,1) = 0.0;
  fPri(1,1) = 1.0/1.244;
  
}
VARIABLES_BLOCK{
  
  PARAMETER_VECTOR(lamF_bar,2);
  PARAMETER_VECTOR(lamM_bar,2);
  PARAMETER_SCALAR(V1F_bar);
  PARAMETER_SCALAR(V1M_bar);
  
  
  PARAMETER_SCALAR(logkappaM_bar);
  PARAMETER_SCALAR(logkappaF_bar);
  
  PARAMETER_VECTOR(zf1,40);
  PARAMETER_VECTOR(zm1,40);
  PARAMETER_VECTOR(zf3,20);
  PARAMETER_VECTOR(zm3,20);
  PARAMETER_VECTOR(beta,5);
	
	
	GENERATED_SCALAR(tau1f); 
	GENERATED_SCALAR(tau1m);
	GENERATED_SCALAR(tau2f);
	GENERATED_SCALAR(tau2m);
	GENERATED_SCALAR(rhof);
	GENERATED_SCALAR(rhom);
	
	GENERATED_SCALAR(tau3f);
	GENERATED_SCALAR(tau3m);
	
	GENERATED_VECTOR(gb1f,40);
	GENERATED_VECTOR(gb1m,40);
	GENERATED_VECTOR(gb3f,20);
	GENERATED_VECTOR(gb3m,20);
	
	GENERATED_VECTOR(gbeta,5);
	
	
	
}
MODEL_BLOCK{
  
  VectorXv lamF(2);
  VectorXv lamM(2);
  
  var V1F;
  var V1M;
  
  var LV1F;
  var LV1M;
  
  MatrixXv WF(2,2);
  MatrixXv WM(2,2);
  
  var logkappaM;
  var logkappaF;
  var kappaM;
  var kappaF;
  
  
  VectorXv b1f(40);
  VectorXv b1m(40);
  VectorXv b3f(20);
  VectorXv b3m(20);
  
  VectorXv eta(360);
  
  var detf;
  var wt1f;
  var wt12f;
  var wt2f;
  var detm;
  var wt1m;
  var wt12m;
  var wt2m;
  
  var stdkm;
  var stdkf;
  
  for(int i = 1; i<=2; i++){
    lamF[i-1] =  lamF_bar[i-1]/sqrt(3.0-static_cast<double>(i)+10.0);
    lamM[i-1] =  lamM_bar[i-1]/sqrt(3.0-static_cast<double>(i)+10.0);
  }
  
  LV1F = sqrt(exp(lamF[0])/fPri(1,1) + 20.0*exp(lamF[0]-lamF[1]));
  V1F = V1F_bar/LV1F;
  LV1M = sqrt(exp(lamM[0])/fPri(1,1) + 20.0*exp(lamM[0]-lamM[1]));
  V1M = V1M_bar/LV1M;
  
  WF(0,0) = exp(lamF[0]);
  WF(0,1) = WF(0,0)*V1F;
  WF(1,0) = WF(0,1);
  WF(1,1) = WF(0,0)*pow(V1F,2) + exp(lamF[1]);
  
  
  
  
  WM(0,0) = exp(lamM[0]);
  WM(0,1) = WM(0,0)*V1M;
  WM(1,0) = WM(0,1);
  WM(1,1) = WM(0,0)*pow(V1M,2) + exp(lamM[1]);
  
  
  
  logkappaF = logkappaF_bar/sqrt(1.0+10.0);
  logkappaM = logkappaM_bar/sqrt(1.0+10.0);
  
  kappaF = exp(logkappaF);
  kappaM = exp(logkappaM);
  
  
  
  detf = WF(0,0)*WF(1,1) - pow(WF(0,1),2);
  wt1f = sqrt(WF(1,1)/detf);
  wt12f = - WF(0,1)/(detf*wt1f);
  wt2f = sqrt(1.0/WF(1,1));
  
  detm = WM(0,0)*WM(1,1) - pow(WM(0,1),2);
  wt1m = sqrt(WM(1,1)/detm);
  wt12m = - WM(0,1)/(detm*wt1m);
  wt2m = sqrt(1.0/WM(1,1));
  
  stdkm = 1.0/sqrt(kappaM);
  stdkf = 1.0/sqrt(kappaF);
  
  for(int i = 1; i<=20;i++){
    b1f[i-1] = wt1f*zf1[i-1];
    b1f[i+20-1] = wt12f*zf1[i-1] + wt2f*zf1[20+i-1];
    b1m[i-1] = wt1m*zm1[i-1];
    b1m[i+20-1] = wt12m*zm1[i-1] + wt2m*zm1[20+i-1];
    b3f[i-1] = stdkf*zf3[i-1];
    b3m[i-1] = stdkm*zm3[i-1];
  }
  
  
  for(int i = 1; i<=240; i++){
    eta[i-1] = beta[0] + beta[1]*fW[i-1] + beta[2]*mW[i-1] + beta[3]*WW[i-1] + beta[4]*fall[i-1] 
    + b1f[f1[i-1]-1] + b1m[m1[i-1]-1]; 
  }
  
  for(int i = 241; i<=360; i++){
    
    eta[i-1] = beta[0] + beta[1]*fW[i-1] + beta[2]*mW[i-1] + beta[3]*WW[i-1] + beta[4]*fall[i-1] 
    + b3f[f2[i-240-1]-1] + b3m[m2[i-240-1]-1];
  }
  
  
  // gamma prior on exp(lambda)
  for(int i=1;i<=2;i++){
    target += (3.0-static_cast<double>(i))*lamF[i-1] - 0.5*exp(lamF[i-1])/fPri(i-1,i-1);
    target += (3.0-static_cast<double>(i))*lamM[i-1] - 0.5*exp(lamM[i-1])/fPri(i-1,i-1);
  }
  // normal prior on V
  target += -0.5*pow(V1F*exp(0.5*lamF[0])/sqrt(fPri(1,1)),2);
  target += -0.5*pow(V1M*exp(0.5*lamM[0])/sqrt(fPri(1,1)),2);
  
  
  target += logkappaF - 0.622*kappaF;
  target += logkappaM - 0.622*kappaM;
  
  
  target += stan::math::normal_lpdf(zf1 , 0.0,1.0);
  target += stan::math::normal_lpdf(zm1 , 0.0,1.0);
  target += stan::math::normal_lpdf(zf3 , 0.0,1.0);
  target += stan::math::normal_lpdf(zm3 , 0.0,1.0);
  
  target += stan::math::bernoulli_logit_lpmf(y , eta);
  
  target += -log(LV1F) -log(LV1M);
  
  // generated quantities
  
  tau1f = 1.0/pow(wt1f,2);
  tau1m = 1.0/pow(wt1m,2);
  tau2f = detf/WF(0,0);
  tau2m = detm/WM(0,0);
  rhof = -WF(0,1)/sqrt(WF(0,0)*WF(1,1));
  rhom = -WM(0,1)/sqrt(WM(0,0)*WM(1,1));
  
  tau3f = kappaF;
  tau3m = kappaM;
  
  gb1f = b1f;
  gb1m = b1m;
  gb3f = b3f;
  gb3m = b3m;
  
  gbeta = beta;
  
}

