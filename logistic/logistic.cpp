INCLUDE_BLOCK{
  
}

DATA_BLOCK{
    
    DATA_IVECTOR(y);
    DATA_MATRIX(X);
    double pvar;
    
}
SETUP_BLOCK{
  pvar = 100.0; 
  if(y.size() != X.rows()){
    std::cout << "Warning; dimensions of y and X does not match" << std::endl;
  }
}
VARIABLES_BLOCK{
	PARAMETER_SCALAR(alpha);
	PARAMETER_VECTOR(beta,X.cols());
	
  GENERATED_SCALAR(ga);
  GENERATED_VECTOR(gb,X.cols());
  
}
MODEL_BLOCK{
    
    // alpha prior
    target -= (0.5/pvar)*square(alpha);
    // beta prior
    target -= (0.5/pvar)*beta.dot(beta);
    // observations
    target += stan::math::bernoulli_logit_glm_lpmf( y , X, alpha, beta);
    

  
  ga = alpha;
  gb = beta;
  
}

