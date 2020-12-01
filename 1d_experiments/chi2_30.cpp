INCLUDE_BLOCK{
    
}

DATA_BLOCK{
  
}
SETUP_BLOCK{
    
}
VARIABLES_BLOCK{
	
	PARAMETER_SCALAR(x);
	
  GENERATED_SCALAR(gx);
  GENERATED_SCALAR(gx2);
  
}
MODEL_BLOCK{
    double k = 30.0;
    gx=x;
    gx2 = square(x);
    target += (0.5*k-1.0)*log(k+sqrt(2.0*k)*x) - sqrt(0.5*k)*x;
  

}

