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
    double df = 20.0;
    gx=x;
    gx2 = square(x);
    target -= 0.5*(df+1.0)*log(1.0+square(x*sqrt(df/(df-2.0)))/df);
  

}

