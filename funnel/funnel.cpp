INCLUDE_BLOCK{
    
}

DATA_BLOCK{

}
SETUP_BLOCK{
    
}
VARIABLES_BLOCK{
	
	PARAMETER_VECTOR(v,2);
  GENERATED_VECTOR(gv,2);
  
}
MODEL_BLOCK{
    
    target -= 0.5*pow(v(0),2) + 0.5*pow(v(1),2)/exp(3.0*v(0)) + 1.5*v(0);

  gv=v;

}

