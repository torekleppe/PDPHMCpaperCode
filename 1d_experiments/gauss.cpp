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
    gx=x;
    gx2 = square(x);
    target -= 0.5*pow(x,2);
  

}

