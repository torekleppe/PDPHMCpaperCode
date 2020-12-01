INCLUDE_BLOCK{
  
}

DATA_BLOCK{
    DATA_ISCALAR(n);
  DATA_SCALAR(vr);
}
SETUP_BLOCK{
    
}
VARIABLES_BLOCK{
	
	PARAMETER_VECTOR(x,n);
	

  
  GENERATED_VECTOR(gx,n);
  
  
}
MODEL_BLOCK{
  var resMean = square(x(0));
  target = -0.5*resMean;
  
  for(int i=1;i<n;i++) target -= (0.5/vr)*square(x(i)-resMean);
  
  gx = doubleValue(x);
}

