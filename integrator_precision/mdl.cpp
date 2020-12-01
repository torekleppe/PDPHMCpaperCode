

INCLUDE_BLOCK{
    // include here
}

DATA_BLOCK{

}

SETUP_BLOCK{
  
}

VARIABLES_BLOCK{
	PARAMETER_VECTOR(x,2);
  GENERATED_VECTOR(g,5);
}


MODEL_BLOCK{
  
    // target
    target += -pow(x(0),2) + 0.5*x(0)*x(1) - 0.125*pow(x(1),2);
  
    // generated
    g(0) = x(0);
    g(1) = x(1);
    g(2) = pow(x(0),2);
    g(3) = x(0)*x(1);
    g(4) = pow(x(1),2);
}

