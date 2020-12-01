data{
  int n;
  real vr;
}



parameters{
	vector[n] x;
}

transformed parameters{
  real mn;
  mn = square(x[1]);
}


model{
  target += -0.5*mn;
  
	for(i in 2:n) target += -(0.5/vr)*square(x[i]-mn); 
}

generated quantities{
  
}




