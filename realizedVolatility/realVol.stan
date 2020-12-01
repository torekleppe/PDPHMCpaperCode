functions{
/*
		Cholesky factorization L*L^T of symmetric tridiagonal n x n matrix G, where 
		* diag(G) = [diagFirstLastElem,diagElem,...,diagElem,diagFirstLastElem]
		* first sup/sub-diagonal elements are all equal to offDiagElem
		
		Th routine returns a vector[2*n] L where 
		* L[1:n] is the diagonal of L
		* L[n+1:2*n-1] is the sub-diagonal of L
		* L[2*n] is the log-determinant of L
	*/
	vector CIP_TriDiagChol_const1n(int n, real diagFirstLastElem, real diagElem, real offDiagElem){
		vector[2*n] L;
		real LlogDet;
		// first iteration
		L[1] = sqrt(diagFirstLastElem);
		LlogDet = log(L[1]);
		// iteration 2:n-1
		for ( t in 2:(n-1)){
			L[n+t-1] = offDiagElem/L[t-1];
			L[t] = sqrt(diagElem - pow(L[n+t-1],2));
			LlogDet += log(L[t]);
		}
		// last iteration
		L[2*n-1] = offDiagElem/L[n-1];
		L[n] = sqrt(diagFirstLastElem - pow(L[2*n-1],2));
		LlogDet += log(L[n]);
		// done Cholesky
		
		L[2*n] = LlogDet;
		return(L);
	}
	
	/*
		Cholesky factorization L*L^T of symmetric tridiagonal n x n matrix G, where 
		* diag(G) = diagElem (diagElem has length n)
		* first sup/sub-diagonal elements are all equal to offDiagElem
		
		Th routine returns a vector[2*n] L where 
		* L[1:n] is the diagonal of L
		* L[n+1:2*n-1] is the sub-diagonal of L
		* L[2*n] is the log-determinant of L
	*/
	
	
	vector CIP_TriDiagChol_diag_constod(vector diagElem, real offDiagElem){
		int n = rows(diagElem);
		vector[2*n] L;
		real LlogDet;
		L[1] = sqrt(diagElem[1]);
		LlogDet = log(L[1]);
		for ( t in 2:n){
			L[n+t-1] = offDiagElem/L[t-1];
			L[t] = sqrt(diagElem[t] - pow(L[n+t-1],2));
			LlogDet += log(L[t]);
		}
		L[2*n] = LlogDet;
		return(L);
	}
	
	/*
		Cholesky factorization L*L^T of symmetric tridiagonal n x n matrix G, where 
		* diag(G) = diagElem (diagElem has length n)
		* first sup/sub-diagonal elements are in offDiagElem ( offDiagElem has length n-1)
		
		Th routine returns a vector[2*n] L where 
		* L[1:n] is the diagonal of L
		* L[n+1:2*n-1] is the sub-diagonal of L
		* L[2*n] is the log-determinant of L
	*/
	
	
	vector CIP_TriDiagChol(vector diagElem, vector offDiagElem){
		int n = rows(diagElem);
		vector[2*n] L;
		real LlogDet;
		L[1] = sqrt(diagElem[1]);
		LlogDet = log(L[1]);
		for ( t in 2:n){
			L[n+t-1] = offDiagElem[t-1]/L[t-1];
			L[t] = sqrt(diagElem[t] - pow(L[n+t-1],2));
			LlogDet += log(L[t]);
		}
		L[2*n] = LlogDet;
		return(L);
	}
	
	/*
		Solves L^T x = b for x when L is the output of one of the tridiagonal
		Cholesky factorizations above
	*/
	vector CIP_TriDiagChol_LT_solve(vector L, vector b){
		int n = rows(b);
		vector[n] x;
		// first solve
		x[n] = b[n]/L[n];
		// remaining solves
		for ( tt in 1:(n-1)){
			x[n-tt] = (b[n-tt] - x[n-tt+1]*L[2*n-tt])/L[n-tt];
		}
		return(x);
	}
	
	/*
		Solves L x = b for x when L is the output of one of the tridiagonal
		Cholesky factorizations above
	*/
	vector CIP_TriDiagChol_L_solve(vector L, vector b){
		int n = rows(b);
		vector[n] x;
		// first solve
		x[1] = b[1]/L[1];
		// remaining solves
		for ( i in 2:n){
			x[i] = (b[i] - x[i-1]*L[n+i-1])/L[i];
		}
		return(x);
	}
	
	/*
		Solves L L^T x = G x = b for x when L is the output of one of the tridiagonal
		Cholesky factorizations above
	*/
	vector CIP_TriDiagChol_LLT_solve(vector L, vector b){
		return(CIP_TriDiagChol_LT_solve(L,CIP_TriDiagChol_L_solve(L,b)));
	}
	
}

data{
	int T;
	matrix[5*T,5] yy;
	matrix[5*T,5] yinv;
	real ldets;
}

parameters{
	vector[5] mu;
	vector<lower=0>[5] sigmaSq;
	vector<lower=-1,upper=1>[5] delta;
	vector[10] hts;
	matrix[T,5] zs;
	real<lower=6.0,upper=200.0> nu;
}

transformed parameters{

	vector[5] sigma;
	matrix[T,5] xs;
//	matrix[T,5] ytilde;
	matrix[5,5] H;
	vector[2*T] L;
	vector[T] meantmp;
	real logdet;
	real tmp;
	real ssq;
	real d1n;
	real dn;
	real od;
	real pm;
	real pm1n;
	real ytilde;
	vector[5] firstx;
	vector[5] firstz;
	vector[5] lastz;
	
	
	sigma = sqrt(sigmaSq);
	
	// build H-matrix
	for(i in 1:5){
	  H[i,i] = 1.0;
	}
	for(j in 1:4){
	  for(i in (j+1):5){
	    H[j,i] = 0.0;
	  }
	}
	H[2,1] = hts[1];
	H[3,1] = hts[2];
	H[4,1] = hts[3];
	H[5,1] = hts[4];
	
	H[3,2] = hts[5];
	H[4,2] = hts[6];
	H[5,2] = hts[7];
	
	H[4,3] = hts[8];
	H[5,3] = hts[9];
	
	H[5,4] = hts[10];
	
	
	logdet = 0.0;
	
	for(i in 1:5){
	  
	  ssq = sigmaSq[i];
    d1n = 1.0/ssq + 0.5*nu;
    dn = (1.0+square(delta[i]))/ssq + 0.5*nu;
    od = -delta[i]/ssq;
	  // lhs vector for initial iterate
    pm1n = mu[i]*(1.0-delta[i])/ssq;
    pm = mu[i]*square((delta[i]-1.0)/sigma[i]);
    
    ytilde = quad_form(yinv[1:5,1:5],H[1:5,i]);
    meantmp[1] = pm1n + 0.5*nu*log(nu/ytilde);
	  
	  for( t in 2:(T-1)){
      ytilde = quad_form(yinv[((t-1)*5+1):(t*5),1:5],H[1:5,i]);
      meantmp[t] = pm + 0.5*nu*log(nu/ytilde);
    }  
	  ytilde = quad_form(yinv[((T-1)*5+1):(T*5),1:5],H[1:5,i]);
    meantmp[T] = pm1n + 0.5*nu*log(nu/ytilde); 
    
	  L = CIP_TriDiagChol_const1n(T,d1n,dn,od);
	  xs[1:T,i] = CIP_TriDiagChol_LLT_solve(L,meantmp) + CIP_TriDiagChol_LT_solve(L,zs[1:T,i]);
	  logdet += L[2*T];
	  
	}
	
	firstx = xs[1,1:5]';
	firstz = zs[1,1:5]';
	lastz = zs[T,1:5]';
}

model{

  target += inv_gamma_lpdf(sigmaSq | 2.0, 0.5);
  target += normal_lpdf(hts| 0.0, 10.0);
  target += normal_lpdf(mu | 0.0, 5.0);
  
	for(i in 1:5) {
	  target += normal_lpdf(xs[1,i] | mu[i], sigma[i]/sqrt(1.0-square(delta[i])));
	  target += normal_lpdf(xs[2:T,i] | mu[i] + delta[i]*(xs[1:(T-1),i]-mu[i]),sigma[i]);
	}

	// common factors in the inverse wishart lpdf
	target += -0.5*(nu+6.0)*ldets + 0.5*nu*sum(xs) - 2.5*T*0.6931471805599453*nu;
	for(i in 1:5){
	  target += -T*lgamma(0.5*(nu+1.0-i));
	}
	// trace factor
	for(t in 1:T){
	  target += -0.5*trace(H * diag_pre_multiply(exp(row(xs,t)),H') * block(yinv,(t-1)*5+1,1,5,5));
	  //target += inv_wishart_lpdf(block(yy,(t-1)*5+1,1,5,5) | nu , H * diag_pre_multiply(exp(row(xs,t)),H'));
	}
	
	target += -logdet;
	
}


