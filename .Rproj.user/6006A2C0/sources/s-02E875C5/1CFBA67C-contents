data{
//Sample sizes
  int<lower=1> nitemWorked;
  int<lower=1> nstud;
  int<lower=1> nitem;
  int<lower=0> min_k;
  int<lower=2> max_k;
  int<lower=0> ncov;
  int<lower=1> nfac;

// prior information
  matrix[nitem, nfac] lambda_prior;

// indices
  int<lower=1,upper=nstud> studentM[nitemWorked];
  int<lower=1,upper=nitem> section[nitemWorked];

// index for factor loadings --------------
  matrix[nitem, nfac] factoridx;
  int<lower=0> firstitem[nitem];

// data data
  int<lower=min_k,upper=max_k> grad[nitemWorked];
  matrix[nstud,ncov] X;
  int<lower=0,upper=1> Z[nstud];
  real Y[nstud];
}

parameters{
  vector[nfac] eta[nstud];
  cholesky_factor_corr[nfac] L;

  matrix[nitem, nfac] lambda_free;
  vector[max_k-1] tau[nitem];

  matrix[ncov, nfac] betaU;
  vector[ncov] betaY;

  real b00;
  vector[nfac] a1;
  real b0;

  vector[nfac] b1;

  real<lower=0> sigY[2];
}

transformed parameters{

  matrix[nitem, nfac] lambda;

// Factor loading constraints
  for(jjj in 1:nfac) {
    for(jj in 1:nitem) {
	  if(factoridx[jj, jjj] != 0) {
        if(firstitem[jj] == 1) {   // first loading per factor constrained to 1.
          lambda[jj, jjj] = 1;
        } else {
          lambda[jj, jjj] = lambda_free[jj, jjj];
        }
       } else {
         lambda[jj, jjj] = 0;
       }
    }
  };


}

model{
  matrix[max_k, nitemWorked] p;      // probs of reponse
  matrix[max_k, nitemWorked] s;      // logits of reponse

  vector[nfac] A = rep_vector(1, nfac);
  matrix[nfac, nfac] A0;

  vector[nfac] muEta[nstud];
  vector[nstud] muY0;
  vector[nstud] muY;
  real sigYI[nstud];

  L ~ lkj_corr_cholesky(nfac);
  A0 = diag_pre_multiply(A, L);

  for(i in 1:nstud){
 	  muEta[i] = to_vector(X[i, ]*betaU);

	  muY0[i] = b00+ to_row_vector(a1)*eta[i] + Z[i] * (b0 + to_row_vector(b1)*eta[i]);
	  muY[i]  = muY0[i] + X[i,]*betaY;

	  sigYI[i]=sigY[Z[i]+1];
  };

//priors
  // IRT priors
  for(i in 1:nitem) {
    for(ii in 1:(max_k-1)) {
	    tau[i , ii] ~ uniform(-5, 5);
    };
	  for(j in 1:nfac) {
      lambda_free[i,j] ~ normal(lambda_prior[i,j], 1);
    };
  };

  // PS priors
  betaY ~ uniform(-5, 5);
  for(i in 1:nfac) {
    betaU[,i] ~ uniform(-5, 5);
  };
  a1 ~ uniform(-5, 5);
  b1 ~ uniform(-5, 5);
  b00 ~ uniform(-5, 5);
  b0  ~ uniform(-5, 5);

// Fully Latent Principal Stratification model
  // Latent variable model
  for(i in 1:nitemWorked) {
    s[1,i] = 0; //reference
    for(k in 2:max_k) {
	    s[k,i] = s[k-1, i] + tau[section[i], k-1] + lambda[section[i], 1:nfac] * eta[studentM[i]];
	  }
    p[,i] = softmax(s[,i]);
	  grad[i] ~ categorical(p[,i]);
  }

  // Causal model
  eta ~ multi_normal_cholesky(muEta, A0);
  Y ~ normal(muY,sigYI);
}
// last line
