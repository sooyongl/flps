data{
  //Sample sizes
  int<lower=1> nitemWorked;
  int<lower=1> ncov;
  int<lower=1> nstud;
  int<lower=1> nitem;
  int<lower=1> nfac;
  int<lower=0> min_k;
  int<lower=1> max_k;
  // prior information
  matrix[nitem, nfac] loading_prior;

  // indices
  int<lower=1,upper=nstud> studentM[nitemWorked];
  int<lower=1,upper=nitem> section[nitemWorked];

  // index for factor loadings
  matrix[nitem, nfac] factoridx;
  int<lower=0> firstitem[nitem];

  // data data
  int<lower=min_k,upper=max_k> grad[nitemWorked];
  matrix[nstud,ncov] X;
  int<lower=0,upper=1> Z[nstud];
  real Y[nstud];

  // Priors
  matrix[1,2] ptau0;
  matrix[nfac,2] ptau1;
  matrix[nfac,2] pomega;
}

parameters{
  // IRT model
  vector[nfac] fsc[nstud];       // person scores for each factor
  cholesky_factor_corr[nfac] L; // Cholesky decomp of corr mat of random slopes

  matrix[nitem, nfac] loading_free; // discrimination of nitem
  real intcpt[nitem];                 // difficulty of question nitem

  matrix[ncov, nfac] XF;
  vector[ncov] XY;

  real intcptY;
  vector[nfac] omega;
  real tau0;

  vector[nfac] tau1;

  real<lower=0> sigY[2];
}

transformed parameters {

  matrix[nitem, nfac] loading;

  // Factor loading constraints
  for(jjj in 1:nfac) {
    for(jj in 1:nitem) {
      if(factoridx[jj, jjj] != 0) {
        if(firstitem[jj] == 1) { // first loading per factor constrained to 1.
        loading[jj, jjj] = 1;
        } else {
          loading[jj, jjj] = loading_free[jj, jjj];
        }
      } else {
        loading[jj, jjj] = 0;
      }
    }
  };

}

model{
  real linPred[nitemWorked];

  vector[nfac] A = rep_vector(1, nfac);
  matrix[nfac, nfac] A0;

  vector[nfac] muEta[nstud];
  vector[nstud] muY0;
  vector[nstud] muY;
  real sigYI[nstud];

  L ~ lkj_corr_cholesky(nfac);
  A0 = diag_pre_multiply(A, L);

  for(i in 1:nstud){

	muEta[i] = to_vector(X[i, ]*XF);

	muY0[i] = intcptY+ to_row_vector(omega)*fsc[i] + Z[i] * (tau0 + to_row_vector(tau1)*fsc[i]);
	muY[i]  = muY0[i] + X[i,]*XY;

	sigYI[i]=sigY[Z[i]+1];
  };

    // Causal model
	fsc ~ multi_normal_cholesky(muEta, A0);
  Y ~ normal(muY,sigYI);

// Fully Latent Principal Stratification model
    // Latent variable model
	for(j in 1:nitemWorked) {
      linPred[j] = intcpt[section[j]] + loading[section[j],1:nfac] * fsc[studentM[j]];

	  grad[j] ~ bernoulli_logit(linPred[j]);
	}

     //priors
    // IRT priors
    intcpt ~ normal(0, 5);
    for(i in 1:nitem) {
      for(j in 1:nfac) {
        loading_free[i, j] ~ normal(loading_prior[i, j], 1);
      };
    };

// PS priors
  XY ~ normal(0, 5);
  intcptY ~ normal(0, 5);
  tau0  ~ normal(ptau0[1,1], ptau0[1,2]);

  for(i in 1:nfac) {
    XF[,i] ~ normal(0, 5);

	omega[i] ~ normal(pomega[i,1], pomega[i,2]);
    tau1[i] ~ normal(ptau1[i,1], ptau1[i,2]);
  };

}
// last line blank
