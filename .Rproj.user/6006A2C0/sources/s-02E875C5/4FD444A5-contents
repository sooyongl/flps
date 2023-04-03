data{
  //Sample sizes
  int<lower=1> nitemWorked;
  int<lower=1> ncov;
  int<lower=1> nstud;
  int<lower=1> nitem;
  int<lower=1> nfac;
  // prior information
  matrix[nitem, nfac] lambda_prior;

  // indices
  int<lower=1,upper=nstud> studentM[nitemWorked];
  int<lower=1,upper=nitem> section[nitemWorked];

  // index for factor loadings
  matrix[nitem, nfac] factoridx;
  int<lower=0> firstitem[nitem];

  // data data
  real grad[nitemWorked];
  matrix[nstud,ncov] X;
  int<lower=0,upper=1> Z[nstud];
  real Y[nstud];
}

parameters{
  // IRT model
  vector[nfac] eta[nstud];
  cholesky_factor_corr[nfac] L;

  matrix[nitem, nfac] lambda_free;
  real tau[nitem];

  matrix[ncov, nfac] betaU;
  vector[ncov] betaY;

  real b00;
  vector[nfac] a1;
  real b0;

  vector[nfac] b1;
  real<lower=0> sigR;
  real<lower=0> sigY[2];
}

transformed parameters {
  matrix[nitem, nfac] lambda;

  // Factor loading constraints
  for(jjj in 1:nfac) {
    for(jj in 1:nitem) {
      if(factoridx[jj, jjj] != 0) {
        if(firstitem[jj] == 1) { // first loading per factor constrained to 1.
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
	  muEta[i] = to_vector(X[i, ]*betaU);
	  muY0[i] = b00+ to_row_vector(a1)*eta[i] + Z[i] * (b0 + to_row_vector(b1)*eta[i]);
	  muY[i]  = muY0[i] + X[i,]*betaY;
	  sigYI[i]=sigY[Z[i]+1];
  };

  for(j in 1:nitemWorked) {
    linPred[j] = tau[section[j]] + lambda[section[j],1:nfac] * eta[studentM[j]];
  }

  // model sampling
  grad ~ normal(linPred, sigR);
  eta ~ multi_normal_cholesky(muEta, A0);
  Y ~ normal(muY,sigYI);

 //priors
  // IRT priors
  tau ~ normal(0, 1);
  for(i in 1:nitem) {
    for(j in 1:nfac) {
      lambda_free[i, j] ~ normal(1, 1);
     };
  };

  // PS priors
  betaY ~ normal(0, 1);
  for(i in 1:nfac) {
    betaU[,i] ~ normal(0, 1);
  };

  a1 ~ normal(0, 1);
  b1 ~ normal(0, 1);
  b00 ~ normal(0, 1);
  b0  ~ normal(0, 1);
}
// last line blank
