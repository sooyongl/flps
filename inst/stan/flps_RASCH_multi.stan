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
  matrix[nitem, nfac] lambda_prior;

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
}

transformed data {
   matrix[nitem, nfac] lambda = factoridx;

}

parameters{
  // IRT model
  vector[nfac] eta[nstud];       // person scores for each factor
  cholesky_factor_corr[nfac] L; // Cholesky decomp of corr mat of random slopes

  real tau[nitem];                 // difficulty of question nitem

  matrix[ncov, nfac] betaU;
  vector[ncov] betaY;

  real b00;
  vector[nfac] a1;
  real b0;

  vector[nfac] b1;

  real<lower=0> sigY[2];
}

transformed parameters {
  real linPred[nitemWorked];

  for(j in 1:nitemWorked) {
    linPred[j] = tau[section[j]] + lambda[section[j],1:nfac] * eta[studentM[j]];
  }
}

model{
  vector[nfac] A = rep_vector(1, nfac);
  matrix[nfac, nfac] A0;

  vector[nfac] muEta[nstud];
  vector[nstud] muY0;
  vector[nstud] muY;
  real sigYI[nstud];

  L ~ lkj_corr_cholesky(nfac);
  A0 = diag_pre_multiply(A, L);

  for(i in 1:nstud){
    //useEff[i] = to_row_vector(a1)*eta[i];
    //trtEff[i] = b0 + to_row_vector(b1)*eta[i];
    //muY[i]=b00+useEff[i]+Z[i]*trtEff[i];

	muEta[i] = to_vector(X[i, ]*betaU);

	muY0[i] = b00+ to_row_vector(a1)*eta[i] + Z[i] * (b0 + to_row_vector(b1)*eta[i]);
	muY[i]  = muY0[i] + X[i,]*betaY;

	sigYI[i]=sigY[Z[i]+1];
  };

//priors
    // IRT priors
    //tau ~ normal(0, 1);
	tau ~ uniform(-5, 5);

    // PS priors
    betaY ~ uniform(-5, 5);
    //betaU ~ uniform(-5, 5);
    for(i in 1:nfac) {
      betaU[,i] ~ uniform(-5, 5);
    };

    a1 ~ uniform(-5, 5);
    b1 ~ uniform(-5, 5);
    b00 ~ uniform(-5, 5);
    b0  ~ uniform(-5, 5);

// Fully Latent Principal Stratification model
    // Latent variable model
    grad~bernoulli_logit(linPred);
    // Causal model
    //for(i in 1:nstud){
    //  eta[i, ] ~ multi_normal_cholesky(X[i, ]*betaU, A0);
    //}
	eta ~ multi_normal_cholesky(muEta, A0);
    Y~normal(muY,sigYI);
}
