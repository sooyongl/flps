data {
  // Data dimensions
  int<lower=1> nitemWorked;  // number of rows in long-format data
  int<lower=1> nitem;        // number of items
  int<lower=1> nstud;        // number of respondents
  int<lower=1> ncov;        // number of covariates
  int<lower=1> nfac;         // number of latent factors
  int<lower=0> min_k;       // min category
  int<lower=1> max_k;       // max category       

  // Item Data indices
  int stud_idx[nitemWorked];  // student index for long-format data
  int item_idx[nitemWorked];   // item index for long-format data

  // Index for factor loadings
  matrix[nitem, nfac] factoridx;
  int<lower=0> firstitem[nitem];

  // data data
  int<lower=min_k,upper=max_k> grad[nitemWorked]; // Item data    
  matrix[nstud, ncov] X;                  // Covariates
  int<lower=0, upper=1> Z[nstud];         // Treatment assignments
  real Y[nstud]; 

  // Priors
  // prior information
  matrix[nitem, nfac] loading_prior;
  matrix[1,2] ptau0;
  matrix[nfac,2] ptau1;
  matrix[nfac,2] pomega;
}

     
parameters{
  // IRT model
  vector[nfac] fsc[nstud];       // person scores for each factor
  cholesky_factor_corr[nfac] L;  // Cholesky decomp of corr mat of random slopes

  matrix[nitem, nfac] loading_free;      // Item slopes
  real intcpt[nitem];               // Item intercepts

  matrix[ncov, nfac] betaU;
  vector[ncov] betaY;

  real intcptY;
  vector[nfac] omega;
  real tau0;
  vector[nfac] tau1;

  real<lower=0> sigY;
}
 
transformed parameters{
 
  matrix[nitem, nfac] loading;

  // Factor loading constraints
  for (i in 1:nitem) {
    for (j in 1:nfac) {
      if (factoridx[i, j] != 0) {
        loading[i, j] = (firstitem[i] == 1) ? 1 : loading_free[i, j];
      } else {
        loading[i, j] = 0;
      }
    }
  }
}
 
model {
 real linPred[nitemWorked];
 vector[nfac] A = rep_vector(1, nfac);
 matrix[nfac, nfac] A0;
 vector[nfac] muEta[nstud];
 vector[nstud] muY0;
 vector[nstud] muY;

 // Prior on correlation matrix
 L ~ lkj_corr_cholesky(nfac);
 A0 = diag_pre_multiply(A, L);

  // FLPS model
  for(i in 1:nstud) {
    muEta[i] = to_vector(X[i, ] * betaU);
    muY0[i] = intcptY 
            + dot_product(to_row_vector(omega), fsc[i]) 
            + Z[i] * (tau0 + dot_product(to_row_vector(tau1), fsc[i]));
    muY[i] = muY0[i] + dot_product(X[i, ], betaY);
    
  }

  // Likelihoods
  // Outcome
  Y ~ normal(muY, sigY);

  // Latent variable model
  fsc ~ multi_normal_cholesky(muEta, A0);
    for(j in 1:nitemWorked) {
    linPred[j] = intcpt[item_idx[j]] 
               + dot_product(loading[item_idx[j], 1:nfac], fsc[stud_idx[j]]);
    grad[j] ~ bernoulli_logit(linPred[j]);
  }

  //priors
  // Priors for IRT
  intcpt ~ normal(0, 2.5);
  for(i in 1:nitem) {
    for(j in 1:nfac) {
      loading_free[i, j] ~ normal(loading_prior[i, j], 1);
    }
  }

  // Priors for PS
  betaY ~ normal(0, 5);
  intcptY ~ normal(0, 5);
  tau0 ~ normal(ptau0[1, 1], ptau0[1, 2]);

  for(i in 1:nfac) {
    betaU[:, i] ~ normal(0, 5);

    omega[i] ~ normal(pomega[i, 1], pomega[i, 2]);
    tau1[i] ~ normal(ptau1[i, 1], ptau1[i, 2]);
  }

}
 
//lastline
