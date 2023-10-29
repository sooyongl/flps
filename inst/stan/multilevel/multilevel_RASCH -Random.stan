data{
  // Data dimensions
  int<lower=1> nitemWorked;  // number of rows in long-format data
  int<lower=1> nitem;        // number of items
  int<lower=1> nstud;       // number of respondents
  int<lower=1> ncov_lv1;        // number of covariates level 1
  int<lower=1> ncov_lv2;        // number of covariates level 2
  int<lower=1> nfac;        // number of latent factors
  int<lower=0> min_k;       // min category
  int<lower=1> max_k;       // max category
  
  // Item Data indices
  int stud_idx[nitemWorked];  // student index for long-format data
  int item_idx[nitemWorked];   // item index for long-format data
  
  // Cluster index
  int<lower=1> nsch;                  // Number of schools
  int<lower=1,upper=nsch> sch[nstud]; // Group membership for each individual
  
  // index for factor loadings
  matrix[nitem, nfac] factoridx;
  int<lower=0> firstitem[nitem];
  
  // data data
  int<lower=min_k,upper=max_k> grad[nitemWorked]; // Item data
  matrix[nstud, ncov_lv1] X;
  matrix[nsch, ncov_lv2] cm_X;
  
  int<lower=0, upper=1> cm_Z[nsch];
  int<lower=0, upper=1> Z[nstud];
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
  vector[nfac] fscW[nstud]; // person scores for each factor
  cholesky_factor_corr[nfac] LW;  // Cholesky decomp of corr mat of random slopes
  vector[nfac] fscB[nsch]; // person scores for each factor
  cholesky_factor_corr[nfac] LB;  // Cholesky decomp of corr mat of random slopes
  
  real intcpt[nitem];               // Item intercepts
  
  // Covariates effects on Outcome
  matrix[ncov_lv1, nfac] betaUW;     // Within-
  matrix[ncov_lv2, nfac] betaUB;     // Between-

  //vector[nfac] betaUW[ncov_lv1];     // Within-
  //vector[nfac] betaUB[ncov_lv2];     // Between-
  
  vector[ncov_lv1] betaYW;     // Within-
  vector[ncov_lv2] betaYB;     // Between-
  
  real intcptY;
  vector[nfac] omegaW;
  vector[nfac] omegaB;
  
  real tau0W;
  real tau0B;
  
  vector[nfac] tau1W;
  vector[nfac] tau1B;
  
  real<lower=0> sigY;
  real<lower=0> sigYB;
  
  vector[nsch] uY;
  vector[nitem] ugrad[nsch];
}

model{
  real linPred[nitemWorked];
  
  vector[nfac] AW = rep_vector(1, nfac);
  matrix[nfac, nfac] A0W;
  
  vector[nfac] AB = rep_vector(1, nfac);
  matrix[nfac, nfac] A0B;
  
  vector[nfac] muEtaW[nstud];
  vector[nfac] muEtaB[nsch];
  
  vector[nstud] muY0;
  vector[nstud] muY;
  
  // FLPS model
  for(i in 1:nstud) {
    
    int g = sch[i];  // School for individual i
    
    muEtaW[i] =  to_vector(X[i, ] * betaUW);
    muEtaB[g] =  to_vector(cm_X[g, ] * betaUB);
    
    muY[i] = intcptY
           + uY[g]
           
           + dot_product(to_row_vector(omegaB), fscB[g]) 
           + cm_Z[g] * tau0B 
           + cm_Z[g] * dot_product(to_row_vector(tau1B), fscB[g])
           + dot_product(cm_X[g, ], betaYB)
           
           + dot_product(to_row_vector(omegaW), fscW[i]) 
           + Z[i] * tau0W 
           + Z[i] * dot_product(to_row_vector(tau1W), fscW[i])
           + dot_product(X[i, ], betaYW);
    
  }
  
  // Likelihoods
  // Outcome
  Y ~ normal(muY, sigY);
  uY ~ normal(0, sigYB);
  for (g in 1:nsch) {
    ugrad[g] ~ normal(0, 1);
  }

  // Latent variable model
  // Prior on correlation matrix
  LW ~ lkj_corr_cholesky(nfac);
  A0W = diag_pre_multiply(AW, LW);
  
  LB ~ lkj_corr_cholesky(nfac);
  A0B = diag_pre_multiply(AB, LB);

  fscW ~ multi_normal_cholesky(muEtaW, A0W);
  fscB ~ multi_normal_cholesky(muEtaB, A0B);
  
  for(j in 1:nitemWorked) {
    int g = sch[stud_idx[j]];  // School for individual i
    
    linPred[j] = ugrad[g][item_idx[j]] +
               + intcpt[item_idx[j]] 
               + dot_product(factoridx[item_idx[j], 1:nfac], 
                            to_row_vector(fscW[stud_idx[j]] + fscB[g])
                            )
    ;
    grad[j] ~ bernoulli_logit(linPred[j]);
  }
  
  
  
  //priors
  // Priors for IRT
  intcpt ~ normal(0, 2.5);
  
  // Priors for PS
  betaYW ~ normal(0, 5);
  betaYB ~ normal(0, 5);
  intcptY ~ normal(0, 5);
  
  tau0W ~ normal(ptau0[1, 1], ptau0[1, 2]);
  tau0B ~ normal(ptau0[1, 1], ptau0[1, 2]);
  
  
  for(i in 1:nfac) {
    betaUW[, i] ~ normal(0, 5);
    betaUB[, i] ~ normal(0, 5);
    
    omegaW[i] ~ normal(pomega[i, 1], pomega[i, 2]);
    omegaB[i] ~ normal(pomega[i, 1], pomega[i, 2]);
    
    tau1W[i] ~ normal(ptau1[i, 1], ptau1[i, 2]);
    tau1B[i] ~ normal(ptau1[i, 1], ptau1[i, 2]);
    
  }
}
// last line blank