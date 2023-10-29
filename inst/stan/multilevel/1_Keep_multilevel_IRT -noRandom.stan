data {
   // Data dimensions
  int<lower=1> nitemWorked;  // number of rows in long-format data
  int<lower=1> nitem;        // number of items
  int<lower=1> nstud;        // number of respondents
  
  int<lower=1> ncov_lv1;        // number of covariates level 1
  int<lower=1> ncov_lv2;        // number of covariates level 2
  int<lower=1> nfac;         // number of latent factors
  int<lower=0> min_k;       // min category
  int<lower=1> max_k;       // max category       

  // Item Data indices
  int stud_idx[nitemWorked];  // student index for long-format data
  int item_idx[nitemWorked];   // item index for long-format data

  // Cluster index
  int<lower=1> nsch;                  // Number of schools
  int<lower=1,upper=nsch> sch[nstud]; // Group membership for each individual


  // Index for factor loadings
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
  vector[nfac] fsc[nstud]; // person scores for each factor
  cholesky_factor_corr[nfac] L; // Cholesky decomp of corr mat of random slopes

  matrix[nitem, nfac] loading_free;      // Item slopes
  real intcpt[nitem];               // Item intercepts

  // Covariates effects on Outcome
  matrix[ncov_lv1, nfac] betaUW;     // Within-
  
  vector[ncov_lv1] betaYW;     // Within-
  vector[ncov_lv2] betaYB;     // Between-
  
  real intcptY;
  vector[nfac] omega;
  
  real tau0W;
  real tau0B;
  vector[nfac] tau1;
  
  real<lower=0> sigY;
  real<lower=0> sigYB;
  
  vector[nsch] uY;
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
    
    int g = sch[i];  // School for individual i
    
    muEta[i] = to_vector(X[i, ] * betaUW);
   
    muY[i] = intcptY
           + uY[g]
           + dot_product(cm_X[g, ], betaYB)
           + cm_Z[g] * tau0B
           + dot_product(to_row_vector(omega), fsc[i]) 
           + Z[i] * (tau0_W + dot_product(to_row_vector(tau1), fsc[i]))
           + dot_product(X[i, ], betaYW);
           
  }

  // Likelihoods
  // Outcome
  Y ~ normal(muY, sigY);
  uY ~ normal(0, sigYB);


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
  betaYW ~ normal(0, 5);
  betaYB ~ normal(0, 5);
  intcptY ~ normal(0, 5);
  
  tau0W ~ normal(ptau0[1, 1], ptau0[1, 2]);
  tau0B ~ normal(ptau0[1, 1], ptau0[1, 2]);
  
  for(i in 1:nfac) {
    betaUW[:, i] ~ normal(0, 5);
    
    omega[i] ~ normal(pomega[i, 1], pomega[i, 2]);
    tau1[i] ~ normal(ptau1[i, 1], ptau1[i, 2]);
  }
}
 
//lastline
