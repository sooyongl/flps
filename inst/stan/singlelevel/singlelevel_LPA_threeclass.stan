data {
  // Data dimensions
  int<lower=1> nitemWorked;  // number of rows in long-format data
  int<lower=1> nitem;        // number of items
  int<lower=1> nstud;       // number of respondents
  int<lower=1> ncov;        // number of covariates
  int<lower=1> nclass;      // number of latent class
  
  // Item Data indices
  int stud_idx[nitemWorked];  // student index for long-format data
  int item_idx[nitemWorked];   // item index for long-format data
  
  // data data
  real grad[nitemWorked]; // Item data      
  matrix[nstud, ncov] X;                  // Covariates
  int<lower=0, upper=1> Z[nstud];         // Treatment assignments
  real Y[nstud]; 
  
  // Priors
  // prior information
  
}

parameters{
  real p[nclass, nitem];  // Item Response
  real<lower=0, upper=1> mu_p;  // hyper parameter
  real<lower=0> sigma_p;        // hyper parameter
  
  vector[nclass] alpha;             // Intercept for class proportion
  vector[ncov] betaY;     // Coefficients for the outcome Y
  vector[ncov] gammaU;     // Coefficients for class membership
      
  vector[nclass] tau0;           // Intercept for Y for each class
  vector[nclass] tau1;           // Coefficient for Z for each class
  vector<lower=0>[nclass] sigY; // Standard deviations for Y for each class
  vector<lower=0>[nitem] sigR; // Standard deviations for indicators with equality
  
}

transformed parameters{
  matrix[nstud, nclass] eta; // Linear predictor for class membership
  vector[nclass] nu[nstud];  // Probability of class membership for all students

  // Individual class membership probabilities conditional on covariates
  for (n in 1:nstud) {
    for (k in 1:nclass) {
      eta[n, k] = alpha[k] + dot_product(X[n], gammaU);
    }
    nu[n] = softmax(to_vector(eta[n, ]));
  }

}

model {
  // likelihood for the outcome 'Y' 
  vector[nstud] cov_eff;
  for (n in 1:nstud) {
    cov_eff[n] = dot_product(X[n], betaY);
  }
  
  for (n in 1:nstud) {
    real lpdf_class[nclass];
    //real cov_eff = dot_product(X[n], betaY);
    
    for (k in 1:nclass) {
      //real mu_class = tau00[k] + tau01[k] * Z[n] + cov_eff[n];
      lpdf_class[k] = log(nu[n][k]) + normal_lpdf(Y[n] | tau0[k] + tau1[k] * Z[n] + cov_eff[n], sigY[k]);
    }
    
    target += log_sum_exp(lpdf_class);
  }

 // Likelihood for item data
 for (w in 1:nitemWorked) {
   real lpmf_class[nclass];
   
   for (k in 1:nclass) {
     lpmf_class[k] = log(nu[stud_idx[w]][k]) + normal_lpdf(grad[w] | p[k, item_idx[w]], sigR[item_idx[w]]);
   }
   
   target += log_sum_exp(lpmf_class);
 }
 
 // Priors
 alpha ~ normal(0, 2.5);
 gammaU ~ normal(0, 5);
 
 betaY ~ normal(0, 2);
 tau0 ~ normal(0, 2);
 tau1 ~ normal(0, 1);
 sigY ~ cauchy(0, 2.5);
 
  mu_p ~ normal(0, 2);
  sigma_p ~ cauchy(0, 2.5);;
  for (k in 1:nclass) {
    for (j in 1:nitem) {
      p[k, j] ~ normal(mu_p, sigma_p);
   }
 }
}
 
//lastline
