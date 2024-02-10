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
  //real<lower=0> sigma_p;        // hyper parameter

  real alpha;             // Intercept for class proportion
  vector[ncov] betaY;     // Coefficients for the outcome Y
  vector[ncov] gammaU;     // Coefficients for class membership

  vector[nclass] tau0;           // Intercept for Y for each class
  vector[nclass] tau1;           // Coefficient for Z for each class
  vector<lower=0>[nclass] sigY; // Standard deviations for Y for each class
  // vector<lower=0>[nitem] sigR; // Standard deviations for indicators for each class
  real<lower=0> sigR[nclass, nitem];  // Item Response
}

transformed parameters{
  vector[nstud] nu; // Probability of class membership for all students

  // Individual class membership probabilities conditional on covariates
  for (n in 1:nstud) {
    nu[n] = inv_logit(alpha + dot_product(X[n], gammaU));
  }

  // PS effects-Difference in Y on Z coefficient between classes
   real b1 = tau1[2] - tau1[1];
  // Omega-Difference in intercept in Y between classes
   real a1 = tau0[2] - tau0[1];
}

model {
 // likelihood for the outcome 'Y'
// for (n in 1:nstud) {
//    // Compute likelihood for Y
//   real mu_class1 = tau0[1] + tau1[1] * Z[n] + dot_product(X[n], betaY);
//   real mu_class2 = tau0[2] + tau1[2] * Z[n] + dot_product(X[n], betaY);
//   target += log_mix(nu[n],
//             normal_lpdf(Y[n] | mu_class1, sigY[1]),
//             normal_lpdf(Y[n] | mu_class2, sigY[2])
//             );
// }
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
      //lpdf_class[k] = log(nu[n][k]) + normal_lpdf(Y[n] | tau0[k] + tau1[k] * Z[n] + cov_eff[n], sigY[k]);
	  lpdf_class[k] = normal_lpdf(Y[n] | tau0[k] + tau1[k] * Z[n] + cov_eff[n], sigY[k]);
    }

    target += log_sum_exp(lpdf_class);
  }

 // likelihood for item data'
 for (w in 1:nitemWorked) {
   target += log_mix(nu[stud_idx[w]],
                      normal_lpdf(grad[w] | p[1,item_idx[w]], sigR[1, item_idx[w]]),
                      normal_lpdf(grad[w] | p[2,item_idx[w]], sigR[2, item_idx[w]])
               );
 }

 // Priors
 alpha ~ normal(0, 2.5);
 gammaU ~ normal(0, 2);

 betaY ~ normal(0, 2);
 tau0 ~ normal(0, 2);
 tau1 ~ normal(0, 2);
 sigY ~ cauchy(0, 2.5);

  mu_p ~ normal(0, 5);
  //sigma_p ~ cauchy(0, 2.5);
  for (k in 1:nclass) {
    for (j in 1:nitem) {
      p[k, j] ~ normal(mu_p, 1);
	    sigR[k, j] ~ cauchy(0, 2.5);
   }
 }

}

//lastline
