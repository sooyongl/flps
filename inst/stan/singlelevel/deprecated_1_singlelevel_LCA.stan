data {
  // Data dimensions
  int<lower=1> nitemWorked;  // number of rows in long-format data
  int<lower=1> nitem;        // number of items
  int<lower=1> nstud;       // number of respondents
  int<lower=1> ncov;        // number of covariates
  int<lower=1> nclass;      // number of latent class
  int<lower=0> min_k;       // min category
  int<lower=1> max_k;       // max category       

  // Item Data indices
  int stud_idx[nitemWorked];  // student index for long-format data
  int item_idx[nitemWorked];   // item index for long-format data

  // data data
  int<lower=min_k,upper=max_k> grad[nitemWorked]; // Item data    
  matrix[nstud, ncov] X;                  // Covariates
  int<lower=0, upper=1> Z[nstud];         // Treatment assignments
  real Y[nstud]; 

  // Priors
  // prior information
  
}
     
parameters{
 //real<lower=0, upper=1> p[nclass, nitem];  // Item Response probabilities
 //real<lower=0, upper=1> mu_p;  // hyper parameter
 real<lower=-3, upper=3> mu_p;  // hyper parameter
 real<lower=0> sigma_p;        // hyper parameter
 
  ordered[nclass] p[nitem];   // log-odds of answering 1 for each item in each class
  //positive_ordered[nclass] lambda; 
 
 real alpha[nclass];             // Intercept for class proportion
 vector[ncov] betaY;     // Coefficients for the outcome Y
 vector[ncov] gammaU;     // Coefficients for class membership

 vector[nclass] tau0;           // Intercept for Y for each class
 vector[nclass] tau1;           // Coefficient for Z for each class
 vector<lower=0>[nclass] sigY; // Standard deviations for Y for each class
}
 
transformed parameters{
  //vector[nstud] nu; // Probability of class membership for all students
  matrix[nstud, nclass] eta; // Linear predictor for class membership
  vector[nclass] nu[nstud];  // Probability of class membership for all students
  
  // Individual class membership probabilities conditional on covariates
  //for (n in 1:nstud) {
  //  nu[n] = inv_logit(alpha + dot_product(X[n], gammaU));
  //}
  
  for (n in 1:nstud) {
    for (k in 1:nclass) {
      eta[n, k] = alpha[k] + dot_product(X[n], gammaU);
    }
    nu[n] = softmax(to_vector(eta[n, ]));
  }

  // PS effects-Difference in Y on Z coefficient between classes
  //real b1 = tau1[2] - tau1[1]; 
  // Omega-Difference in intercept in Y between classes
  //real a1 = tau0[2] - tau0[1]; 
}
 
model {

//vector[nclas] mu_class[nstdu];

matrix[nclass, nstud] mu_class;
//matrix[nclass, nstud] item_block;
//matrix[nclass, nstud] distal_block;
//matrix[nclass, nstud] total;
vector[nclass] item_block[nstud];
vector[nclass] distal_block[nstud];
vector[nclass] total[nstud];

// likelihood for the outcome 'Y' 
 for(cc in 1:nclass) {
    for (n in 1:nstud) {
    // Compute likelihood for Y
		mu_class[cc, n] = tau0[cc] + tau1[cc] * Z[n] + dot_product(X[n], betaY);
		distal_block[n,cc] += normal_lpdf(Y[n] | mu_class[cc, n], sigY[cc]);
   }
   
   for (w in 1:nitemWorked) {
	item_block[stud_idx[w],cc] += bernoulli_logit_lpmf(grad[w] | p[item_idx[w], cc]);
   }
 }

for (i in 1:nstud) {
    for (j in 1:nclass) {
        total[i][j] = item_block[i][j] + distal_block[i][j];
    }
}

target += log_mix(nu, total);
 
 // Priors
 alpha ~ normal(0, 2.5);
 gammaU ~ normal(0, 5.5);
 
 betaY ~ normal(0, 2.5);
 tau0 ~ normal(0, 2.5);
 tau1 ~ normal(0, 2.5);
 sigY ~ cauchy(0, 2.5);
  
  mu_p ~ beta(2, 2);
  sigma_p ~ beta(0.5, 0.5);
  for(i in 1:nitem){
	pi[i] ~ normal(mu_p, sigma_p);
  }
//for (k in 1:nclass) {
//  for (j in 1:nitem) {
//    p[k, j] ~ beta(mu_p * (1/sigma_p^2 - 1), (1 - mu_p) * (1/sigma_p^2 - 1));
//   }
// }

}
 
//lastline
