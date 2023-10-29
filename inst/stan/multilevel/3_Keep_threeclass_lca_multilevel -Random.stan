data {
 // Data dimensions
int<lower=1> nitemWorked;  // number of rows in long-format data
int<lower=1> nitem;        // number of items
int<lower=1> nstud;       // number of respondents

int<lower=1> ncov_lv1;        // number of covariates level 1
int<lower=1> ncov_lv2;        // number of covariates level 2
int<lower=1> nclass;      // number of latent class
int<lower=0> min_k;       // min category
int<lower=1> max_k;       // max category       

  // Item Data indices
  int stud_idx[nitemWorked];  // student index for long-format data
  int item_idx[nitemWorked];   // item index for long-format data

  // Cluster index
  int<lower=1> nsch;                  // Number of schools
  int<lower=1,upper=nsch> sch[nstud]; // Group membership for each individual


  // data data
  int<lower=min_k,upper=max_k> grad[nitemWorked]; // Item data    
  matrix[nstud, ncov_lv1] X;
  matrix[nsch, ncov_lv2] cm_X;
  
  int<lower=0, upper=1> cm_Z[nsch];
  int<lower=0, upper=1> Z[nstud];
  real Y[nstud];

  // Priors
  // prior information
  
}


parameters{

 real<lower=0, upper=1> p[nclass, nitem];  // Item Response probabilities


// Covariates effects on Outcome
vector[ncov_lv1] betaYW;     // Within-
vector[ncov_lv2] betaYB;     // Between-

// Coefficients for class membership
vector[ncov_lv1] betaUW;     // Within-
vector[ncov_lv2] betaUB;    // Between-

// Treatment effects on the outcome
vector[nclass] tau1W;   // Within-
vector[nclass] tau1B;   // Between-

// Outcome Mean differences by LC
// Overall mean ?
vector[nclass] tau0; 

// Intercept for class proportion
real alphaB[nclass];
  
// Random effects
vector[nclass] uB_Y[nsch];
vector[nclass] uB_nu[nsch];

// Standard deviations for Y for each class
vector<lower=0>[nclass] sigmaYW; // Within-
vector<lower=0>[nclass] sigmaYB; // Between-


// Standard deviations for Intercept for class proportion random effects
vector<lower=0>[nclass] sigmaNuB;

}

transformed parameters{
  matrix[nstud, nclass] eta; // Linear predictor for class membership
  vector[nclass] nu[nstud];  // Probability of class membership for all students
 
  // Individual class membership probabilities conditional on covariates
  for (n in 1:nstud) {
    int g = sch[n];  // School for individual n
    
    for (k in 1:nclass) {
      eta[n, k] = alphaB[k] 
 	            + uB_nu[g][k]
                + dot_product(cm_X[g], betaUB) 
 	            + dot_product(X[n], betaUW);
    }
    nu[n] = softmax(to_vector(eta[n, ]));
  }

}

model {
  // likelihood for the outcome 'Y' 
  for (n in 1:nstud) {
    int g = sch[n];  // School for individual n
    real lpdf_class[nclass]; // Lik by class
    
    // Precompute this if it does not change inside the inner loop
    real log_nu_n_k;  
    // Precompute dot products if they do not depend on 'k'.
    real Xn_betaYW = dot_product(X[n], betaYW);
    real cmXg_betaYB = dot_product(cm_X[g], betaYB);
    
    for (k in 1:nclass) {
      real mu_class = tau0[k] 
	                + tau1W[k] * Z[n] 
			        + Xn_betaYW // Use precomputed value
					+ uB_Y[g][k]
					+ tau1B[k] * cm_Z[g]
					+ cmXg_betaYB // Use precomputed value
					;
			log_nu_n_k = log(nu[n][k]);
			
      lpdf_class[k] = log_nu_n_k + normal_lpdf(Y[n] | mu_class, sigmaYW[k]);
    }
    
    target += log_sum_exp(lpdf_class);
  }

 // Likelihood for item data
 for (w in 1:nitemWorked) {
   real lpmf_class[nclass];
   
   for (k in 1:nclass) {
     lpmf_class[k] = log(nu[stud_idx[w]][k]) + bernoulli_lpmf(grad[w] | p[k, item_idx[w]]);
   }
   
   target += log_sum_exp(lpmf_class);
 }
 
 // Priors
 
 sigmaYW ~ cauchy(0, 2);
 sigmaYB ~ cauchy(0, 2);
 sigmaNuB ~ cauchy(0, 2);
 alphaB ~ normal(0, 1);

 for (g in 1:nsch) {
  for (k in 1:nclass) {
    uB_Y[g][k] ~ normal(0, sigmaYB[k]);
  }
   uB_nu[g] ~ normal(0, sigmaNuB);
 }
 
 
 betaUB ~ normal(0, 5);
 betaUW ~ normal(0, 5);
 
 betaYB ~ normal(0, 2);
 betaYW ~ normal(0, 2);
 
 tau0 ~ normal(0, 2);
 tau1W ~ normal(0, 1);
 tau1B ~ normal(0, 1);
 
 for (k in 1:nclass) {
    for (j in 1:nitem) {
      p[k, j] ~ beta(0.5, 0.5);
   }
 }

}
 
//lastline
