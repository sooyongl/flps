data {
 // Data dimensions
int<lower=1> nitemWorked;  // number of rows in long-format data
int<lower=1> nitem;        // number of items
int<lower=1> nstud;       // number of respondents

int<lower=1> ncov_lv1;        // number of covariates level 1
int<lower=1> ncov_lv2;        // number of covariates level 2
int<lower=1> nclass;      // number of latent class

  // Item Data indices
  int stud_idx[nitemWorked];  // student index for long-format data
  int item_idx[nitemWorked];   // item index for long-format data

  // Cluster index
  int<lower=1> nsch;                  // Number of schools
  int<lower=1,upper=nsch> sch[nstud]; // Group membership for each individual


  // data data
  real grad[nitemWorked]; // Item data      
  matrix[nstud, ncov_lv1] X;
  matrix[nsch, ncov_lv2] cm_X;
  
  int<lower=0, upper=1> cm_Z[nsch];
  int<lower=0, upper=1> Z[nstud];
  real Y[nstud];

  // Priors
  // prior information
  
}


parameters{

 real p[nclass, nitem];  // Item Response

// Covariates effects on Outcome
vector[ncov_lv1] betaYW;     // Within-
vector[ncov_lv2] betaYB;     // Between-

// Coefficients for class membership
vector[ncov_lv1] betaUW;     // Within-

// Treatment effects on the outcome
vector[nclass] b01_W;   // Within-
real b01_B;   // Within-

// Outcome Mean differences by LC
// Overall mean ?
vector[nclass] b00; 

// Intercept for class proportion
real alphaB_nu[nclass];
  
// Random effects
vector[nclass] uB_Y[nsch];

// Standard deviations for Y for each class
vector<lower=0>[nclass] sigmaYW; // Within-
vector<lower=0>[nclass] sigmaYB; // Between-

vector<lower=0>[nitem] sigR; // Standard deviations for indicators with equality
}

transformed parameters{
  matrix[nstud, nclass] eta; // Linear predictor for class membership
  vector[nclass] nu[nstud];  // Probability of class membership for all students
 
  // Individual class membership probabilities conditional on covariates
  for (n in 1:nstud) {
        
    for (k in 1:nclass) {
      eta[n, k] = alphaB_nu[k] 
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
      real mu_class = b00[k] 
	                + b01_W[k] * Z[n] 
					+ Xn_betaYW // Use precomputed value
					+ uB_Y[g][k]
					+ b01_B * cm_Z[g]
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
     lpmf_class[k] = log(nu[stud_idx[w]][k]) + normal_lpdf(grad[w] | p[k, item_idx[w]], sigR[item_idx[w]]);
   }
   
   target += log_sum_exp(lpmf_class);
 }
 
 // Priors
 
 sigmaYW ~ cauchy(0, 2);
 sigmaYB ~ cauchy(0, 2);
 alphaB_nu ~ normal(0, 1);

 for (g in 1:nsch) {
  for (k in 1:nclass) {
    uB_Y[g][k] ~ normal(0, sigmaYB[k]);
  }
 }
 
 betaUW ~ normal(0, 5);
 
 betaYB ~ normal(0, 2);
 betaYW ~ normal(0, 2);
 
 b00 ~ normal(0, 2);
 b01_W ~ normal(0, 1);
 b01_B ~ normal(0, 1);
 
  for (k in 1:nclass) {
    for (j in 1:nitem) {
      p[k, j] ~ normal(0, 5);
   }
 }
 
}
 
//lastline
