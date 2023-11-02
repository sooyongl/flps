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

// Treatment effects on the outcome
vector[nclass] tau1W;   // Within-
real tau1B;   // Between

// Outcome Mean differences by LC
// Overall mean ?
vector[nclass] tau0; 

// Intercept for class proportion
real alphaB; 

// Random effects
vector[nsch] uB_Y1;
vector[nsch] uB_Y2;

// Standard deviations for Y for each class
vector<lower=0>[nclass] sigmaYW; // Within-
vector<lower=0>[nclass] sigmaYB; // Between-

// Standard deviations for Intercept for class proportion random effects
}
 
transformed parameters{
 vector[nstud] nu; // Probability of class membership for all nstud

// Individual class membership probabilities conditional on covariates
 for (i in 1:nstud) {

    nu[i] = inv_logit(alphaB 
                    + dot_product(X[i], betaUW)
                    );

}

// PS effects-Difference in Y on Z coefficient between classes
// real b1W = tau1W[2] - tau1W[1]; 

// Omega-Difference in intercept in Y between classes
// real a1 = tau0[2] - tau0[1]; 
}
 
model {
 //  Outcome model with random effects
// likelihood for the outcome 'Y' 
for (i in 1:nstud) {
    int g = sch[i];  // School for individual n
  // Compute likelihood for Y
  real mu_class1 = tau0[1] 
                   + uB_Y1[g]
                   
                   + tau1W[1] * Z[i] 
                   + tau1B * cm_Z[g] 
                   + dot_product(cm_X[g], betaYB)
                   + dot_product(X[i], betaYW)
                   ;
                   
  real mu_class2 = tau0[2]
                   + uB_Y2[g]
                   
                   + tau1W[2] * Z[i] 
                   + tau1B * cm_Z[g]
                   + dot_product(cm_X[g], betaYB)
                   + dot_product(X[i], betaYW)
                   ;
                   
  target += log_mix(nu[i], 
                    normal_lpdf(Y[i] | mu_class1, sigmaYW[1]),
                    normal_lpdf(Y[i] | mu_class2, sigmaYW[2])
                    );
}

// likelihood for item data'
for (w in 1:nitemWorked) {
  
  target += log_mix(nu[stud_idx[w]], 
                     bernoulli_lpmf(grad[w] | p[1, item_idx[w]]),
                     bernoulli_lpmf(grad[w] | p[2, item_idx[w]])
  );
}

// Priors
for (k in 1:nclass) {
  for (j in 1:nitem) {
    p[k, j] ~ beta(0.5,0.5); // Jeffreys' Prior:
  }
}

sigmaYW ~ cauchy(0, 2);
sigmaYB ~ cauchy(0, 2);

betaYW ~ normal(0, 2);
betaYB ~ normal(0, 1);

betaUW ~ normal(0, 1);

tau1W ~ normal(0, 1);
tau1B ~ normal(0, 1);

tau0 ~ normal(0, 1);
alphaB ~ normal(0, 1);

uB_Y1 ~ normal(0, sigmaYB[1]);
uB_Y2 ~ normal(0, sigmaYB[2]);
}
 
//lastline
