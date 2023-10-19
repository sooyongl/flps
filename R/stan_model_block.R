#' Model block generator
#'
#' model_stan('mixture', T, 2)
#' model_stan('irt', T, 2)
#' @noRd
model_stan <- function(type = 'irt', cate = TRUE, level = 1, lv_randomeffect = FALSE) {

  if(type %in% c("lca","lpa")) {

    p_sig <-  ifelse(cate,
"                     bernoulli_lpmf(grad[w] | p[1, item_idx[w]]),
                      bernoulli_lpmf(grad[w] | p[2, item_idx[w]])",

"                     normal_lpdf(grad[w] | p[1,item_idx[w]], sigR[item_idx[w]]),
                      normal_lpdf(grad[w] | p[2,item_idx[w]], sigR[item_idx[w]])")

    prior_type <- ifelse(cate,
                         "
  mu_p ~ beta(2, 2);
  sigma_p ~ beta(0.5, 0.5);
  for (k in 1:nclass) {
    for (j in 1:nitem) {
      p[k, j] ~ beta(mu_p * (1/sigma_p^2 - 1), (1 - mu_p) * (1/sigma_p^2 - 1));
   }
 }",
                         "
  mu_p ~ normal(0, 2);
  sigma_p ~ cauchy(0, 2.5);;
  for (k in 1:nclass) {
    for (j in 1:nitem) {
      p[k, j] ~ normal(mu_p, sigma_p);
   }
 }")

    if(level == 1) {
      script <- glue(
        "
  // likelihood for the outcome 'Y'
  for (n in 1:nstud) {
    // Compute likelihood for Y
    real mu_class1 = tau0[1] + tau1[1] * Z[n] + dot_product(X[n], betaY);
    real mu_class2 = tau0[2] + tau1[2] * Z[n] + dot_product(X[n], betaY);
    target += log_mix(nu[n],
              normal_lpdf(Y[n] | mu_class1, sigY[1]),
              normal_lpdf(Y[n] | mu_class2, sigY[2])
              );
  }

  // likelihood for item data'
  for (w in 1:nitemWorked) {
    target += log_mix(nu[stud_idx[w]],
 {{p_sig}}
                );
  }


  // Priors
  alpha ~ normal(0, 2.5);
  betaU ~ normal(0, 5);

  betaY ~ normal(0, 2);
  tau0 ~ normal(0, 2);
  tau1 ~ normal(0, 1);
  sigY ~ cauchy(0, 2.5);

 {{prior_type}}

",.open = "{{", .close = "}}")

    } else {

      if(lv_randomeffect) {

      } else {

      }


      script <- glue(
        "
  //  Outcome model with random effects
  // likelihood for the outcome 'Y'
  for (i in 1:nstud) {
      int g = sch[i];  // School for individual n
    // Compute likelihood for Y
    real mu_class1 = tau0[1]
                     + uB_Y1[g]
                     + tau1_B[1] * cm_Z[g]
                     + tau1_W[1] * Z[i]

                     + dot_product(cm_X[g], betaYB)
                     + dot_product(X[i], betaYW)
                     ;

    real mu_class2 = tau0[2]
                     + uB_Y2[g]
                     + tau1_B[2] * cm_Z[g]
                     + tau1_W[2] * Z[i]

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
  {{p_sig}}
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
  sigmaNuB ~ cauchy(0, 2);

  betaYW ~ normal(0, 2);
  betaYB ~ normal(0, 1);

  betaUB ~ normal(0, 1);
  betaUW ~ normal(0, 2);

  tau1_W ~ normal(0, 1);
  tau1_B ~ normal(0, 1);

  tau0 ~ normal(0, 1);
  alphaB_nu ~ normal(0, 1);

  uB_Y1 ~ normal(0, sigmaYB[1]);
  uB_Y2 ~ normal(0, sigmaYB[2]);
  uB_nu ~ normal(0, sigmaNuB);

",.open = "{{", .close = "}}")

      }

  }

  if(!type %in% c("lca","lpa")) {

    p_type <- ifelse(cate, "<lower=0, upper=1>", "")

    prior_type <- switch(type,
"irt" =
" intcpt ~ normal(0, 2.5);
  for(i in 1:nitem) {
    for(j in 1:nfac) {
      loading_free[i, j] ~ normal(loading_prior[i, j], 1);
    }
  }",
"sem" =
" intcpt ~ normal(0, 2.5);
  for(i in 1:nitem) {
    for(j in 1:nfac) {
      loading_free[i, j] ~ normal(loading_prior[i, j], 1);
    }
  }",
'rasch' =
" intcpt ~ normal(0, 2.5);",
'grm' =
" for(i in 1:nitem) {
    intcpt[i] ~ normal(0, 2.5);
    for(j in 1:nfac) {
      loading_free[i, j] ~ normal(loading_prior[i, j], 1);
    }
  }",
'grm0' =
" for(i in 1:nitem) {
    intcpt[i] ~ normal(0, 2.5);
  }"
)

    p_lik <- switch(type,
           "irt" =
"  for(j in 1:nitemWorked) {
     linPred[j] = intcpt[item_idx[j]]
                + dot_product(loading[item_idx[j], 1:nfac], fsc[stud_idx[j]]);
     grad[j] ~ bernoulli_logit(linPred[j]);
   }",
'sem' =
" for(j in 1:nitemWorked) {
    linPred[j] = intcpt[item_idx[j]]
               + dot_product(loading[item_idx[j], 1:nfac], fsc[stud_idx[j]]);
  }
  grad ~ normal(linPred, sigR);
",
"rasch" =
" for(j in 1:nitemWorked) {
   linPred[j] = intcpt[item_idx[j]]
              + dot_product(factoridx[item_idx[j], 1:nfac], fsc[stud_idx[j]]);
   grad[j] ~ bernoulli_logit(linPred[j]);
  }",

'grm' =
" for(i in 1:nitemWorked) {
      grad[i] ~ ordered_logistic(loading[item_idx[i], 1:nfac] * fsc[stud_idx[i]], intcpt[item_idx[i]]);
  }",
'grm0' =
" for(i in 1:nitemWorked) {
      grad[i] ~ ordered_logistic(fsc[stud_idx[i]], intcpt[item_idx[i]]);
  }",

)

    script <- glue(
" real linPred[nitemWorked];
  vector[nfac] A = rep_vector(1, nfac);
  matrix[nfac, nfac] A0;
  vector[nfac] muEta[nstud];
  vector[nstud] muY0;
  vector[nstud] muY;
  real sigYI[nstud];

  // Prior on correlation matrix
  L ~ lkj_corr_cholesky(nfac);
  A0 = diag_pre_multiply(A, L);",
  .open = "{{", .close = "}}")

    if(level == 1) {

      script <- glue(
        "
{{script}}

  // FLPS model
  for(i in 1:nstud) {
    muEta[i] = to_vector(X[i, ] * betaU);
    muY0[i] = intcptY
            + dot_product(to_row_vector(omega), fsc[i])
            + Z[i] * (tau0 + dot_product(to_row_vector(tau1), fsc[i]));
    muY[i] = muY0[i] + dot_product(X[i, ], betaY);
    sigYI[i] = sigY[Z[i] + 1];
  }

  // Likelihoods
  // Outcome
  Y ~ normal(muY, sigYI);

",.open = "{{", .close = "}}")


    } else {


      if(lv_randomeffect) {

      } else {

      }


      script <- glue(
        "
{{script}}

  // FLPS model
  for(i in 1:nstud) {

    int g = sch[i];  // School for individual i

    muEta[i] = to_vector(X[i, ] * betaUW);

    muY[i] = intcptY
           + dot_product(cm_X[g, ], betaYB)
           + uY[g]
           + dot_product(to_row_vector(omega), fsc[i])
           + Z[i] * (tau0 + dot_product(to_row_vector(tau1), fsc[i]))
           + dot_product(X[i, ], betaYW);

    sigYI[i] = sigY[Z[i] + 1];
  }

  // Likelihoods
  // Outcome
  Y ~ normal(muY, sigYI);
  uY ~ normal(0, sigYB);

",.open = "{{", .close = "}}")

      }

    script <- glue(
      "
{{script}}

  // Latent variable model
  fsc ~ multi_normal_cholesky(muEta, A0);
  {{p_lik}}

",.open = "{{", .close = "}}")



    if(level == 1) {

      script <- glue(
        "
{{script}}

  //priors
  // Priors for IRT
  {{prior_type}}

  // Priors for PS
  betaY ~ normal(0, 5);
  intcptY ~ normal(0, 5);
  tau0 ~ normal(ptau0[1, 1], ptau0[1, 2]);

  for(i in 1:nfac) {
    betaU[:, i] ~ normal(0, 5);

    omega[i] ~ normal(pomega[i, 1], pomega[i, 2]);
    tau1[i] ~ normal(ptau1[i, 1], ptau1[i, 2]);
  }

",.open = "{{", .close = "}}")

    } else {


      if(lv_randomeffect) {

      } else {

        script <- glue(
          "
{{script}}

  //priors
  // Priors for IRT
  {{prior_type}}

  // Priors for PS
  betaYW ~ normal(0, 5);
  betaYB ~ normal(0, 5);
  intcptY ~ normal(0, 5);
  tau0 ~ normal(ptau0[1, 1], ptau0[1, 2]);

  for(i in 1:nfac) {
    betaUW[:, i] ~ normal(0, 5);

    omega[i] ~ normal(pomega[i, 1], pomega[i, 2]);
    tau1[i] ~ normal(ptau1[i, 1], ptau1[i, 2]);
  }

",.open = "{{", .close = "}}")

      }




    }


  }

  script <- glue(
    "

model {
 {{script}}
}

",.open = "{{", .close = "}}")


  return(script)
}
