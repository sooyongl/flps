#' transformed parameters
#'
#' transparams_stan()
#' @noRd
transparams_stan <- function(type = "irt", cate = TRUE, level = 1) {

  type <- tolower(type)

  if(type %in% c("lca","lpa")) {

    p_type <- ifelse(cate, "<lower=0, upper=1>", "")
    p_sig <-  ifelse(cate, "", "vector<lower=0>[nsec] sigR; // Standard deviations for indicators for each class")

    if(level == 1) {

      script <- glue("
  vector[nstud] nu; // Probability of class membership for all students

  // Individual class membership probabilities conditional on covariates
  for (n in 1:nstud) {
    nu[n] = inv_logit(alpha + dot_product(X[n], betaU));
  }

  // PS effects-Difference in Y on Z coefficient between classes
  real b1 = b01[2] - b01[1];
  // Omega-Difference in intercept in Y between classes
  real a1 = b00[2] - b00[1]; ",.open = "{{", .close = "}}")


    } else {

      script <- glue("
  vector[nstud] nu; // Probability of class membership for all nstud

  // Individual class membership probabilities conditional on covariates
  // with random effects
   for (i in 1:nstud) {
      int g = sch[i];  // School for individual n
      nu[i] = inv_logit(alphaB_nu
                      + uB_nu[g]
                      + dot_product(cm_X[g], betaUB)
                      + dot_product(X[i], betaUW)
                      );

  }

  // PS effects-Difference in Y on Z coefficient between classes
  real b1W = b01_W[2] - b01_W[1];
  real b1B = b01_B[2] - b01_B[1];
  // Omega-Difference in intercept in Y between classes
  real a1 = b00[2] - b00[1]; ",

  .open = "{{", .close = "}}")
    }

  }

  if(!type %in% c("lca","lpa")) {


    script <- ifelse(type %in% c("rasch","grm0"), "",
             "
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
  }")
    }

  script <- glue(
    "

transformed parameters{
 {{script}}
}

",.open = "{{", .close = "}}")


  return(script)
}
