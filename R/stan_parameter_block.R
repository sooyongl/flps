#' Parameters code block for Stan
#'
#' @examples
#' parameters_stan("irt")
#' parameters_stan("irt", cate = T)
#' parameters_stan("mixture", cate = FALSE, 2)
#' @noRd
parameters_stan <- function(type = "irt", cate = TRUE, level = 1, lv_randomeffect = FALSE) {

  type <- tolower(type)

  if(type %in% c("lca","lpa")) {

    p_type <- ifelse(cate, "<lower=0, upper=1>", "")
    p_sig <-  ifelse(cate, "",
                     "vector<lower=0>[nitem] sigR; // Standard deviations for indicators for each class")

    if(level == 1) {

      script <- glue("
  real alpha;             // Intercept for class proportion
  vector[ncov] betaY;     // Coefficients for the outcome Y
  vector[ncov] betaU;     // Coefficients for class membership

  real{p_type} p[nclass, nitem];  // Item Response probabilities
  real<lower=0, upper=1> mu_p;  // hyper parameter
  real<lower=0> sigma_p;        // hyper parameter

  vector[nclass] tau0;           // Intercept for Y for each class
  vector[nclass] tau1;           // Coefficient for Z for each class
  vector<lower=0>[nclass] sigY; // Standard deviations for Y for each class
  {p_sig}")


    } else {

      if(lv_randomeffect) {

      } else {
        script <- glue("
  real{p_type} p[nclass, nitem];  // Item Response probabilities

  // Covariates effects on Outcome
  vector[ncov_lv1] betaYW;     // Within-
  vector[ncov_lv2] betaYB;     // Between-

  // Coefficients for class membership
  vector[ncov_lv1] betaUW;     // Within-
  vector[ncov_lv2] betaUB;    // Between-

  // Treatment effects on the outcome
  vector[nclass] tau1_W;   // Within-
  vector[nclass] tau1_B;   // Between-

  // Outcome Mean differences by LC
  // Overall mean ?
  vector[nclass] tau0;

  // Intercept for class proportion
  real alphaB_nu;

  // Random effects
  vector[nsch] uB_Y1;
  vector[nsch] uB_Y2;
  vector[nsch] uB_nu;

  // Standard deviations for Y for each class
  vector<lower=0>[nclass] sigmaYW; // Within-
  vector<lower=0>[nclass] sigmaYB; // Between-
  {p_sig}

  // Standard deviations for Intercept for class proportion random effects
  real<lower=0> sigmaNuB;

  ")
      }

    }

  }

  if(!type %in% c("lca","lpa")) {

    p_sig <-  ifelse(cate, "", "real<lower=0> sigR;")
    measure_param <- switch(type,
                            "irt" =
"  matrix[nitem, nfac] loading_free;      // Item slopes
   real intcpt[nitem];               // Item intercepts",

"rasch" =
"
  real intcpt[nitem];               // Item intercepts",

"sem" =
"  matrix[nitem, nfac] loading_free;      // Item slopes
   real intcpt[nitem];               // Item intercepts",

"grm" =
" matrix[nitem, nfac] loading_free;   // Item slopes
  ordered[max_k-1] intcpt[nitem];     // Item intercepts",

"grm0" =
" ordered[max_k-1] intcpt[nitem];     // Item intercepts"
)

    if(level == 1) {

      script <- glue("
  // IRT model
  vector[nfac] fsc[nstud];       // person scores for each factor
  cholesky_factor_corr[nfac] L;  // Cholesky decomp of corr mat of random slopes

  {measure_param}

  matrix[ncov, nfac] betaU;
  vector[ncov] betaY;

  real intcptY;
  vector[nfac] omega;
  real tau0;

  vector[nfac] tau1;
  real<lower=0> sigY[2];
  {p_sig}")

    } else {

      measure_param <- switch(type,
                              "irt" =
                                " matrix[nitem, nfac] loading_free;      // Item slopes
  real intcpt[nitem];               // Item intercepts",

  "rasch" =
    " real intcpt[nitem];               // Item intercepts",

  "sem" =
    "  matrix[nitem, nfac] loading_free;      // Item slopes
   real intcpt[nitem];               // Item intercepts",

  "grm" =
    " matrix[nitem, nfac] loading_free;   // Item slopes
  ordered[max_k-1] intcpt[nitem];     // Item intercepts",

  "grm0" =
    " ordered[max_k-1] intcpt[nitem];     // Item intercepts"
      )

      if(lv_randomeffect) {

        script <- glue("
// IRT model
  vector[nfac] fscW[nstud]; // person scores for each factor
  cholesky_factor_corr[nfac] LW;  // Cholesky decomp of corr mat of random slopes
  vector[nfac] fscB[nsch]; // person scores for each factor
  cholesky_factor_corr[nfac] LB;  // Cholesky decomp of corr mat of random slopes

  {measure_param}

  // Covariates effects on Outcome
  matrix[ncov_lv1, nfac] betaUW;     // Within-
  matrix[ncov_lv2, nfac] betaUB;     // Between-

  vector[ncov_lv1] betaYW;     // Within-
  vector[ncov_lv2] betaYB;     // Between-

  real intcptY;
  vector[nfac] omegaW;
  vector[nfac] omegaB;

  real tau0_W;
  real tau0_B;

  vector[nfac] tau1_W;
  vector[nfac] tau1_B;


  real<lower=0> sigY;
  real<lower=0> sigYB;

  //vector[nfac]<lower=0> sigGrad;

  vector[nsch] uY;
  //vector[nsch] ugrad;
  vector[nitem] ugrad[nsch];
")

      } else {
        script <- glue("
  // IRT model
  vector[nfac] fsc[nstud]; // person scores for each factor
  cholesky_factor_corr[nfac] L; // Cholesky decomp of corr mat of random slopes

  {measure_param}

  // Covariates effects on Outcome
  matrix[ncov_lv1, nfac] betaUW;     // Within-

  vector[ncov_lv1] betaYW;     // Within-
  vector[ncov_lv2] betaYB;     // Between-

  real intcptY;
  vector[nfac] omega;

  real tau0;
  vector[nfac] tau1;

  {p_sig}
  real<lower=0> sigY[2];
  real<lower=0> sigYB;

  vector[nsch] uY;")
      }

    }
  }

  script <- glue(
    "

parameters{
 {{script}}
}

",.open = "{{", .close = "}}")


  return(script)
}
