#' Data code block for Stan
#'
#' @examples
#' data_stan("irt")
#' data_stan("irt", cate = T)
#' data_stan("mixture", cate = FALSE, 2)
#' @noRd
data_stan <- function(type = "irt", cate = TRUE, level = 1, lv_randomeffect = FALSE) {

  type <- tolower(type)

  prior_loading <- ifelse(type %in% c("lca","lpa"),
                          "",
" matrix[nitem, nfac] loading_prior;
  matrix[1,2] ptau0;
  matrix[nfac,2] ptau1;
  matrix[nfac,2] pomega;")


  cov_dim <- ifelse(level == 1,
                    "
int<lower=1> ncov;        // number of covariates",
                    "
int<lower=1> ncov_lv1;        // number of covariates level 1
int<lower=1> ncov_lv2;        // number of covariates level 2"
  )

  # data_demension
  if(!type %in% c("lca","lpa")) {
    script <- glue(
" // Data dimensions
  int<lower=1> nitemWorked;  // number of rows in long-format data
  int<lower=1> nitem;        // number of items
  int<lower=1> nstud;        // number of respondents
  {cov_dim}
  int<lower=1> nfac;         // number of latent factors
")
  } else {
    script <- glue(
" // Data dimensions
  int<lower=1> nitemWorked;  // number of rows in long-format data
  int<lower=1> nitem;        // number of items
  int<lower=1> nstud;        // number of respondents
  {cov_dim}
  int<lower=1> nclass;       // number of latent class
")
  }

  if(cate) {
    script <-   glue(
      "
{script}
  int<lower=0> min_k;       // min category
  int<lower=1> max_k;       // max category
")

  }

  script <- glue(
    "
{script}

  // Item Data indices
  int stud_idx[nitemWorked];  // student index for long-format data
  int item_idx[nitemWorked];   // item index for long-format data
")

  if(level == 2) {
    script <- glue(
      "
{script}

  // Cluster index
  int<lower=1> nsch;                  // Number of schools
  int<lower=1,upper=nsch> sch[nstud]; // Group membership for each individual

")

  }


  if(!type %in% c("lca","lpa")) {
    script <- glue(
      "
{script}

  // Index for factor loadings
  matrix[nitem, nfac] factoridx;
  int<lower=0> firstitem[nitem];
")
  }


  if(cate) {

    script <- glue(
      "
{script}

  // data data
  int<lower=min_k,upper=max_k> grad[nitemWorked]; // Item data
")
  } else {
    script <- glue(
      "
{script}

  // data data
  real grad[nitemWorked]; // Item data
")

  }

  if(level == 1) {
    script <- glue(
      "
{script}

  matrix[nstud, ncov] X;                  // Covariates
  int<lower=0, upper=1> Z[nstud];         // Treatment assignments
  real Y[nstud];
")

  } else {



    script <- glue(
      "
{script}

  matrix[nstud, ncov_lv1] X;
  matrix[nsch, ncov_lv2] cm_X;

  int<lower=0, upper=1> cm_Z[nsch];
  int<lower=0, upper=1> Z[nstud];
  real Y[nstud];
")
  }

  script <- glue(
    "
{script}

  // Priors
  // prior information
  {prior_loading}
")


  script <- glue(
    "data {
 {{script}}
}


",.open = "{{", .close = "}}")


  return(script)
}
