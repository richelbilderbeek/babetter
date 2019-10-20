#' Create a random alpha parameter
#' @author Richel J.C. Bilderbeek
create_rnd_alpha_param <- function() {
  beautier::create_alpha_param( # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random beta parameter
#' @author Richel J.C. Bilderbeek
create_rnd_beta_param <- function() {
  beautier::create_beta_param( # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random clock rate parameter
#' @author Richel J.C. Bilderbeek
create_rnd_clock_rate_param <- function() {
  beautier::create_clock_rate_param( # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random kappa 1 parameter
#' @author Richel J.C. Bilderbeek
create_rnd_kappa_1_param <- function() {
  beautier::create_kappa_1_param( # nolint internal function
    lower = stats::runif(n = 1, min = -10, max = 10),
    value = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random kappa 2 parameter
#' @author Richel J.C. Bilderbeek
create_rnd_kappa_2_param <- function() {
  beautier::create_kappa_2_param( # nolint internal function
    lower = stats::runif(n = 1, min = -10, max = 10),
    value = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random lambda parameter
#' @author Richel J.C. Bilderbeek
create_rnd_lambda_param <- function() {
  beautier::create_lambda_param( # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random m parameter
#' @author Richel J.C. Bilderbeek
create_rnd_m_param <- function() {
  beautier::create_m_param( # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random mean parameter
#' @author Richel J.C. Bilderbeek
create_rnd_mean_param <- function() {
  beautier::create_mean_param( # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random mu parameter
#' @author Richel J.C. Bilderbeek
create_rnd_mu_param <- function() {
  beautier::create_mu_param( # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random parameter
#' @author Richel J.C. Bilderbeek
create_rnd_param <- function() {

  param_index <- sample(x = 1:18, size = 1)
  if (param_index == 1) {
    beautier::create_alpha_param() # nolint internal function
  } else if (param_index == 2) {
    beautier::create_beta_param() # nolint internal function
  } else if (param_index == 3) {
    beautier::create_clock_rate_param() # nolint internal function
  } else if (param_index == 4) {
    beautier::create_kappa_1_param() # nolint internal function
  } else if (param_index == 5) {
    beautier::create_kappa_2_param() # nolint internal function
  } else if (param_index == 6) {
    beautier::create_lambda_param() # nolint internal function
  } else if (param_index == 7) {
    beautier::create_m_param() # nolint internal function
  } else if (param_index == 8) {
    beautier::create_mean_param() # nolint internal function
  } else if (param_index == 9) {
    beautier::create_mu_param() # nolint internal function
  } else if (param_index == 10) {
    beautier::create_rate_ac_param() # nolint internal function
  } else if (param_index == 11) {
    beautier::create_rate_ag_param() # nolint internal function
  } else if (param_index == 12) {
    beautier::create_rate_at_param() # nolint internal function
  } else if (param_index == 13) {
    beautier::create_rate_cg_param() # nolint internal function
  } else if (param_index == 14) {
    beautier::create_rate_ct_param() # nolint internal function
  } else if (param_index == 15) {
    beautier::create_rate_gt_param() # nolint internal function
  } else if (param_index == 16) {
    beautier::create_s_param() # nolint internal function
  } else if (param_index == 17) {
    beautier::create_scale_param() # nolint internal function
  } else {
    testit::assert(param_index == 18)
    beautier::create_sigma_param() # nolint internal function
  }
}

#' Create a random rate AC parameter
#' @author Richel J.C. Bilderbeek
create_rnd_rate_ac_param <- function() {
  beautier::create_rate_ac_param( # nolint internal function
    estimate = create_rnd_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10),
    lower = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random rate AG parameter
#' @author Richel J.C. Bilderbeek
create_rnd_rate_ag_param <- function() {
  beautier::create_rate_ag_param( # nolint internal function
    estimate = create_rnd_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10),
    lower = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random rate AT parameter
#' @author Richel J.C. Bilderbeek
create_rnd_rate_at_param <- function() {
  beautier::create_rate_at_param( # nolint internal function
    estimate = create_rnd_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10),
    lower = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random rate CG parameter
#' @author Richel J.C. Bilderbeek
create_rnd_rate_cg_param <- function() {
  beautier::create_rate_cg_param( # nolint internal function
    estimate = create_rnd_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10),
    lower = stats::runif(n = 1, min = -10, max = 10)
  )

}

#' Create a random rate CT parameter
#' @author Richel J.C. Bilderbeek
create_rnd_rate_ct_param <- function() {
  beautier::create_rate_ct_param( # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10),
    lower = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random rate GT parameter
#' @author Richel J.C. Bilderbeek
create_rnd_rate_gt_param <- function() {
  beautier::create_rate_gt_param( # nolint internal function
    estimate = create_rnd_estimate(), # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10),
    lower = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random s parameter
#' @author Richel J.C. Bilderbeek
create_rnd_s_param <- function() {
  lower <- stats::runif(n = 1, min = -10, max = 10)
  value <- lower + stats::runif(n = 1, min = 0.1, max = 10)
  upper <- value + stats::runif(n = 1, min = 0.1, max = 10)
  testit::assert(lower < value)
  testit::assert(value < upper)
  beautier::create_s_param( # nolint internal function
    value = value,
    lower = lower,
    upper = upper
  )
}

#' Create a random scale parameter
#' @author Richel J.C. Bilderbeek
create_rnd_scale_param <- function() {
  beautier::create_scale_param( # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random sigma parameter
#' @author Richel J.C. Bilderbeek
create_rnd_sigma_param <- function() {
  sigma_param <- NA
  while (beautier::is_one_na(sigma_param)) { # nolint internal function
    tryCatch(
      sigma_param <- beautier::create_sigma_param( # nolint internal function
        value = stats::runif(n = 1, min = -10, max = 10)
      ),
      error = function(error) {
        whitelist <- c(
          "'value' must be non-zero and positive"
        )
        testit::assert(
          beautier::is_in_patterns(line = error$message, patterns = whitelist)
        )
      }
    )
  }
  sigma_param
}

