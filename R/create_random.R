#' Create a random alpha parameter
#' @author Richel J.C. Bilderbeek
create_rnd_alpha_param <- function() {
  beautier::create_alpha_param( # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random BD tree prior
#' @author Richel J.C. Bilderbeek
create_rnd_bd_tree_prior <- function() {
  beautier::create_bd_tree_prior( # nolint internal function
    birth_rate_distr = create_rnd_distr(), # nolint internal function
    death_rate_distr = create_rnd_distr() # nolint internal function
  )
}

#' Create a random beta distribution
#' @author Richel J.C. Bilderbeek
create_rnd_beta_distr <- function() {

  beta_distr <- NA
  while (beautier:::is_one_na(beta_distr)) {
    tryCatch(
      beta_distr <- beautier::create_beta_distr( # nolint internal function
        alpha = create_rnd_alpha_param(), # nolint internal function
        beta = create_rnd_beta_param() # nolint internal function
      ),
      error = function(error) {
        whitelist <- c(
          "'alpha' must have a positive value",
          "'beta' must have a value of at least 1.0"
        )
        testit::assert(
          beautier:::is_in_patterns(line = error$message, patterns = whitelist)
        )
      }
    )
  }
  beta_distr
}

#' Create a random beta parameter
#' @author Richel J.C. Bilderbeek
create_rnd_beta_param <- function() {
  beautier::create_beta_param( # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random boolean
#' @author Richel J.C. Bilderbeek
#' @export
create_rnd_bool <- function() {
  sample(x = 1:2, size = 1) == 1
}

#' Create a random CBS tree prior
#' @author Richel J.C. Bilderbeek
create_rnd_cbs_tree_prior <- function() {
  beautier::create_cbs_tree_prior() # nolint internal function
}

#' Create a random CCP tree prior
#' @author Richel J.C. Bilderbeek
create_rnd_ccp_tree_prior <- function() {
  beautier::create_ccp_tree_prior( # nolint internal function
    pop_size_distr = create_rnd_distr() # nolint internal function
  )
}

#' Create a random CEP tree prior
#' @author Richel J.C. Bilderbeek
create_rnd_cep_tree_prior <- function() {
  beautier::create_cep_tree_prior( # nolint internal function
    pop_size_distr = create_rnd_distr(), # nolint internal function
    growth_rate_distr = create_rnd_distr() # nolint internal function
  )
}

#' Create a random clock model
#' @author Richel J.C. Bilderbeek
create_rnd_clock_model <- function() {
  clock_model_index <- sample(x = 1:2, size = 1)
  if (clock_model_index == 1) {
    create_rnd_rln_clock_model() # nolint internal function
  } else {
    testit::assert(clock_model_index == 2)
    create_rnd_strict_clock_model() # nolint internal function
  }
}

#' Create a random clock rate parameter
#' @author Richel J.C. Bilderbeek
create_rnd_clock_rate_param <- function() {
  beautier::create_clock_rate_param( # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random distribution
#' @author Richel J.C. Bilderbeek
create_rnd_distr <- function() {
  distr_index <- sample(x = 1:10, size = 1)
  if (distr_index == 1) {
    create_rnd_beta_distr() # nolint internal function
  } else if (distr_index == 2) {
    create_rnd_exp_distr() # nolint internal function
  } else if (distr_index == 3) {
    create_rnd_gamma_distr() # nolint internal function
  } else if (distr_index == 4) {
    create_rnd_inv_gamma_distr() # nolint internal function
  } else if (distr_index == 5) {
    create_rnd_laplace_distr() # nolint internal function
  } else if (distr_index == 6) {
    create_rnd_log_normal_distr() # nolint internal function
  } else if (distr_index == 7) {
    create_rnd_normal_distr() # nolint internal function
  } else if (distr_index == 8) {
    create_rnd_one_div_x_distr() # nolint internal function
  } else if (distr_index == 9) {
    create_rnd_poisson_distr() # nolint internal function
  } else {
    testit::assert(distr_index == 10)
    create_rnd_uniform_distr() # nolint internal function
  }
}

#' Create a random value for 'estimate',
#' which must be TRUE or FALSE
#' @author Richel J.C. Bilderbeek
create_rnd_estimate <- function() {
  create_rnd_bool() # nolint internal function
}

#' Create a random exponential distribution
#' @author Richel J.C. Bilderbeek
create_rnd_exp_distr <- function() {
  beautier::create_exp_distr(
    mean = create_rnd_mean_param() # nolint internal function
  )
}

#' Create a random value for 'freq equilibrium', which
#' can be 'estimated', 'empirical' or 'all_equal'
#' @author Richel J.C. Bilderbeek
create_rnd_freq_equilibrium <- function() {
  options <- c("estimated", "empirical", "all_equal")
  options[sample(x = 1:3, size = 1)]
}

#' Create a random gamma distribution
#' @author Richel J.C. Bilderbeek
create_rnd_gamma_distr <- function() {

  gamma_distr <- NA
  while (beautier:::is_one_na(gamma_distr)) {
    tryCatch(
        gamma_distr <- beautier::create_gamma_distr( # nolint internal function
        alpha = create_rnd_alpha_param(), # nolint internal function
        beta = create_rnd_beta_param() # nolint internal function
      ),
      error = function(error) {
        whitelist <- c(
          "'value' of 'alpha' must be positive",
          "'value' of 'beta' must be positive"
        )
        testit::assert(
          beautier:::is_in_patterns(line = error$message, patterns = whitelist)
        )
      }
    )
  }
  gamma_distr
}

#' Create a random gamma site model
#' @author Richel J.C. Bilderbeek
create_rnd_gamma_site_model <- function() {
  gamma_site_model <- NA
  while (beautier:::is_one_na(gamma_site_model)) {
    tryCatch(
      gamma_site_model <- beautier::create_gamma_site_model( # nolint internal function
        gamma_cat_count = sample(x = -1:4, size = 1),
        gamma_shape = stats::runif(n = 1, min = -1.0, max = 1.0),
        prop_invariant = stats::runif(n = 1, min = -1.0, max = 1.0),
        gamma_shape_prior_distr = create_rnd_distr(), # nolint internal function
        freq_equilibrium = create_rnd_freq_equilibrium() # nolint internal function
      ),
      error = function(error) {
        whitelist <- c(
          "'gamma_cat_count' must be positive",
          "'gamma_shape' must be positive",
          "'prop_invariant' must be in range \\[0\\.0, 1\\.0\\]",
          "'gamma_shape_prior_distr' must be NA for a 'gamma_cat_count' of less than two" # nolint indeed long error message, preferred this over using paste0
        )
        testit::assert(
          beautier:::is_in_patterns(line = error$message, patterns = whitelist)
        )
      }
    )
  }
  testit::assert(beautier:::is_gamma_site_model(gamma_site_model)) # nolint internal function
  gamma_site_model
}

#' Create a random GTR site model
#' @author Richel J.C. Bilderbeek
create_rnd_gtr_site_model <- function() {
  beautier::create_gtr_site_model( # nolint internal function
    gamma_site_model = create_rnd_gamma_site_model(), # nolint internal function
    rate_ac_prior_distr = create_rnd_distr(), # nolint internal function
    rate_ag_prior_distr = create_rnd_distr(), # nolint internal function
    rate_at_prior_distr = create_rnd_distr(), # nolint internal function
    rate_cg_prior_distr = create_rnd_distr(), # nolint internal function
    rate_gt_prior_distr = create_rnd_distr(), # nolint internal function
    rate_ac_param = create_rnd_rate_ac_param(), # nolint internal function
    rate_ag_param = create_rnd_rate_ag_param(), # nolint internal function
    rate_at_param = create_rnd_rate_at_param(), # nolint internal function
    rate_cg_param = create_rnd_rate_cg_param(), # nolint internal function
    rate_ct_param = create_rnd_rate_ct_param(), # nolint internal function
    rate_gt_param = create_rnd_rate_gt_param(), # nolint internal function
    freq_equilibrium = create_rnd_freq_equilibrium() # nolint internal function
  )
}

#' Create a random HKY site model
#' @author Richel J.C. Bilderbeek
create_rnd_hky_site_model <- function() {
  beautier::create_hky_site_model( # nolint internal function
    gamma_site_model = create_rnd_gamma_site_model(), # nolint internal function
    kappa = stats::runif(n = 1, min = -100.0, max = 100.0),
    kappa_prior_distr = create_rnd_distr(), # nolint internal function
    freq_equilibrium = create_rnd_freq_equilibrium() # nolint internal function
  )
}

#' Create a random inverse-gamma distribution
#' @author Richel J.C. Bilderbeek
create_rnd_inv_gamma_distr <- function() {
  beautier::create_inv_gamma_distr( # nolint internal function
    alpha = create_rnd_alpha_param(), # nolint internal function
    beta = create_rnd_beta_param() # nolint internal function
  )
}

#' Create a random JC69 distribution
#' @author Richel J.C. Bilderbeek
create_rnd_jc69_site_model <- function() {
  beautier::create_jc69_site_model( # nolint internal function
    gamma_site_model = create_rnd_gamma_site_model() # nolint internal function
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

#' Create a random Laplace distribution
#' @author Richel J.C. Bilderbeek
create_rnd_laplace_distr <- function() {
  beautier::create_laplace_distr( # nolint internal function
    mu = create_rnd_mu_param(), # nolint internal function
    scale = create_rnd_scale_param() # nolint internal function
  )
}

#' Create a random log-normal distribution
#' @author Richel J.C. Bilderbeek
create_rnd_log_normal_distr <- function() {

  log_normal_distr <- NA
  while (beautier:::is_one_na(log_normal_distr)) {
    tryCatch(
      log_normal_distr <- beautier::create_log_normal_distr( # nolint internal function
        m = create_rnd_m_param(), # nolint internal function
        s = create_rnd_s_param() # nolint internal function
      ),
      error = function(error) {
        whitelist <- c(
          "'value' of 's' must be positive"
        )
        testit::assert(
          beautier:::is_in_patterns(line = error$message, patterns = whitelist)
        )
      }
    )
  }
  log_normal_distr
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

#' Create a random MRCA prior
#' @param fasta_filename a FASTA filename
#' @author Richel J.C. Bilderbeek
create_rnd_mrca_prior <- function(fasta_filename) {
  all_taxa_names <- beautier::get_taxa_names(fasta_filename) # nolint internal function
  n_taxa <- stats::runif(min = 1, max = length(all_taxa_names), n = 1)
  taxa_names <- sample(x = all_taxa_names, size = n_taxa)
  beautier::create_mrca_prior( # nolint internal function
    alignment_id = beautier::get_alignment_id(fasta_filename),
    taxa_names = taxa_names,
    is_monophyletic = create_rnd_bool(),
    mrca_distr = create_rnd_distr()
  )
}

#' Creates a random MRCA prior option. This is either zero, one
#' or more MRCA priors. For zero MRCA priors, NA is used. When two
#' or more MRCA priors (see \code{\link{create_rnd_two_mrca_priors}})
#' are created,
#' those are checked to be compatible
#' @param fasta_filename a FASTA filename
#' @author Richel J.C. Bilderbeek
create_rnd_mrca_priors <- function(fasta_filename) {
  param_index <- sample(x = 1:3, size = 1)
  if (param_index == 1) {
    NA
  } else if (param_index == 2) {
    create_rnd_mrca_prior(fasta_filename) # nolint internal function
  } else {
    testit::assert(param_index == 3)
    create_rnd_two_mrca_priors(fasta_filename) # nolint internal function
  }
}

#' Creates two MRCA priors, checked to be compatible
#' @param fasta_filename a FASTA filename
#' @author Richel J.C. Bilderbeek
create_rnd_two_mrca_priors <- function(fasta_filename) {
  while (1) {
    mrca_priors <- list(
      create_rnd_mrca_prior(fasta_filename), # nolint internal function
      create_rnd_mrca_prior(fasta_filename) # nolint internal function
    )
    if (beautier:::are_mrca_taxa_non_intersecting(mrca_priors)) { # nolint internal function
      return(mrca_priors)
    }
  }
}

#' Create a random mu parameter
#' @author Richel J.C. Bilderbeek
create_rnd_mu_param <- function() {
  beautier::create_mu_param( # nolint internal function
    value = stats::runif(n = 1, min = -10, max = 10)
  )
}

#' Create a random normal distribution
#' @author Richel J.C. Bilderbeek
create_rnd_normal_distr <- function() {
  beautier::create_normal_distr( # nolint internal function
    mean = create_rnd_mean_param(), # nolint internal function
    sigma = create_rnd_sigma_param() # nolint internal function
  )
}

#' Create a random 1/x distribution
#' @author Richel J.C. Bilderbeek
create_rnd_one_div_x_distr <- function() {
  beautier::create_one_div_x_distr() # nolint internal function
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

#' Create a random Poisson distribution
#' @author Richel J.C. Bilderbeek
create_rnd_poisson_distr <- function() {
  beautier::create_poisson_distr( # nolint internal function
    lambda = create_rnd_lambda_param() # nolint internal function
  )
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

#' Create a random RLN clock model
#' @author Richel J.C. Bilderbeek
create_rnd_rln_clock_model <- function() {
  beautier::create_rln_clock_model( # nolint internal function
    mean_rate_prior_distr = create_rnd_distr(), # nolint internal function
    ucldstdev_distr = create_rnd_distr(), # nolint internal function
    mean_clock_rate = stats::runif(n = 1, min = -100.0, max = 100.0),
    n_rate_categories = sample(x = -2:10, size = 1),
    normalize_mean_clock_rate = create_rnd_bool() # nolint internal function
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
  while (beautier:::is_one_na(sigma_param)) { # nolint internal function
    tryCatch(
      sigma_param <- beautier::create_sigma_param( # nolint internal function
        value = stats::runif(n = 1, min = -10, max = 10)
      ),
      error = function(error) {
        whitelist <- c(
          "'value' must be non-zero and positive"
        )
        testit::assert(
          beautier:::is_in_patterns(line = error$message, patterns = whitelist)
        )
      }
    )
  }
  sigma_param
}

#' Create a random site model
#' @author Richel J.C. Bilderbeek
create_rnd_site_model <- function() {

  site_model_index <- sample(x = 1:4, size = 1)
  if (site_model_index == 1) {
    create_rnd_jc69_site_model() # nolint internal function
  } else if (site_model_index == 2) {
    create_rnd_hky_site_model() # nolint internal function
  } else if (site_model_index == 3) {
    create_rnd_tn93_site_model() # nolint internal function
  } else {
    testit::assert(site_model_index == 4)
    create_rnd_gtr_site_model() # nolint internal function
  }
}

#' Create a random strict clock model
#' @author Richel J.C. Bilderbeek
create_rnd_strict_clock_model <- function() {
  beautier::create_strict_clock_model( # nolint internal function
    clock_rate_param = create_rnd_clock_rate_param(), # nolint internal function
    clock_rate_distr = create_rnd_distr() # nolint internal function
  )
}

#' Create a random TN93 site model
#' @author Richel J.C. Bilderbeek
create_rnd_tn93_site_model <- function() {
  beautier::create_tn93_site_model( # nolint internal function
    gamma_site_model = create_rnd_gamma_site_model(), # nolint internal function
    kappa_1_param = create_rnd_kappa_1_param(), # nolint internal function
    kappa_2_param = create_rnd_kappa_2_param(), # nolint internal function
    kappa_1_prior_distr = create_rnd_distr(), # nolint internal function
    kappa_2_prior_distr = create_rnd_distr(), # nolint internal function
    freq_equilibrium = create_rnd_freq_equilibrium() # nolint internal function
  )
}

#' Create a random tree prior
#' @author Richel J.C. Bilderbeek
create_rnd_tree_prior <- function() {
  tree_prior_index <- sample(x = 1:5, size = 1)

  if (tree_prior_index == 1) {
    create_rnd_bd_tree_prior() # nolint internal function
  } else if (tree_prior_index == 2) {
    create_rnd_cbs_tree_prior() # nolint internal function
  } else if (tree_prior_index == 3) {
    create_rnd_ccp_tree_prior() # nolint internal function
  } else if (tree_prior_index == 4) {
    create_rnd_cep_tree_prior() # nolint internal function
  } else {
    testit::assert(tree_prior_index == 5)
    create_rnd_yule_tree_prior() # nolint internal function
  }
}

#' Create a random uniform distribution
#' @author Richel J.C. Bilderbeek
create_rnd_uniform_distr <- function() {

  uniform_distr <- NA
  while (beautier:::is_one_na(uniform_distr)) {
    tryCatch(
      uniform_distr <- beautier::create_uniform_distr( # nolint internal function
        upper = stats::runif(n = 1, min = -10, max = 10)
      ),
      error = function(error) {
        whitelist <- c(
          "'upper' must be non-zero and positive"
        )
        testit::assert(
          beautier:::is_in_patterns(line = error$message, patterns = whitelist)
        )
      }
    )
  }
  uniform_distr
}

#' Create a random Yule tree prior
#' @author Richel J.C. Bilderbeek
create_rnd_yule_tree_prior <- function() {
  beautier::create_yule_tree_prior( # nolint internal function
    birth_rate_distr = create_rnd_distr() # nolint internal function
  )
}