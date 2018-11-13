context("create_random")


test_that("create_rnd_alpha_param", {
  expect_true(
    beautier:::is_alpha_param(
      create_rnd_alpha_param()
    )
  )
})

test_that("create_rnd_bd_tree_prior", {
  set.seed(0)
  expect_true(
    beautier:::is_bd_tree_prior(
      create_rnd_bd_tree_prior()
    )
  )
})

test_that("create_rnd_beta_distr", {
  set.seed(0)
  expect_true(
    beautier:::is_beta_distr(
      create_rnd_beta_distr()
    )
  )
})

test_that("create_rnd_beta_param", {
  expect_true(
    beautier:::is_beta_param(
      create_rnd_beta_param()
    )
  )
})

test_that("create_rnd_bool", {
  expect_true(create_rnd_bool() %in% c(TRUE, FALSE))
})

test_that("create_rnd_cbs_tree_prior", {
  set.seed(0)
  expect_true(
    beautier:::is_cbs_tree_prior(
      create_rnd_cbs_tree_prior()
    )
  )
})

test_that("create_rnd_ccp_tree_prior", {
  set.seed(0)
  expect_true(
    beautier:::is_ccp_tree_prior(
      create_rnd_ccp_tree_prior()
    )
  )
})

test_that("create_rnd_cep_tree_prior", {
  set.seed(0)
  expect_true(
    beautier:::is_cep_tree_prior(
      create_rnd_cep_tree_prior()
    )
  )
})

test_that("create_rnd_clock_model", {
  set.seed(0)
  # Repeat often enough so all execution branches are hit
  for (i in seq(1, 5)) {
    expect_true(
      beautier:::is_clock_model(
        create_rnd_clock_model()
      )
    )
  }
})

test_that("create_rnd_clock_rate_param", {
  expect_true(
    beautier:::is_clock_rate_param(
      create_rnd_clock_rate_param()
    )
  )
})

test_that("create_rnd_distr", {
  set.seed(0)
  expect_true(
    beautier:::is_distr(
      create_rnd_distr()
    )
  )
})

test_that("create_rnd_estimate", {
  expect_true(create_rnd_estimate() %in% c(TRUE, FALSE))
})

test_that("create_rnd_exp_distr", {
  set.seed(0)
  expect_true(
    beautier:::is_exp_distr(
      create_rnd_exp_distr()
    )
  )
})

test_that("create_rnd_freq_equilibrium", {
  expect_true(create_rnd_freq_equilibrium()
    %in% c("estimated", "empirical", "all_equal")
  )
})

test_that("create_rnd_gamma_distr", {
  set.seed(0)
  expect_true(
    beautier:::is_gamma_distr(
      create_rnd_gamma_distr()
    )
  )
})

test_that("create_rnd_gamma_site_model", {
  set.seed(0)
  for (i in seq(1, 3)) {
    expect_true(
      beautier:::is_gamma_site_model(
        create_rnd_gamma_site_model()
      )
    )
  }
})

test_that("create_rnd_gtr_site_model", {
  set.seed(0)
  expect_true(
    beautier:::is_gtr_site_model(
      create_rnd_gtr_site_model()
    )
  )
})

test_that("create_rnd_hky_site_model", {
  set.seed(0)
  expect_true(
    beautier:::is_hky_site_model(
      create_rnd_hky_site_model()
    )
  )
})

test_that("create_rnd_inv_gamma_distr", {
  set.seed(0)
  expect_true(
    beautier:::is_inv_gamma_distr(
      create_rnd_inv_gamma_distr()
    )
  )
})

test_that("create_rnd_jc69_site_model", {
  set.seed(0)
  expect_true(
    beautier:::is_jc69_site_model(
      create_rnd_jc69_site_model()
    )
  )
})

test_that("create_rnd_kappa_1_param", {
  expect_true(
    beautier:::is_kappa_1_param(
      create_rnd_kappa_1_param()
    )
  )
})

test_that("create_rnd_kappa_2_param", {
  expect_true(
    beautier:::is_kappa_2_param(
      create_rnd_kappa_2_param()
    )
  )
})

test_that("create_rnd_lambda_param", {
  expect_true(
    beautier:::is_lambda_param(
      create_rnd_lambda_param()
    )
  )
})

test_that("create_rnd_laplace_distr", {
  set.seed(0)
  expect_true(
    beautier:::is_laplace_distr(
      create_rnd_laplace_distr()
    )
  )
})

test_that("create_rnd_log_normal_distr", {
  set.seed(0)
  for (i in seq(1, 6)) {
    expect_true(
      beautier:::is_log_normal_distr(
        create_rnd_log_normal_distr()
      )
    )
  }
})

test_that("create_rnd_m_param", {
  expect_true(
    beautier:::is_m_param(
      create_rnd_m_param()
    )
  )
})

test_that("create_rnd_mean_param", {
  expect_true(
    beautier:::is_mean_param(
      create_rnd_mean_param()
    )
  )
})

test_that("create_rnd_mrca_prior", {
  expect_true(
    beautier:::is_mrca_prior(
      create_rnd_mrca_prior(
        beautier::get_beautier_path("anthus_aco_sub.fas")
      )
    )
  )
})

test_that("create_rnd_mrca_priors", {
  set.seed(0)
  # Repeat often enough so all execution branches are hit
  for (i in seq(1, 6)) {
    expect_true(
      beautier:::are_mrca_priors(
        create_rnd_mrca_priors(
          beautier::get_beautier_path("anthus_aco_sub.fas")
        )
      )
    )
  }
})


test_that("create_rnd_mu_param", {
  expect_true(
    beautier:::is_mu_param(
      create_rnd_mu_param()
    )
  )
})

test_that("create_rnd_normal_distr", {
  set.seed(0)
  expect_true(
    beautier:::is_normal_distr(
      create_rnd_normal_distr()
    )
  )
})

test_that("create_rnd_one_div_x_distr", {
  set.seed(0)
  expect_true(
    beautier:::is_one_div_x_distr(
      create_rnd_one_div_x_distr()
    )
  )
})

test_that("create_rnd_param", {

  set.seed(0)
  # Repeat often enough so all execution branches are hit
  for (i in seq(1, 65)) {
    expect_true(
      beautier:::is_param(
        create_rnd_param()
      )
    )
  }
})

test_that("create_rnd_poisson_distr", {
  set.seed(0)
  expect_true(
    beautier:::is_poisson_distr(
      create_rnd_poisson_distr()
    )
  )
})

test_that("create_rnd_rate_ac_param", {
  expect_true(
    beautier:::is_rate_ac_param(
      create_rnd_rate_ac_param()
    )
  )
})

test_that("create_rnd_rate_ag_param", {
  expect_true(
    beautier:::is_rate_ag_param(
      create_rnd_rate_ag_param()
    )
  )
})

test_that("create_rnd_rate_at_param", {
  expect_true(
    beautier:::is_rate_at_param(
      create_rnd_rate_at_param()
    )
  )
})

test_that("create_rnd_rate_cg_param", {
  expect_true(
    beautier:::is_rate_cg_param(
      create_rnd_rate_cg_param()
    )
  )
})

test_that("create_rnd_rate_ct_param", {
  expect_true(
    beautier:::is_rate_ct_param(
      create_rnd_rate_ct_param()
    )
  )
})

test_that("create_rnd_rate_gt_param", {
  expect_true(
    beautier:::is_rate_gt_param(
      create_rnd_rate_gt_param()
    )
  )
})

test_that("create_rnd_rln_clock_model", {
  set.seed(0)
  expect_true(
    beautier:::is_rln_clock_model(
      create_rnd_rln_clock_model()
    )
  )
})

test_that("create_rnd_s_param", {
  expect_true(
    beautier:::is_s_param(
      create_rnd_s_param()
    )
  )
})

test_that("create_rnd_scale_param", {
  expect_true(
    beautier:::is_scale_param(
      create_rnd_scale_param()
    )
  )
})

test_that("create_rnd_sigma_param", {
  set.seed(0)
  for (i in seq(1, 2)) {
    expect_true(
      beautier:::is_sigma_param(
        create_rnd_sigma_param()
      )
    )
  }
})

test_that("create_rnd_site_model", {
  set.seed(0)
  # Repeat often enough so all execution branches are hit
  # 8 is too few
  for (i in seq(1, 16)) {
    expect_true(
      beautier:::is_site_model(
        create_rnd_site_model()
      )
    )
  }
})

test_that("create_rnd_strict_clock_model", {
  set.seed(0)
  expect_true(
    beautier:::is_strict_clock_model(
      create_rnd_strict_clock_model()
    )
  )
})

test_that("create_rnd_tn93_site_model", {
  set.seed(0)
  expect_true(
    beautier:::is_tn93_site_model(
      create_rnd_tn93_site_model()
    )
  )
})

test_that("create_rnd_tree_prior", {
  set.seed(0)
  # Repeat often enough so all execution branches are hit
  for (i in seq(1, 11)) {
    expect_true(
      beautier:::is_tree_prior(
        create_rnd_tree_prior()
      )
    )
  }
})

test_that("create_rnd_two_mrca_priors", {
  set.seed(0)
  # Repeat often enough so all execution branches are hit
  for (i in seq(1, 10)) {
    mrca_priors <- create_rnd_two_mrca_priors(
      beautier::get_beautier_path("anthus_aco_sub.fas")
    )
    testit::assert(length(mrca_priors) == 2)
    expect_true(
      beautier:::are_mrca_priors(mrca_priors)
    )
    expect_true(
      beautier:::are_mrca_taxa_non_intersecting(mrca_priors)
    )

  }
})

test_that("create_rnd_uniform_distr", {
  set.seed(0)
  for (i in seq(1, 2)) {
    expect_true(
      beautier:::is_uniform_distr(
        create_rnd_uniform_distr()
      )
    )
  }
})

test_that("create_rnd_yule_tree_prior", {
  set.seed(0)
  expect_true(
    beautier:::is_yule_tree_prior(
      create_rnd_yule_tree_prior()
    )
  )
})
