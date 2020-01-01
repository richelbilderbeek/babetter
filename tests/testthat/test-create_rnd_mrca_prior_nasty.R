test_that("use", {
  set.seed(42)
  mrca_prior <- NA
  while (!beautier::is_mrca_prior(mrca_prior)) {
    mrca_prior <- create_rnd_mrca_prior_nasty()
  }
  expect_true(beautier::is_mrca_prior(mrca_prior))
})
