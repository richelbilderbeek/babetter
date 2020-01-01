test_that("use", {
  set.seed(42)
  site_model <- NA
  while (!beautier::is_site_model(site_model)) {
    site_model <- create_rnd_site_model_nasty()
  }
  expect_true(beautier::is_site_model(site_model))
})
