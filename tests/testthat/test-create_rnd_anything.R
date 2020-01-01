test_that("create site model", {
  set.seed(42)
  site_model <- NA
  while (!beautier::is_site_model(site_model)) {
    site_model <- create_rnd_anything()
  }
  expect_true(beautier::is_site_model(site_model))
})
