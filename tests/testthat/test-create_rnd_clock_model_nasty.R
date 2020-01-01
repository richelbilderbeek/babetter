test_that("use", {
  set.seed(42)
  clock_model <- NA
  while (!beautier::is_clock_model(clock_model)) {
    clock_model <- create_rnd_clock_model_nasty()
  }
  expect_true(beautier::is_clock_model(clock_model))
})
