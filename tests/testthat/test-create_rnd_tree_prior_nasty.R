test_that("use", {
  set.seed(42)
  tree_prior <- NA
  while (!beautier::is_tree_prior(tree_prior)) {
    tree_prior <- create_rnd_tree_prior_nasty()
  }
  expect_true(beautier::is_tree_prior(tree_prior))
})
