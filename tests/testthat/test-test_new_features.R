test_that("use", {

  text <- beautier::create_beast2_input_from_model(
    input_filename = beautier::get_beautier_path("test_output_0.fas"),
    inference_model = beautier::create_inference_model(
      mcmc = beautier::create_mcmc(
        chain_length = 1e7,
        store_every = 1e3,
        pre_burnin = 1e6
      )
    )
  )
  expect_true(beastier::are_beast2_input_lines_deep(text))
})
