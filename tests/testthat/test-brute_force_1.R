context("test-brute_force_1")

test_that("all combinations", {

  # Need at least 6 taxa, else get the error:
  #
  #   'group_sizes_dimension' (5) must be less than the number of taxa (5)
  #
  input_fasta_filename <- beautier::get_beautier_path("test_output_6.fas")

  for (site_model in beautier::create_site_models()) {
    for (clock_model in beautier::create_clock_models()) {
      for (tree_prior in beautier::create_tree_priors()) {
        if (runif(n = 1) > 0.1) next
        output_xml_filename <- tempfile()
        inference_model <- beautier::create_test_inference_model(
          site_model = site_model,
          clock_model = clock_model,
          tree_prior = tree_prior
        )
        beautier::create_beast2_input_file_from_model(
          input_filename = input_fasta_filename,
          output_filename = output_xml_filename,
          inference_model = inference_model
        )
        is_ok <- beastier::is_beast2_input_file(
          output_xml_filename,
          show_warnings = TRUE
        )
        if (!is_ok) {
          print(paste(site_model$name, clock_model$name, tree_prior$name))
          beastier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
        }
        testthat::expect_true(is_ok)
      }
    }
  }
})

test_that("monophyletic MRCA prior with distr", {

  input_fasta_filename <- beautier::get_beautier_path("anthus_aco_sub.fas")
  mrca_prior <- beautier::create_mrca_prior(
    alignment_id = beautier::get_alignment_id(input_fasta_filename),
    taxa_names = beautier::get_taxa_names(input_fasta_filename),
    is_monophyletic = TRUE,
    mrca_distr = beautier::create_normal_distr(
      mean = 15.0, sigma = 1.0
    )
  )

  for (site_model in beautier::create_site_models()) {
    for (clock_model in beautier::create_clock_models()) {
      for (tree_prior in beautier::create_tree_priors()) {

        if (runif(n = 1) > 0.01) next
        output_xml_filename <- tempfile()

        is_valid <- FALSE
        tryCatch({
          inference_model <- beautier::create_test_inference_model(
            site_model = site_model,
            clock_model = clock_model,
            tree_prior = tree_prior,
            mrca_prior = mrca_prior
          )
          beautier::create_beast2_input_file_from_model(
            input_filename = input_fasta_filename,
            output_filename = output_xml_filename,
            inference_model = inference_model
          )
          is_valid <- TRUE
          }, error = function(e) {} # nolint indeed ignore the error
        )
        if (!is_valid) next()

        is_ok <- beastier::is_beast2_input_file(
          output_xml_filename,
          show_warnings = TRUE
        )
        if (!is_ok) {
          print(paste(site_model$name, clock_model$name, tree_prior$name))
          beastier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
        }
        testthat::expect_true(is_ok)
      }
    }
  }
})

test_that("non-monophyletic MRCA prior with distr", {

  input_fasta_filename <- beautier::get_beautier_path("anthus_aco_sub.fas")
  mrca_prior <- beautier::create_mrca_prior(
    alignment_id = beautier::get_alignment_id(input_fasta_filename),
    taxa_names = beautier::get_taxa_names(input_fasta_filename),
    is_monophyletic = FALSE,
    mrca_distr = beautier::create_normal_distr(
      mean = 15.0, sigma = 1.0
    )
  )

  for (site_model in beautier::create_site_models()) {
    for (clock_model in beautier::create_clock_models()) {
      for (tree_prior in beautier::create_tree_priors()) {

        if (runif(n = 1) > 0.01) next
        output_xml_filename <- tempfile()

        is_valid <- FALSE
        tryCatch({
          beautier::create_beast2_input_file(
            input_filename = input_fasta_filename,
            output_filename = output_xml_filename,
            site_model = site_model,
            clock_model = clock_model,
            tree_prior = tree_prior,
            mrca_prior = mrca_prior
          )
          is_valid <- TRUE
          }, error = function(e) {} # nolint indeed ignore the error
        )
        if (!is_valid) next()

        is_ok <- beastier::is_beast2_input_file(
          output_xml_filename,
          show_warnings = TRUE
        )
        if (!is_ok) {
          print(paste(site_model$name, clock_model$name, tree_prior$name))
          beastier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
        }
        testthat::expect_true(is_ok)
      }
    }
  }
})
