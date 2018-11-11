context("test-brute_force_1")

test_that("all combinations", {

  input_fasta_filename <- beautier:::get_beautier_path("anthus_aco_sub.fas")

  for (site_model in beautier:::create_site_models()) {
    for (clock_model in beautier:::create_clock_models()) {
      for (tree_prior in beautier:::create_tree_priors()) {
        if (runif(n = 1) > 0.001) next
        output_xml_filename <- tempfile()
        beautier::create_beast2_input_file(
          input_filenames = input_fasta_filename,
          output_filename = output_xml_filename,
          site_models = site_model,
          clock_models = clock_model,
          tree_priors = tree_prior
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

  input_fasta_filename <- beautier:::get_beautier_path("anthus_aco_sub.fas")
  mrca_prior <- beautier::create_mrca_prior(
    alignment_id = beautier::get_alignment_id(input_fasta_filename),
    taxa_names = beautier::get_taxa_names(input_fasta_filename),
    is_monophyletic = TRUE,
    mrca_distr = beautier::create_normal_distr(
      mean = 15.0, sigma = 1.0
    )
  )

  for (site_model in beautier:::create_site_models()) {
    for (clock_model in beautier:::create_clock_models()) {
      for (tree_prior in beautier:::create_tree_priors()) {

        if (runif(n = 1) > 0.001) next
        output_xml_filename <- tempfile()
        beautier::create_beast2_input_file(
          input_filenames = input_fasta_filename,
          output_filename = output_xml_filename,
          site_models = site_model,
          clock_models = clock_model,
          tree_priors = tree_prior,
          mrca_priors = mrca_prior
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

test_that("non-monophyletic MRCA prior with distr", {

  input_fasta_filename <- beautier:::get_beautier_path("anthus_aco_sub.fas")
  mrca_prior <- beautier::create_mrca_prior(
    alignment_id = beautier::get_alignment_id(input_fasta_filename),
    taxa_names = beautier::get_taxa_names(input_fasta_filename),
    is_monophyletic = FALSE,
    mrca_distr = beautier::create_normal_distr(
      mean = 15.0, sigma = 1.0
    )
  )

  for (site_model in beautier:::create_site_models()) {
    for (clock_model in beautier:::create_clock_models()) {
      for (tree_prior in beautier:::create_tree_priors()) {

        if (runif(n = 1) > 0.001) next
        output_xml_filename <- tempfile()
        beautier::create_beast2_input_file(
          input_filenames = input_fasta_filename,
          output_filename = output_xml_filename,
          site_models = site_model,
          clock_models = clock_model,
          tree_priors = tree_prior,
          mrca_priors = mrca_prior
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
