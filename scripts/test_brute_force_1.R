library(beautier)

brute_force_1_combinations_fixed_crown_age <- function() {

  n_fail <- 0

  input_fasta_filename <- beautier::get_beautier_path("anthus_aco.fas")

  for (site_model in beautier:::create_site_models()) {
    for (clock_model in beautier:::create_clock_models()) {
      for (tree_prior in beautier:::create_tree_priors()) {

        output_xml_filename <- tempfile()
        create_beast2_input_file(
          input_filenames = input_fasta_filename,
          site_models = site_model,
          clock_models = clock_model,
          tree_priors = tree_prior,
          output_filename = output_xml_filename,
          posterior_crown_age = 15
        )
        is_ok <- beastier::is_beast2_input_file(
          output_xml_filename,
          show_warnings = TRUE
        )
        testthat::expect_true(is_ok)
        if (!is_ok) {
          print(paste(site_model$name, clock_model$name, tree_prior$name))
          beastier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
          n_fail <- n_fail + 1
        }
      }
    }
  }
  n_fail
}

quit(status = n_fail, save = "no")
