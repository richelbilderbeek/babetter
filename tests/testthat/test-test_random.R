context("test-test_random")

create_random <- function(
  input_fasta_filename = beautier:::get_beautier_path("anthus_aco.fas")
) {
  input_filename <- beautier:::get_beautier_path("anthus_aco.fas")
  output_xml_filename <- tempfile()
  site_model <- beautier:::create_rnd_site_model()
  clock_model <- beautier:::create_rnd_clock_model()
  tree_prior <- beautier:::create_rnd_tree_prior()
  mrca_priors <- beautier:::create_rnd_mrca_priors(input_filename)

  beautier::create_beast2_input_file(
    input_filenames = input_filename,
    output_filename = output_xml_filename,
    site_models = site_model,
    clock_models = clock_model,
    tree_priors = tree_prior,
    mrca_priors = mrca_priors
  )
  is_ok <- beastier::is_beast2_input_file(
    output_xml_filename,
    show_warnings = TRUE
  )
  if (!is_ok) {
    print("ERROR")
    file.copy(output_xml_filename, "~/bad.xml", overwrite = TRUE)
    beastier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
    print("site model:")
    print(site_model)
    print("clock model:")
    print(clock_model)
    print("tree prior:")
    print(tree_prior)
    print("mrca priors:")
    print(mrca_priors)
    print("site model$name:")
    print(site_model$name)
    print("clock model$name:")
    print(clock_model$name)
    print("tree prior$name:")
    print(tree_prior$name)
    print("length(mrca priors):")
    print(length(mrca_priors))
  }
  is_ok
}

test_that("use", {

  seed <- as.integer((as.double(Sys.time())*1000 + Sys.getpid()) %% 2^31)
  set.seed(seed)

  # 30 attempts per minute, use one hour
  for (i in seq(1, 30 * 60)) {
    ok <- create_random()
    if (ok == FALSE) {
      status <- 1
      print(paste("seed:", seed))
      break
    }
    expect_true(ok)
  }
})