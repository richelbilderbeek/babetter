context("test-test_random")

create_random <- function(
  input_fasta_filename = beautier::get_beautier_path("anthus_aco.fas")
) {
  input_filename <- beautier::get_beautier_path("anthus_aco.fas")
  output_xml_filename <- tempfile()
  site_model <- create_rnd_site_model()
  clock_model <- create_rnd_clock_model()
  tree_prior <- create_rnd_tree_prior()
  mrca_priors <- create_rnd_mrca_priors(input_filename)

  beautier::create_beast2_input_file(
    input_filename = input_filename,
    output_filename = output_xml_filename,
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    mrca_prior = mrca_priors
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

  skip("Not now")
  seed <- as.integer((as.double(Sys.time())*1000 + Sys.getpid()) %% 2^31)
  set.seed(seed)

  # 1 attempts per minute, use ten minutes
  for (i in seq(1, 1 * 10)) {
    print(i)
    ok <- create_random()
    if (ok == FALSE) {
      print(paste("seed:", seed))
    }
    expect_true(ok)
  }
})
