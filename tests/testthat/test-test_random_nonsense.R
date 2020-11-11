context("test-random_nonsense")

create_rnd_crown_age <- function() {
  values <- c(-1, 0, 1, 15, NA)
  sample(x = values, size = 1)
}


#' Tries hard to creates one random BEAST2 input file
create_random_nonsense <- function(
  input_fasta_filename = beautier::get_beautier_path("anthus_aco.fas")
) {
  output_xml_filename <- tempfile()

  while (1) {

    fasta_filename <- NULL
    site_model <- NULL
    clock_model <- NULL
    tree_prior <- NULL
    mrca_prior <- NULL
    output_xml_filename <- NULL

    tryCatch(
      {
        fasta_filename <- beautier::get_beautier_path("anthus_aco.fas")
        site_model <- create_rnd_site_model_nasty()
        clock_model <- create_rnd_clock_model_nasty()
        tree_prior <- create_rnd_tree_prior_nasty()
        mrca_prior <- create_rnd_mrca_prior_nasty(fasta_filename)
        output_xml_filename <- tempfile()
        beautier::create_beast2_input_file(
          input_filename = fasta_filename,
          output_filename = output_xml_filename,
          site_model = site_model,
          clock_model = clock_model,
          tree_prior = tree_prior,
          mrca_prior = mrca_prior
        )
        return(TRUE)
      },
    error = function(error) {
        whitelist <- c(
          "'clock_model' must be a valid clock model",
          "'tree_prior' must be a valid tree prior",
          "'site_model' must be a valid site model",
          "'mrca_prior' must be a valid MRCA prior"
        )
        if (!beautier::is_in_patterns(line = error$message, patterns = whitelist)) {
          message("ERROR:")
          message(error$message)
          message("Not found in the whitelist")
          message("FULL ERROR:")
          beastier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
          message("site model:")
          message(site_model)
          message("clock model:")
          message(clock_model)
          message("tree prior:")
          message(tree_prior)
          message("mrca prior:")
          message(mrca_prior)
          return(FALSE)
        }
      }
    )
  }
}

test_that("use", {

  seed <- as.integer((as.double(Sys.time())*1000 + Sys.getpid()) %% 2^31)
  set.seed(seed)

  # 1 attempts per minute, use 1 minute
  for (i in seq(1, 100)) {
    set.seed(i + 4)
    message(i)
    ok <- create_random_nonsense()
    if (ok == FALSE) {
      message(paste("seed:", seed))
    }
    expect_true(ok)
  }
})
