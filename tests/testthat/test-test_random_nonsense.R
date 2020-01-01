context("test-random_nonsense")

create_rnd_anything <- function(
  fasta_filename = beautier::get_beautier_path("anthus_aco.fas")
) {
  anything_index <- sample(x = 1:14, size = 1)

  if (anything_index == 1) {
    create_rnd_site_model()
  } else if (anything_index == 2) {
    create_rnd_clock_model()
  } else if (anything_index == 3) {
    create_rnd_tree_prior()
  } else if (anything_index == 4) {
    create_rnd_gamma_site_model()
  } else if (anything_index == 5) {
    create_rnd_distr()
  } else if (anything_index == 6) {
    create_rnd_freq_equilibrium()
  } else if (anything_index == 7) {
    create_rnd_estimate()
  } else if (anything_index == 8) {
    create_rnd_param()
  } else if (anything_index == 9) {
    "nonsense"
  } else if (anything_index == 10) {
    NA
  } else if (anything_index == 11) {
    NULL
  } else if (anything_index == 12) {
    42
  } else if (anything_index == 13) {
    ape::rcoal(4)
  } else if (anything_index == 14) {
    create_rnd_mrca_prior(fasta_filename)
  } else {
    testit::assert(!"Should not get here")
  }
}

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
          print("ERROR:")
          print(error$message)
          print("Not found in the whitelist")
          print("FULL ERROR:")
          beastier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
          print("site model:")
          print(site_model)
          print("clock model:")
          print(clock_model)
          print("tree prior:")
          print(tree_prior)
          print("mrca prior:")
          print(mrca_prior)
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
    print(i)
    ok <- create_random_nonsense()
    if (ok == FALSE) {
      print(paste("seed:", seed))
    }
    expect_true(ok)
  }
})
