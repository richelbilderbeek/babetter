#' Creates something
#' @param fasta_filename a FASTA filename
#' @export
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

