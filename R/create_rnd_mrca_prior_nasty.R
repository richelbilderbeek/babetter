#' May create a random MRCA prior
#' @param fasta_filename a FASTA filename
#' @export
create_rnd_mrca_prior_nasty <- function(
  fasta_filename = beautier::get_beautier_path("anthus_aco.fas")
) {
  if (create_rnd_bool()) {
    create_rnd_mrca_prior(fasta_filename)
  } else {
    create_rnd_anything()
  }
}

