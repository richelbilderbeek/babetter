#' May create a random tree prior
#' @export
create_rnd_tree_prior_nasty <- function() {
  if (create_rnd_bool()) {
    create_rnd_tree_prior()
  } else {
    create_rnd_anything()
  }
}

