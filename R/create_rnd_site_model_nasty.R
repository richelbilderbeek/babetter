#' May create a random site model
#' @export
create_rnd_site_model_nasty <- function() {
  if (create_rnd_bool()) {
    create_rnd_site_model()
  } else {
    create_rnd_anything()
  }
}

