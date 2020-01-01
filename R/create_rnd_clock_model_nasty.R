#' May create a random clock model
#' @export
create_rnd_clock_model_nasty <- function() {
  if (create_rnd_bool()) {
    create_rnd_clock_model()
  } else {
    create_rnd_anything()
  }
}

