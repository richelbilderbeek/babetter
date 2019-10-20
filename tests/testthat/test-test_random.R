context("test-test_random")

create_random <- function(
  input_fasta_filename = beautier::get_beautier_path("anthus_aco.fas")
) {
  output_xml_filename <- tempfile()
  inference_model <- create_rnd_inference_model(input_fasta_filename)

  beautier::create_beast2_input_file_from_model(
    input_filename = input_fasta_filename,
    inference_model = inference_model,
    output_filename = output_xml_filename
  )
  is_ok <- beastier::is_beast2_input_file(
    output_xml_filename,
    show_warnings = TRUE
  )
  if (!is_ok) {
    print("ERROR")
    file.copy(output_xml_filename, "~/bad.xml", overwrite = TRUE)
    beastier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
    print("site inference_model:")
    print(inference_model)
  }
  is_ok
}

test_that("use", {

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
