context("test-test_random")


test_that("use", {


  # 1 attempts per minute, use ten minutes
  for (i in seq(1, 1 * 10)) {

    seed <- as.integer((as.double(Sys.time())*1000 + Sys.getpid()) %% 2^31)
    set.seed(seed)

    message(i)
    ok <- FALSE
    tryCatch(
      {
        ok <- create_random()
      },
      error = function(e) {} # nolint indeed ignore
    )
    if (ok != TRUE) {
      message(paste("seed:", seed))
    }
    # expect_true(ok)
  }
})
