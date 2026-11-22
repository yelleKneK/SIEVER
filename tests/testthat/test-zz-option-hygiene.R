# Runs after the other ordinary test files (testthat takes the files in
# alphabetical order).

test_that("no test file leaves a SIEVER option set for the files after it", {
  # An option set inside a testServer block (sieve.auto_run = "never", for
  # one) outlives its test and keeps every later file's pages from running
  # anything by themselves.
  o <- options()
  now <- o[grep("^sieve(r)?[.]", names(o))]
  expect_identical(now[order(names(now))], siever_options_at_start)
})

test_that("no test file leaves a graphics device open or an Rplots.pdf behind", {
  # A test that reads a ggplot's labels can open the default device and
  # leave Rplots.pdf, which later files check against.
  expect_null(grDevices::dev.list())
  expect_false(file.exists("Rplots.pdf"))
})

test_that("no test file leaves lavaan or lavaan.mi attached or detached", {
  # lavaan and lavaan.mi are the packages SIEVER's own code attaches
  # (sieve_mi_model(), for one), for the call only, and must not be left on
  # the user's search path. The
  # rest of the search path is not compared: shiny::testServer() attaches
  # shiny, and the replay scripts the tests source run library(DMAR), as a
  # user's script does.
  mine <- c("package:lavaan", "package:lavaan.mi")
  expect_identical(intersect(search(), mine), intersect(siever_search_at_start, mine))
})
