# Launcher for the end to end (shinytest2) tests.
#
# shinytest2 starts the app in a separate R process, so that process has to
# get SIEVER on its own, and it has to get the SIEVER that is under test.
#
# When this file sits inside a package source tree (the usual case during
# devtools::test(), where SIEVER is only ever pkgload::load_all()ed), the
# source tree is loaded, whether or not some SIEVER is also installed in a
# library on the path. shinytest2 0.5.0 and later arrange the same thing
# themselves, by turning library(SIEVER) in the app process into a load of the
# package under development; an earlier shinytest2 would attach the installed
# copy and report its results as the working source's. Loading the source
# tree here makes the outcome the same under either.
#
# When there is no source tree in that position (R CMD check, which runs the
# tests from a copy beside the package it has just installed), the installed
# package is the one under test and is the one loaded.
#
# Author: Ken Kelley

# The source tree this launcher belongs to: tests/testthat/apps/siever is four
# levels below the package root. Only that exact position counts, so a check
# directory that happens to sit inside a source tree is not mistaken for one.
sieve_launcher_source_root <- function(start = getwd()) {
  root <- normalizePath(file.path(start, "..", "..", "..", ".."), mustWork = FALSE)
  desc <- file.path(root, "DESCRIPTION")
  if (!file.exists(desc) || !dir.exists(file.path(root, "R"))) {
    return(NULL)
  }
  fields <- readLines(desc, warn = FALSE)
  if (!any(grepl("^Package:[[:space:]]*SIEVER[[:space:]]*$", fields))) {
    return(NULL)
  }
  root
}

dmar_source <- Sys.getenv("SIEVER_TEST_DMAR_SOURCE", unset = "")
if (nzchar(dmar_source)) pkgload::load_all(dmar_source, quiet = TRUE, helpers = FALSE)

root <- sieve_launcher_source_root()
if (!is.null(root)) {
  pkgload::load_all(root, quiet = TRUE, helpers = FALSE, export_all = FALSE)
} else if (requireNamespace("SIEVER", quietly = TRUE)) {
  library(SIEVER)
} else {
  stop("Cannot find the SIEVER package: it is neither installed nor above ", getwd())
}

source(file.path("..", "protocol.R"), local = TRUE)
siever_browser_protocol(SIEVER::run_app())
