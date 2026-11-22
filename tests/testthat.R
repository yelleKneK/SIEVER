# The test script R CMD check runs. Run the whole suite with
# devtools::test() or testthat::test_local().
# Independent numerical oracles use native fork workers in the same test
# process as callr. processx documents forwarding the earlier SIGCHLD handler:
# https://github.com/r-lib/processx#mixing-processx-and-the-parallel-base-r-package
# Set this before processx loads. This configures the test subprocess only;
# production worker ownership/cancellation is also checked with the flag unset.
if (.Platform$OS.type == "unix" && !nzchar(Sys.getenv("PROCESSX_NOTIFY_OLD_SIGCHLD"))) {
  if ("processx" %in% loadedNamespaces()) {
    stop("Run the package test script in a fresh R process, before processx loads.")
  }
  Sys.setenv(PROCESSX_NOTIFY_OLD_SIGCHLD = "1")
}

library(testthat)
library(SIEVER)

# R CMD check runs a curated subset of the suite: the engine contract and
# identity files, the contracts of the headless analyses, and fast
# scientific anchors. No file below starts a browser or asserts a duration.
# On CRAN (NOT_CRAN not true) the blocks in them that start another R
# process are skipped, and so are the eight blocks that take a second or
# more. One of them holds SIEVER's SRMR to DMAR's over seven kinds of fit;
# a block of two of those fits stays on CRAN, at under half a second, as
# the anchor between the two packages. devtools::check() sets NOT_CRAN, so
# there those blocks run too. NOT_CRAN is read as testthat's skip_on_cran()
# reads it, with as.logical(), so the two agree.
# The whole suite, with the bootstraps at their production counts, the
# background jobs and the browser tests, takes about an hour on an idle
# machine, and several times that inside R CMD check, which limits a package
# to two processes (_R_CHECK_LIMIT_CORES_). It runs with devtools::test() or
# testthat::test_local(), which do not read this file, and here when the
# environment variable SIEVER_FULL_TESTS is true. The subset is chosen from
# the time of each file with NOT_CRAN unset. zz-option-hygiene runs last and
# checks that the files before it left the session's options as they found
# them.
cran_files <- c(
  "contracts", "fixture-contract",
  "dmar-contract", "dmar-engine-identity", "dmar-request-entrypoints", "dmar-api",
  "ave-api-status", "ave-dispatch-refusal", "headless-uncertainty-status",
  "congeneric-fmi", "scientific-anchors", "session-replay-integrity",
  "decisions", "study-workflows", "item-workflows", "item-copies",
  "assistant-privacy",
  "page-guide", "page-update-duration", "finalize-wiring-contract",
  "zz-option-hygiene"
)

if (isTRUE(as.logical(Sys.getenv("SIEVER_FULL_TESTS")))) {
  test_check("SIEVER")
} else {
  test_check("SIEVER", filter = paste0("^(", paste(cran_files, collapse = "|"), ")$"))
}
