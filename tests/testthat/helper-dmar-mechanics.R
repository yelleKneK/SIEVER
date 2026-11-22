# Individual low-count tests of request plumbing, arithmetic and replay opt
# in explicitly. These fixtures do not establish inferential adequacy;
# test-dmar-results.R checks unmodified production counts separately.
siever_local_dmar_mechanics <- function(envir = parent.frame()) {
  withr::local_options(dmar.bootstrap_exploratory = TRUE, .local_envir = envir)
  testthat::local_mocked_bindings(sieve_dmar_boot_required = function(...) 2L,
    .package = "SIEVER", .env = envir)
  original_warn <- get(".dmar_warn", asNamespace("DMAR"))
  testthat::local_mocked_bindings(.dmar_warn = function(message, code, ...) {
    if (!identical(code, "bootstrap_exploratory")) original_warn(message, code, ...)
    invisible(NULL)
  }, .package = "DMAR", .env = envir)
  # A background arithmetic/replay fixture needs the same explicit option
  # in its own R process. Only this test's copied bootstrap is altered.
  bootstrap <- get("sieve_worker_bootstrap", asNamespace("SIEVER"))
  original_body <- body(bootstrap)
  body(bootstrap) <- substitute({ BODY; options(dmar.bootstrap_exploratory = TRUE) }, list(BODY = original_body))
  testthat::local_mocked_bindings(sieve_worker_bootstrap = bootstrap,
    .package = "SIEVER", .env = envir)
  invisible(NULL)
}
