missing_study_state <- function() {
  siever_test_study(labels = FALSE, theory = FALSE, name = "Missing responses",
    configure = function(s) {
      s$items[1:25, 1] <- NA_real_
    })
}


# Shared fixture builder uses the same load and confirmation services as Ingest.
# Test setup is omitted from the per-action log, matching existing module fixtures.
siever_test_study <- function(fixture = sieve_hs9_fixture(), confirmed = TRUE,
                               labels = TRUE, theory = TRUE, name = fixture$name, configure = NULL) {
  s <- sieve_study_state(fixture, confirmed = FALSE, labels = labels,
    theory = theory, name = name)
  shiny::isolate({
    if (is.function(configure)) configure(s)
    if (confirmed) sieve_confirm_types(s, origin = "auto")
    s$codelog <- character()
    s$replay_data <- list()
    s$replay_states <- list()
    s$replay_steps <- list()
    s$replay_operations <- list()
    s$replay_versions <- NULL
  })
  s
}
