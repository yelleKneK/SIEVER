# The panel setup is evalq'd into one module frame. This is the dynamic
# binding contract behind the narrowly declared package-usage symbols.
test_that("Finalize panel wires create their declared shared-frame bindings", {
  state <- siever_test_study()
  shiny::testServer(mod_finalize_server, args = list(state = state), {
    frame <- environment(bind)
    expected <- get(".siever_finalize_wire_symbols", asNamespace("SIEVER"))
    present <- vapply(expected, exists, logical(1), envir = frame, inherits = TRUE)
    expect_true(all(present), info = paste("Missing shared bindings:", paste(expected[!present], collapse = ", ")))
    callbacks <- c("bind", "current", "time_section", "run_split", "run_inv_ladder",
      "run_anchor", "run_diagnosis", "run_dmacs", "run_ordinal", "run_align", "section_held")
    for (name in callbacks) {
      callback <- get(name, frame, inherits = TRUE)
      expect_true(is.function(callback), info = name)
      expect_identical(environment(callback), frame, info = name)
    }
    expect_identical(get("state", frame, inherits = TRUE), state)
    expect_identical(get("input", frame, inherits = TRUE), input)
    expect_identical(get("session", frame, inherits = TRUE), session)
    # All initial observers can run after the last panel has installed
    # its bindings; none of these setup checks fits a statistical model.
    session$flushReact()
  })
})
