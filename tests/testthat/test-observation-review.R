observation_state <- function() {
  d <- data.frame(
    id = c("same", "same", "C", "D", "E", "F", "G", "H"),
    a = c(2, 1, 4, 1, 2, 4, 3, 4), b = c(2, 2, 4, 4, 1, 2, 3, 1),
    c = c(2, 4, 4, 3, 4, 1, 2, 3), d = c(2, 2, 3, 4, 2, 3, 1, 2)
  )
  fixture <- list(data = d, item_cols = names(d)[-1], theory_map = list(Scale = names(d)[-1]),
    name = "Observation fixture", labels = NULL, load_code = character())
  siever_test_study(fixture, labels = FALSE, theory = FALSE, configure = function(s) {
    m <- s$meta
    m$type <- "ordered"
    s$meta <- m
    roles <- sieve_propose_roles(d)
    roles$role <- c("identifier", rep("item", 4))
    s$roles <- roles
  })
}

test_that("raw longstring definitions respect missing breaks and eligibility", {
  d <- as.data.frame(rbind(c(2, 2, NA, 2, 2, 2), rep(NA, 6), c(1, 1, 1, 1, 4, 4)))
  r <- sieve_response_screens(d, min_run = 4, min_answered = 4)
  expect_equal(r$tbl$longstring, c(3, NA, 4))
  expect_equal(r$tbl$answered, c(5, 0, 6))
  expect_equal(r$n_long, 1)
  expect_equal(r$tbl$flags[2], 0)
  s <- sieve_response_screens(d, min_run = 4, min_answered = 4, missing = "skip")
  expect_equal(s$tbl$longstring, c(5, NA, 4))
  expect_equal(s$n_long, 2)
  expect_error(sieve_response_screens(d, min_run = 2.1), "integer")
  expect_error(sieve_response_screens(d, min_answered = 1), "integer")
  expect_error(sieve_response_screens(data.frame(x = c(1, Inf))), "finite")
})

test_that("observation exclusion remains reversible with duplicate imported IDs", {
  st <- observation_state()
  shiny::isolate({
    original <- st$raw
    rule <- sieve_response_screens(st$raw[, -1], min_run = 3)
    rule$tbl$id <- st$raw$id
    out <- sieve_set_case_status(st, c(2L, 5L), "excluded", "Reviewed raw response pattern", rule)
    expect_equal(out$removed, c(2L, 5L))
    expect_identical(sieve_case_index(st), c(1L, 3L, 4L, 6L, 7L, 8L))
    review <- sieve_observation_review(st, response_items = "a")
    expect_equal(review$original_row, 1:8)
    expect_equal(review$status[c(2, 5)], c("excluded", "excluded"))
    expect_equal(review$identifier[1:2], c("same", "same"))
    expect_equal(review[["Response: a"]], as.character(original$a))
    expect_identical(st$removed_cases[[1]]$screening, rule$rule)
    expect_error(sieve_set_case_status(st, 3.2, "excluded"), "integer")
    snap <- sieve_project_snapshot(st)
    fresh <- sieve_new_state()
    sieve_project_restore(fresh, snap)
    expect_equal(sieve_observation_review(fresh)$status, review$status)
    expect_length(fresh$case_actions, 1)
    back <- sieve_set_case_status(fresh, c(5L, 2L), "included", "Restored after review")
    expect_setequal(back$restored, c(2L, 5L))
    expect_identical(fresh$raw, original)
    expect_equal(nrow(fresh$items), 8)
    expect_length(fresh$case_actions, 2)
  })
})

test_that("screening observation actions reject an obsolete dataset key", {
  st <- observation_state()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    session$setInputs(case_use = list(row = 2, status = "excluded", key = "obsolete"))
    expect_equal(nrow(state$raw), 8)
    expect_no_error(output$case_grid)
    key <- observation_key()
    session$setInputs(case_reason = "Reviewed", case_use = list(row = 2, status = "excluded", key = key))
    expect_equal(nrow(state$raw), 7)
    expect_false(2L %in% sieve_case_index(state))
    key <- observation_key()
    session$setInputs(case_use = list(row = 2, status = "included", key = key))
    expect_equal(nrow(state$raw), 8)
    expect_equal(sieve_case_index(state), 1:8)
    r <- careless()
    expect_equal(r$tbl$longstring[1], 4)
    state$items$a <- 6 - state$items$a
    sieve_confirm_types(state)
    session$flushReact()
    expect_equal(careless()$tbl$longstring[1], 4)
  })
})

test_that("block resampling preserves whole sampling units", {
  withr::with_seed(23, {
    expected <- sample.int(6, 6, replace = TRUE)
  })
  withr::with_seed(23, expect_identical(sieve_resample_units(6, 1:6), expected))
  withr::with_seed(12, {
    rows <- sieve_resample_units(6, rep(letters[1:3], each = 2))
    expect_equal(length(rows), 6)
    expect_true(all(vapply(split(rows, ceiling(seq_along(rows) / 2)), function(z) diff(z) == 1L, logical(1))))
  })
  expect_error(sieve_resample_units(2, c(1, NA)), "nonmissing")
})
