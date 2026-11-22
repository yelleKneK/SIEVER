# Behavioral tests of the Dimensionality tab (mod_retention), the local work
# on the items (the reverse coding and the response bounds of the Ingest tab's
# item grid, the wording checks of the Content Validity tab) and the entry to
# the Scientific task assistant, the Reproducible Code tab (mod_repro), the
# bifactor tab (mod_bifactor), and the app shell (app_ui, app_config,
# run_app). Every numeric claim is either re-derived inline from the quantity
# it is supposed to summarize or pinned against the psych/lavaan/DMAR call
# that produced it, so a wrong constant fails rather than passing on a stale
# value.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("psych")
skip_if_not_installed("shiny")

# Wait estimates learn from the timings measured earlier in this R process
# (sieve_rate_note(), including the background EFA workers' start-up), so in
# a full run a slow earlier file can lift an estimate past the 2 seconds
# under which a result runs by itself, and the results these tests read
# would wait for their button. Start from the default rates, as this file
# does when run alone.
sieve_rate_reset()

# A loaded, confirmed state on the primary nine Holzinger and Swineford tests
# (n = 301, three theorized factors): what every analysis module sees after a
# normal ingest.
uncov_state <- function(confirmed = TRUE) {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = confirmed, labels = TRUE,
    theory = TRUE, name = fx$name, configure = function(s) {
      s$roles$role[s$roles$role == "auxiliary"] <- "criterion"
    })
  s
}

# Flatten a renderUI or renderDT payload to a searchable string.
uncov_text <- function(x) {
  if (is.null(x)) return("")
  paste(as.character(unlist(x)), collapse = " ")
}

# Edit the ledger held in a reactiveValues store from outside a reactive
# context (the read needs isolate(); the write does not).
uncov_edit_meta <- function(state, f) {
  m <- shiny::isolate(state$meta)
  state$meta <- f(m)
  shiny::isolate(sieve_confirm_types(state))
  state
}

# ---- mod_retention.R: parallel analysis, MAP, VSS, and fit by k ------------

test_that("parallel analysis returns one eigenvalue per item and retains three factors", {
  # Keep production-valid reference counts; pin only the automatic-run timing estimate.
  local_mocked_bindings(sieve_pa_units = function(...) 0.000001)
  st <- uncov_state()
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", n_iter = 200, max_k = 4)

    X <- x_data()
    expect_equal(dim(X), c(301L, 9L))
    expect_identical(cor_kind(), "pearson") # every HS test is continuous
    expect_identical(theory_k(), 3L)

    p <- pa()
    expect_false(is.null(p))
    # The scree series carries one factor-analytic eigenvalue per item.
    expect_length(p$fa.values, ncol(X))
    # Horn's rule re-derived from the series the module actually used (the
    # 95th percentile here), not from fa.simr, which is psych's RESAMPLED
    # mean and not a percentile at all.
    ref <- p$sieve_ref_values
    expect_identical(
      as.integer(p$nfact),
      as.integer(which(!(p$fa.values[seq_along(ref)] > ref))[1] - 1L)
    )
    # The leading eigenvalues are the ones above the reference, in order.
    expect_true(all(p$fa.values[seq_len(p$nfact)] > ref[seq_len(p$nfact)]))
    expect_false(p$fa.values[p$nfact + 1L] > ref[p$nfact + 1L])
    # Three theorized factors, and parallel analysis agrees on these data.
    expect_identical(as.integer(p$nfact), 3L)

    expect_null(output$gate) # nothing blocks the tab
    expect_match(uncov_text(output$scree_note), "retains 3 factors")
    expect_match(uncov_text(output$overview), "Pearson product-moment")
  })
})

test_that("Common factor VSS computes and their suggested counts are the tabled optima", {
  st <- uncov_state()
  # (VSS's solutions are DMAR::fa_exploratory()'s, and DMAR refuses the
  # nine tests' four factor solution after iterating to lavaan's limit; the
  # wait is held under two seconds here so the first result still runs by itself,
  # the precondition of this test, and the refused rank has no value.)
  local_mocked_bindings(sieve_vss_wait = function(max_k, p, R = NULL) 1)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", n_iter = 20, max_k = 4)

    v <- vss_res()
    expect_false(is.null(v))
    # One MAP and one VSS value per probed k, exactly as many as max_k.
    expect_null(v$map)
    expect_length(v$cfit.1, 4L)
    expect_length(v$cfit.2, 4L)
    expect_true(all(is.finite(v$cfit.1[1:3])))
    expect_true(is.na(v$cfit.1[4]))
    expect_identical(v$reason[4], "not converged")
    expect_match(uncov_text(output$mapvss_note), "DMAR refused the solution at k = 4 (not converged)", fixed = TRUE)

    s <- suggestions()
    # MAP is the minimizing k, VSS the maximizing k, re-derived from the table.
    expect_identical(as.integer(s$vss1), as.integer(which.max(v$cfit.1)))
    expect_identical(as.integer(s$vss2), as.integer(which.max(v$cfit.2)))
    # MAP under-extracts relative to parallel analysis on these data; the
    # complexity 2 VSS peak lands on the theorized three factor solution.
    expect_identical(as.integer(s$vss2), 3L)

    txt <- uncov_text(output$mapvss_note)
    expect_false(grepl("MAP|Velicer", txt))
    expect_match(txt, "factor correlations", fixed = TRUE)
    # The rendered table repeats the same values it was built from.
    expect_match(uncov_text(output$mapvss_tbl), "VSS complexity 1", fixed = TRUE)
  })
})

test_that("fit by k has one row per k with EFA degrees of freedom and k = 3 fits closely", {
  # Under ML with FIML (Auto's estimator) the sequence on
  # the nine tests is estimated at about 2.6 seconds (measured 2.5), above
  # the two seconds at which it runs by itself; this test reads its result,
  # so the estimate is pinned below that on the test's frame.
  test_frame <- environment()
  local_mocked_bindings(sieve_efa_plan_wait = function(...) 1, .env = test_frame)
  st <- uncov_state()
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", n_iter = 20, max_k = 4)

    ft <- fit_res()
    expect_false(is.null(ft))
    expect_equal(nrow(ft), 4L) # one row per k in 1..max_k
    expect_identical(as.integer(ft$k), 1:4)

    # Degrees of freedom of a k-factor EFA on p items: ((p - k)^2 - (p + k)) / 2.
    p_items <- ncol(x_data())
    expect_equal(ft$df, ((p_items - ft$k)^2 - (p_items + ft$k)) / 2)

    # The RMSEA point estimate lies inside its own 90 percent interval.
    ok <- is.finite(ft$rmsea)
    expect_true(all(ft$rmsea_lo[ok] <= ft$rmsea[ok] + 1e-12))
    expect_true(all(ft$rmsea[ok] <= ft$rmsea_hi[ok] + 1e-12))
    # Fit improves monotonically as factors are added.
    expect_true(all(diff(ft$cfi) > 0))
    expect_true(all(diff(ft$rmsea) < 0))

    # (No count is read off the sequence by Hu and Bentler's values, and
    # none is drawn as a reference line; each index is read with its
    # interval.)
    expect_false(exists("fit_k", inherits = FALSE))
    note <- uncov_text(output$fit_note)
    expect_false(grepl("smallest|conventional|RMSEA <=|CFI >=", note))
    expect_match(note, "no value of an index is applied as a cutoff", fixed = TRUE)
    expect_false(grepl("Against the conventions|meets all three", uncov_text(output$fit_tbl)))
    # (The column names the RMSEA level, 90% by default, as in DMAR.)
    expect_match(uncov_text(output$fit_tbl), "RMSEA 90% CI", fixed = TRUE)
    expect_identical(fit_run()$conf_level, 0.90)
  })
})

test_that("the evidence summary reports the range and a heuristic tally that counts VSS once", {
  # Keep production-valid reference counts; pin only the automatic-run timing estimate.
  local_mocked_bindings(sieve_pa_units = function(...) 0.000001)
  # Under ML with FIML (Auto's estimator) the sequence on
  # the nine tests is estimated at about 2.6 seconds (measured 2.5), above
  # the two seconds at which it runs by itself; this test reads its result,
  # so the estimate is pinned below that on the test's frame.
  test_frame <- environment()
  local_mocked_bindings(sieve_efa_plan_wait = function(...) 1, .env = test_frame)
  # The tab reports the evidence criterion by criterion, the range of the
  # suggested counts, and a tally by criterion family that is labeled a
  # heuristic and counts Very Simple Structure once; it recommends no
  # number, gives no plus or minus one band, and calls no criterion the
  # primary one. The behavior is tested in full in
  # test-retention-evidence.R; here the same fixture is
  # read through the module. (The VSS wait is held under two seconds, as in
  # the test above, so VSS runs by itself; DMAR refuses its k = 4.)
  st <- uncov_state()
  local_mocked_bindings(sieve_vss_wait = function(max_k, p, R = NULL) 1)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", n_iter = 200, max_k = 4)

    s <- suggestions()
    sm <- evidence_summary()
    ks <- c(s$pa, s$vss1, s$vss2)
    expect_true(all(is.finite(ks))) # every data-driven criterion returned
    # The range of the suggested counts, re-derived from the suggestions.
    expect_identical(sm$range, as.integer(range(ks)))
    expect_identical(sm$range, c(3L, 3L))
    expect_true(sm$agree)
    # The tally counts families, so the two VSS complexities are one vote.
    fam <- c("Parallel analysis", "VSS", "VSS")
    per_k <- vapply(sort(unique(ks)), function(k) length(unique(fam[ks == k])), integer(1))
    expect_identical(sm$by_k$n_families, per_k)
    expect_identical(sm$n_families, 2L)
    expect_null(s$fit)
    expect_identical(sm$modal, 3L)
    expect_identical(sm$neighborhood, c(2L, 4L))
    # Theory k is reported as a reference, never as a criterion.
    expect_identical(as.integer(s$theory), 3L)
    expect_true(sm$theory$in_suggested)

    reco <- uncov_text(output$summary_reco)
    expect_match(reco, "All 3 criteria that produced a count suggest 3 factors", fixed = TRUE)
    expect_match(reco, "Heuristic tally, not a calibrated consensus", fixed = TRUE)
    expect_match(reco, "3 factors is suggested by 2 of the 2 criterion families", fixed = TRUE)
    expect_match(reco, "2 to 4 factors", fixed = TRUE)
    expect_match(reco, "not a confidence interval", fixed = TRUE)
    expect_match(reco, "SIEVER does not choose the number of factors", fixed = TRUE)
    expect_false(grepl("recommend", reco, ignore.case = TRUE))
    tbl <- uncov_text(output$summary_tbl)
    expect_match(tbl, "Parallel analysis (Horn, 1965)", fixed = TRUE)
    expect_false(grepl("MAP|Velicer", tbl))
    expect_false(grepl("primary", uncov_text(output$summary_note), fixed = TRUE))
  })
})

test_that("the retention settings are carried into the logged parallel analysis call", {
  # Keep production-valid reference counts; pin only the automatic-run timing estimate.
  local_mocked_bindings(sieve_pa_units = function(...) 0.000001)
  # VSS reads DMAR solutions; its first run is pinned to run by itself.
  local_mocked_bindings(sieve_vss_wait = function(max_k, p, R = NULL) 1)
  st <- uncov_state()
  shiny::testServer(mod_retention_server, args = list(state = st), {
    # (Three factors: with DMAR's solutions VSS at more factors on the nine
    # tests waits for its button; the setting is carried the same way.)
    session$setInputs(cor_type = "pearson", criterion = "p95", n_iter = 400, max_k = 3)
    log_p95 <- paste(shiny::isolate(state$codelog), collapse = "\n")
    # The recorded run is the DMAR::parallel_analysis() calls, one per kind of
    # reference data, so the settings appear as the call's arguments: the
    # reference datasets (B = 400), the percentile (0.95), and the reference
    # data (normal and resample, Pearson here).
    expect_match(log_p95, "percentile = 0.95,", fixed = TRUE)
    expect_match(log_p95, "B = 400,", fixed = TRUE)
    expect_match(log_p95, 'reference = "resample",', fixed = TRUE)
    expect_match(log_p95, "sieve_vss_factors(R_items, 3,", fixed = TRUE) # vss probes k up to max_k
    # The percentile choice must take DMAR's quantile column, never a mean
    # column under a percentile label.
    expect_match(log_p95, "reference <- pa_normal$reference_quantile", fixed = TRUE)
    expect_false(grepl("reference <- pa_normal$reference_mean", log_p95, fixed = TRUE))
  })

  st <- uncov_state()
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "pearson", criterion = "mean", n_iter = 200, max_k = 3)
    log_mean <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log_mean, "B = 200,", fixed = TRUE)
    # The mean criterion is applied by comparing against the mean of the
    # normal reference's eigenvalues, and the logged code must say so and
    # must not read the quantile column.
    expect_match(log_mean, "reference <- pa_normal$reference_mean", fixed = TRUE)
    expect_false(grepl("reference <- pa_normal$reference_quantile", log_mean, fixed = TRUE))
    expect_match(log_mean, "mean", fixed = TRUE)
  })

  # The control must govern the retained count, not just the prose: the two
  # criteria read different reference series off the same fitted object.
  for (crit in c("mean", "p95")) {
    st <- uncov_state()
    shiny::testServer(mod_retention_server, args = list(state = st), {
      session$setInputs(cor_type = "pearson", criterion = crit, n_iter = 200, max_k = 3)
      p <- pa()
      expect_false(is.null(p))
      expect_identical(
        p$sieve_reference,
        if (crit == "p95") "quantile_95" else "fa.sim"
      )
      # Re-derive against the series the module actually used, not against a
      # series assumed to be it: pinning fa.simr here would make the guard
      # vacuous.
      ref <- p$sieve_ref_values
      above <- p$fa.values[seq_along(ref)] > ref
      first_fail <- which(!above)[1]
      expected <- if (is.na(first_fail)) length(ref) else first_fail - 1L
      expect_identical(as.integer(p$nfact), as.integer(expected))
    })
  }

  # Declaring the items ordered requests polychoric correlations.
  #
  # These are continuous scores, for which polychoric correlations cannot
  # be computed, so the correlations used are Pearson, and the recorded call
  # names those, not cor = "poly": the recorded call must be the one that
  # ran, and must say what was requested. (Genuinely ordered items,
  # for which polychoric correlations are computed and recorded, are tested
  # in test-retention-evidence.R.)
  st <- uncov_edit_meta(uncov_state(), function(m) {
    m$type <- "ordered"
    m
  })
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", n_iter = 200, max_k = 3)
    expect_identical(cor_kind(), "poly")
    expect_identical(basis()$used, "pearson")
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, 'missing = "listwise",', fixed = TRUE)
    expect_false(grepl("ordered = TRUE", log, fixed = TRUE))
    expect_match(log, "# Requested:  polychoric correlations.", fixed = TRUE)
  })
})

test_that("the dimensionality gate names the unconfirmed and too-few-items states", {
  st <- uncov_state(confirmed = FALSE)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", n_iter = 20, max_k = 3)
    expect_match(uncov_text(output$gate), "Item types await confirmation")
  })

  # Two active items cannot support a factor analysis.
  st <- uncov_edit_meta(uncov_state(), function(m) {
    m$status <- "parked"
    m$status[1:2] <- "active"
    m
  })
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", n_iter = 20, max_k = 3)
    expect_null(x_data())
    expect_match(uncov_text(output$gate), "at least three included items")
  })
})

test_that("the retention display helpers format missing counts and round only for display", {
  expect_identical(fmt_k(3L), "3")
  expect_identical(fmt_k(3.0), "3")
  expect_identical(fmt_k(NA_integer_), "not available")
  expect_identical(fmt_k(NULL), "not available")
  expect_identical(fmt_k(c(1L, 2L)), "not available")
  expect_identical(round_or_na(c(0.123456789, NA)), c(0.1235, NA))
  expect_identical(round_or_na(NULL), NA_real_)
  expect_identical(sieve_finite_or(2.5, NA_real_), 2.5)
  expect_identical(sieve_finite_or(NULL, NA_real_), NA_real_)
  expect_identical(sieve_finite_or(Inf, NA_real_), NA_real_)
  expect_identical(sieve_finite_or(c(1, 2), NA_real_), 1)
})

# ---- Local work on the items: reverse coding, bounds, wording checks -------
# The reverse coding and the response bounds are the item grid's
# (mod_ingest_server: an item's Reverse? and Response bounds cells, input
# cell_edit, and the Response bounds and Reverse? rows above the grid, which
# act on a set of items); the wording checks are on the Content Validity tab
# (mod_content_server, output wording_tbl); the keying balance table is on the
# Ingest tab's Ledger summary panel (mod_ingest_server, output
# reverse_balance_tbl). Reverse? Yes in a cell or the Reverse? row reverse
# scores the items, and Reverse? No undoes it, reflecting within the bounds
# stored with the item and returning exactly the original values. The
# transformation service behind the cell and the row is sieve_apply_reverse().

# The included items by construct, then by name, so that each test's
# targets are fixed.
uncov_listed <- function(meta) {
  act <- meta[meta$status == "active", , drop = FALSE]
  act$item[order(act$construct, act$item)]
}

test_that("reverse coding is an involution and uses the observed bounds by default", {
  # The rows the grid pushes in place after a reversal (it is not drawn
  # again), captured from the session message that carries them. The mock
  # is registered on this test's own frame, outside the testServer block.
  test_frame <- environment()
  pushed <- new.env(parent = emptyenv())
  pushed$rows <- list()
  testthat::local_mocked_bindings(
    sieve_session_message = function(session, type, message) {
      if (identical(type, "sieve_ingest_row")) {
        for (row in message$rows) pushed$rows[[length(pushed$rows) + 1L]] <- row
      }
      invisible(NULL)
    },
    .env = test_frame
  )
  st <- uncov_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    listed <- uncov_listed(shiny::isolate(state$meta))
    expect_length(listed, 9L)
    target <- listed[1]
    original <- shiny::isolate(state$items)[[target]]
    raw_before <- shiny::isolate(state$raw)
    rest_before <- shiny::isolate(state$items)[setdiff(listed, target)]
    lo <- min(original, na.rm = TRUE)
    hi <- max(original, na.rm = TRUE)
    # No bounds are declared for the item, so the observed range is used.
    keyed <- sieve_meta_keying(shiny::isolate(state$meta))
    expect_true(is.na(keyed$rev_bounds[keyed$item == target]))

    # Reverse? Yes in the item's cell.
    session$setInputs(cell_edit = list(item = target, field = "reverse", value = "Yes"))
    flipped <- shiny::isolate(state$items)[[target]]
    # Observed bounds are the default reflection point: new = (min + max) - x.
    expect_equal(flipped, (lo + hi) - original)
    # The range is preserved, which is the point of reflecting rather than negating.
    expect_equal(min(flipped, na.rm = TRUE), lo)
    expect_equal(max(flipped, na.rm = TRUE), hi)
    # The exact arithmetic is recorded in the reproducible script.
    expect_match(paste(shiny::isolate(state$codelog), collapse = "\n"),
      sprintf('items[["%s"]] <- (%g + %g) - items[["%s"]]', target, lo, hi, target),
      fixed = TRUE
    )
    # The bounds used are stored with the item as the observed range, the
    # line above the grid says so, and the item's row (pushed in place)
    # shows them in its Response bounds cell.
    meta <- shiny::isolate(state$meta)
    i <- match(target, meta$item)
    expect_true(meta$reverse[i])
    expect_equal(c(meta$rev_lo[i], meta$rev_hi[i]), as.numeric(c(lo, hi)))
    expect_identical(meta$rev_bounds[i], "observed")
    expect_match(uncov_text(output$grid_note),
      sprintf("%s is now reverse scored within its observed range, %g to %g.", target, lo, hi),
      fixed = TRUE
    )
    row <- pushed$rows[[length(pushed$rows)]]
    expect_identical(row$key, target)
    bounds_cell <- row$cells[[as.character(match("Bounds", sieve_grid_columns()) - 1L)]]
    expect_match(bounds_cell, sprintf("%g to %g", lo, hi), fixed = TRUE)
    expect_match(bounds_cell, "the bounds of its reversal, the observed range (none were declared)", fixed = TRUE)
    # Only the item reversed changed, and the data as loaded, from which the
    # originals are restored, are never changed.
    expect_identical(shiny::isolate(state$items)[setdiff(listed, target)], rest_before)
    expect_identical(shiny::isolate(state$raw), raw_before)

    # Reversing a second time (Reverse? No) returns the original values.
    session$setInputs(cell_edit = list(item = target, field = "reverse", value = "No"))
    twice <- shiny::isolate(state$items)[[target]]
    expect_equal(twice, original)
    expect_lt(max(abs(twice - original)), 1e-9)
    expect_false(shiny::isolate(state$meta)$reverse[i])
    expect_match(uncov_text(output$grid_note), sprintf("%s is no longer reverse scored", target), fixed = TRUE)
    expect_identical(shiny::isolate(state$raw), raw_before)
  })
})

test_that("declared scale bounds override the observed ones and invalid bounds are refused", {
  # Bounds are declared in the item's Response bounds cell (or, for a set
  # of items, in the Response bounds row), and the reversal reads them.
  st <- uncov_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    listed <- uncov_listed(shiny::isolate(state$meta))
    target <- listed[1]
    other <- listed[2]
    original <- shiny::isolate(state$items)[[target]]
    expect_gt(min(original, na.rm = TRUE), 0) # observed bounds differ from 0 and 200
    expect_lt(max(original, na.rm = TRUE), 200)
    items_before <- shiny::isolate(state$items)

    # Declaring 0 to 200 writes the ledger and reverses nothing.
    session$setInputs(cell_edit = list(item = target, field = "bounds", value = "0 to 200"))
    meta <- shiny::isolate(state$meta)
    i <- match(target, meta$item)
    expect_identical(c(meta$rev_lo[i], meta$rev_hi[i]), c(0, 200))
    expect_identical(meta$rev_bounds[i], "declared")
    expect_identical(shiny::isolate(state$items), items_before)
    expect_match(uncov_text(output$grid_note), "Response bounds 0 to 200 declared for 1 item;", fixed = TRUE)

    # The reversal reflects within the declared bounds, not the observed ones.
    session$setInputs(cell_edit = list(item = target, field = "reverse", value = "Yes"))
    expect_equal(shiny::isolate(state$items)[[target]], 200 - original)
    expect_match(paste(shiny::isolate(state$codelog), collapse = "\n"),
      sprintf('items[["%s"]] <- (0 + 200) - items[["%s"]]', target, target),
      fixed = TRUE
    )
    expect_match(uncov_text(output$grid_note), "within the response bounds 0 to 200", fixed = TRUE)
    expect_identical(shiny::isolate(state$meta)$rev_bounds[i], "declared")

    # A min at or above the max is rejected, from the cell and from the row,
    # and nothing is touched: not the data, not the ledger, not the session
    # script. The other item's values lie within 0 to 250, so only the order
    # refuses these bounds; text that is not two numbers is refused too.
    before <- shiny::isolate(state$items)
    meta_before <- shiny::isolate(state$meta)
    log_before <- shiny::isolate(state$codelog)
    for (typed in c("250 to 0", "100 to 100")) {
      session$setInputs(cell_edit = list(item = other, field = "bounds", value = typed))
      expect_match(uncov_text(output$grid_note),
        "The lowest response must be below the highest; nothing was declared.",
        fixed = TRUE
      )
    }
    session$setInputs(cell_edit = list(item = other, field = "bounds", value = "seven"))
    expect_match(uncov_text(output$grid_note), "is not two numbers", fixed = TRUE)
    session$setInputs(bounds_lo = 250, bounds_hi = 0, types_scope = "all")
    session$setInputs(bounds_apply = 1L)
    expect_match(uncov_text(output$grid_note), "the lowest below the highest; nothing was declared.", fixed = TRUE)
    expect_identical(shiny::isolate(state$items), before)
    expect_identical(shiny::isolate(state$meta), meta_before)
    expect_identical(shiny::isolate(state$codelog), log_before)

    # The reversed item keeps the bounds of its reversal (a new declaration
    # leaves it alone and says so), so Reverse? No reflects within 0 to 200
    # again and returns the original values.
    session$setInputs(cell_edit = list(item = target, field = "bounds", value = "1 to 300"))
    meta <- shiny::isolate(state$meta)
    expect_identical(c(meta$rev_lo[i], meta$rev_hi[i]), c(0, 200))
    expect_match(uncov_text(output$grid_note), sprintf("%s is reverse scored already", target), fixed = TRUE)
    session$setInputs(cell_edit = list(item = target, field = "reverse", value = "No"))
    expect_equal(shiny::isolate(state$items)[[target]], original)
  })
})

test_that("restore recovers the originals and clears only the reverse coding note", {
  # The ledger carries a free-text note column and one item is already
  # annotated, so the reverse coding token must be appended, not overwritten.
  st <- uncov_edit_meta(uncov_state(), function(m) {
    m$note <- NA_character_
    m$note[m$item == "t10_addition"] <- "flagged in screening"
    m
  })
  # The reversal is Reverse? Yes in each item's cell, and the restore is the
  # Reverse? row set to No for every included item.
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    flip <- uncov_listed(shiny::isolate(state$meta))[1:2]
    expect_identical(flip[1], "t10_addition")
    keep <- setdiff(shiny::isolate(state$meta)$item, flip)
    originals_before <- shiny::isolate(state$items)
    notes_before <- shiny::isolate(state$meta)$note

    for (it in flip) {
      session$setInputs(cell_edit = list(item = it, field = "reverse", value = "Yes"))
    }

    meta <- shiny::isolate(state$meta)
    expect_identical(meta$note[meta$item == flip[1]], "flagged in screening; reverse coded")
    expect_identical(meta$note[meta$item == flip[2]], "reverse coded")
    expect_true(all(is.na(meta$note[meta$item %in% keep])))

    session$setInputs(types_scope = "all", keying_to = "No")
    session$setInputs(keying_apply = 1L)
    expect_identical(shiny::isolate(state$items), originals_before)
    meta <- shiny::isolate(state$meta)
    expect_identical(meta$note[meta$item == flip[1]], "flagged in screening")
    expect_true(is.na(meta$note[meta$item == flip[2]]))
    expect_identical(meta$note, notes_before)
    expect_false(any(meta$reverse))
    # A change of keying withdraws the confirmation of the item types.
    expect_false(isTRUE(shiny::isolate(state$confirmed)))
    # The restore is logged as the reversal steps that undo it, which a
    # script can run (replayed in test-state-integrity.R), not as
    # `items <- items_original`, an object no script defines.
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_false(grepl("items_original", log, fixed = TRUE))
    expect_match(log, sprintf("Undo the reverse scoring of %s", flip[1]), fixed = TRUE)
    expect_match(log, sprintf("Undo the reverse scoring of %s", flip[2]), fixed = TRUE)

    # A second restore with nothing reverse scored leaves the data alone and
    # records nothing.
    log_n <- length(shiny::isolate(state$codelog))
    session$setInputs(keying_apply = 2L)
    expect_identical(shiny::isolate(state$items), originals_before)
    expect_identical(length(shiny::isolate(state$codelog)), log_n)
    expect_match(uncov_text(output$grid_note),
      "Nothing to change: Reverse? is already No for every included item.",
      fixed = TRUE
    )


  })
})



test_that("the wording checks flag the guideline violations they are built to catch", {
  st <- uncov_edit_meta(uncov_state(), function(m) {
    m$wording[1] <- "I always feel calm and I never worry about my daily work."
    m$wording[2] <- "I do not think that it is not important to check my work."
    m$wording[3] <- "I sometimes rush."
    m$wording[4] <- NA_character_ # unworded items are omitted
    m
  })
  # On the Content Validity tab (mod_content_server, the Item Writing
  # Checks sub-tab).
  shiny::testServer(mod_content_server, args = list(state = st), {
    df <- wording_checks()
    expect_equal(nrow(df), 8L) # nine items less the unworded one
    meta <- shiny::isolate(state$meta)

    row1 <- df[df$item == meta$item[1], ]
    expect_true(row1$double_barreled) # "and" joining two predicates
    expect_true(row1$negation)
    expect_false(row1$double_negative) # one negation only
    expect_true(row1$absolute) # always, never
    expect_false(row1$vague)
    expect_match(row1$found, "always")
    expect_match(row1$found, "never")
    expect_equal(row1$words, 12L)

    row2 <- df[df$item == meta$item[2], ]
    expect_true(row2$negation)
    expect_true(row2$double_negative) # "not" twice
    expect_false(row2$absolute)

    row3 <- df[df$item == meta$item[3], ]
    expect_true(row3$vague) # "sometimes"
    expect_false(row3$negation)
    expect_false(row3$double_barreled)
    expect_equal(row3$words, 3L)

    # The flag count is exactly the sum of the component indicators, with the
    # length and reading-grade terms as specified.
    expected_flags <- as.integer(df$double_barreled) + as.integer(df$negation) +
      as.integer(df$double_negative) + as.integer(df$absolute) +
      as.integer(df$vague) + as.integer(df$words > 20L) +
      as.integer(df$words >= 8L & is.finite(df$grade) & df$grade > 12)
    expect_identical(df$flags, expected_flags)

    expect_match(uncov_text(output$wording_tbl), "Double-barreled", fixed = TRUE)
    expect_match(paste(shiny::isolate(state$codelog), collapse = "\n"),
      "Wording checks",
      fixed = TRUE
    )

    # The checks read the ledger and do not wait for the confirmation of the
    # item types.
    state$confirmed <- FALSE
    session$flushReact()
    expect_equal(nrow(wording_checks()), 8L)
    expect_match(uncov_text(output$wording_tbl), "Double-barreled", fixed = TRUE)
  })
  # With no study loaded the table says where to load one. The table is
  # drawn server side, so its rows are read from the data handed to
  # datatable() (the mock is registered on this test's own frame).
  test_frame <- environment()
  seen <- new.env(parent = emptyenv())
  real_datatable <- sieve_dt
  testthat::local_mocked_bindings(
    sieve_dt = function(data, ...) {
      if ("Note" %in% names(data)) seen$note <- data$Note
      real_datatable(data, ...)
    },
    .env = test_frame
  )
  shiny::testServer(mod_content_server, args = list(state = sieve_new_state()), {
    expect_false(grepl("Double-barreled", uncov_text(output$wording_tbl), fixed = TRUE))
    expect_match(seen$note, "Load a study on the Ingest tab", fixed = TRUE)
  })
})

test_that("reverse-keyed balance counts per construct and names the stray single case", {
  # On the Ingest tab's Ledger summary panel (mod_ingest_server), where the
  # table follows the keying of the ledger. The table is drawn server side, so
  # the rows it shows are read from the data handed to datatable() (the mock
  # is registered on this test's own frame, outside the testServer block).
  test_frame <- environment()
  seen <- new.env(parent = emptyenv())
  real_datatable <- sieve_dt
  testthat::local_mocked_bindings(
    sieve_dt = function(data, ...) {
      if (all(c("construct", "reversed", "note") %in% names(data))) seen$balance <- data
      real_datatable(data, ...)
    },
    .env = test_frame
  )
  shown <- function(construct) seen$balance$note[seen$balance$construct == construct]
  st <- uncov_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    bal <- sieve_reverse_balance(shiny::isolate(state$meta))
    expect_equal(nrow(bal), 3L) # Visual, Textual, Speed
    expect_identical(bal$k, c(3L, 3L, 3L))
    expect_identical(bal$reversed, c(0L, 0L, 0L))
    expect_true(all(grepl("none reversed", bal$note)))
    expect_match(uncov_text(output$reverse_balance_tbl), "Reverse keyed", fixed = TRUE)
    expect_identical(seen$balance, bal)

    # One reverse-keyed item in a construct is the case the table warns about.
    state$meta$reverse[state$meta$construct == "Visual"][1] <- TRUE
    bal <- sieve_reverse_balance(shiny::isolate(state$meta))
    expect_identical(bal$reversed[bal$construct == "Visual"], 1L)
    expect_match(bal$note[bal$construct == "Visual"], "a single reversed item")
    session$flushReact()
    invisible(output$reverse_balance_tbl)
    expect_match(shown("Visual"), "a single reversed item", fixed = TRUE)

    state$meta$reverse[state$meta$construct == "Visual"][2] <- TRUE
    bal <- sieve_reverse_balance(shiny::isolate(state$meta))
    expect_identical(bal$reversed[bal$construct == "Visual"], 2L)
    expect_match(bal$note[bal$construct == "Visual"], "reversed items present")
    session$flushReact()
    invisible(output$reverse_balance_tbl)
    expect_match(shown("Visual"), "reversed items present", fixed = TRUE)
    expect_identical(seen$balance, bal)

    # The table does not wait for the confirmation of the item types.
    seen$balance <- NULL
    state$confirmed <- FALSE
    state$meta$reverse[state$meta$construct == "Textual"][1] <- TRUE
    session$flushReact()
    expect_match(uncov_text(output$reverse_balance_tbl), "Reverse keyed", fixed = TRUE)
    expect_match(shown("Textual"), "a single reversed item", fixed = TRUE)
  })
})

# ---- mod_repro.R: the session script and the methods draft -----------------

uncov_archive_directory <- new.env(parent = emptyenv())

test_that("the session script accumulates steps and downloads its displayed code with verified inputs", {
  uncov_archive_directory$before <- getwd()
  st <- uncov_state()
  shiny::testServer(mod_repro_server, args = list(state = st), {
    empty <- script()
    expect_match(empty, "SIEVER session script", fixed = TRUE)
    expect_false(any(grepl("^# \\[", strsplit(empty, "\n")[[1L]])))
    expect_identical(trimws(output$code, which = "right"), trimws(empty, which = "right"))

    sieve_log(state, "First recorded step", "x <- 1")
    sieve_log(state, "Second recorded step", c("y <- 2", "z <- y + 1"))
    s <- script()
    expect_match(s, "# [1] First recorded step", fixed = TRUE)
    expect_match(s, "# [2] Second recorded step", fixed = TRUE)
    expect_match(s, "z <- y + 1", fixed = TRUE)
    expect_false(grepl("no actions recorded yet", s, fixed = TRUE))
    # The step numbering counts logged steps, not lines.
    expect_equal(length(grep("^# \\[", strsplit(s, "\n")[[1]])), 2L)

    # A repeated identical step is not appended twice.
    sieve_log(state, "Second recorded step", c("y <- 2", "z <- y + 1"))
    expect_identical(script(), s)
    session$flushReact()

    path <- output$dl
    expect_true(file.exists(path))
    members <- utils::untar(path, list = TRUE)
    expect_setequal(members, c("siever_session.R", "siever_inputs.rds", "README.txt"))
    directory <- withr::local_tempdir()
    utils::untar(path, exdir = directory)
    contents <- readLines(file.path(directory, "siever_session.R"), warn = FALSE)
    expect_gt(length(contents), 0L)
    expect_identical(
      sub("[\r\n]+$", "", paste(contents, collapse = "\n")),
      sub("[\r\n]+$", "", output$code)
    )
    expect_match(basename(path), "siever_session\\.tar\\.gz$")
    replay <- new.env(parent = baseenv())
    expect_no_error(withr::with_dir(directory,
      sys.source(file.path(directory, "siever_session.R"), envir = replay)
    ))
    expect_equal(replay$z, 3)
  })
})

test_that("a session archive restores the working directory before the next test", {
  expect_identical(getwd(), uncov_archive_directory$before)
})

test_that("the methods draft assembles on demand with numbers recomputed from the data", {
  st <- uncov_state()
  shiny::testServer(mod_repro_server, args = list(state = st), {
    # Before the button is pressed the panel invites the press, no draft. The
    # button is named "Run a new analysis and draft" (the draft says it runs
    # a new analysis).
    expect_match(output$draft, "Press Run a new analysis and draft", fixed = TRUE)

    session$setInputs(draft_go = 1)
    d <- draft()
    expect_gt(nchar(d), 500L)

    items <- shiny::isolate(state$items)
    meta <- shiny::isolate(state$meta)
    n_complete <- sum(stats::complete.cases(items))
    expect_match(d, sprintf("%d respondents on 9 items", nrow(items)), fixed = TRUE)
    expect_match(d, sprintf("(%d complete on every item", n_complete), fixed = TRUE)
    expect_match(d, "Visual (3 items); Textual (3 items); Speed (3 items)", fixed = TRUE)
    expect_match(d, "3 constructs", fixed = TRUE)

    # The fit reported in the prose is the fit of the CFA the app would run.
    m <- sieve_fit_measures(sieve_fit_cfa(items, meta))
    expect_match(d, sprintf("CFI = %.3f", m["cfi"]), fixed = TRUE)
    # (The RMSEA interval is at the RMSEA level, 90%, as in DMAR, apart from
    # the 95% of the draft's other intervals; sieve_fit_measures() reads it at
    # that level by default.)
    expect_identical(sieve_fit_ci_level(m), 0.90)
    expect_match(d, sprintf(
      "RMSEA = %.3f, 90%% CI [%.3f, %.3f]",
      m["rmsea"], m["rmsea_lo"], m["rmsea_hi"]
    ),
    fixed = TRUE
    )
    # Omega with an interval for each of the three constructs, never a bare
    # point estimate (three 95% intervals), and two intervals at the RMSEA
    # level, 90%: the RMSEA's and the unbiased SRMR's
    # (Maydeu-Olivares, 2017; DMAR::sem_residual_fit()).
    expect_equal(length(gregexpr("95% CI [", d, fixed = TRUE)[[1]]), 3L)
    expect_equal(length(gregexpr("90% CI [", d, fixed = TRUE)[[1]]), 2L)
    expect_match(d, "(unbiased SRMR = ", fixed = TRUE)
    expect_match(d, "Kelley, K., & Pornprasertmanit, S. (2016)", fixed = TRUE)

    expect_identical(output$draft, d)
    path <- output$dl_draft
    expect_true(file.exists(path))
    expect_identical(paste(readLines(path, warn = FALSE), collapse = "\n"), d)
    expect_match(basename(path), "siever_methods_draft\\.md$")

    expect_match(paste(shiny::isolate(state$codelog), collapse = "\n"),
      "methods and results draft",
      fixed = TRUE
    )
  })
})

# ---- mod_bifactor.R: the fitted model, its indices, and the verdict --------

test_that("the bifactor model fits, and its syntax and degrees of freedom follow the ledger", {
  st <- uncov_state()
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    expect_null(res()) # nothing is fit until asked
    expect_match(uncov_text(output$intro), "off by default")

    # The primary nine tests yield an improper (Heywood) bifactor solution, so
    # lavaan warns about a negative residual variance. That is the condition
    # the Loadings test below asserts; silence it here so it is not noise.
    suppressWarnings(session$setInputs(fit = 1))
    r <- res()
    expect_null(r$error)
    expect_s4_class(r$fit, "lavaan")
    expect_null(output$gate)

    meta <- shiny::isolate(state$meta)
    active <- meta$item[meta$status == "active"]
    lines <- strsplit(r$syntax, "\n", fixed = TRUE)[[1]]
    # One general factor over every active item, plus one group factor per
    # construct with at least three active items.
    expect_length(lines, 1L + length(unique(meta$construct)))
    expect_match(lines[1], "^general =~ ")
    expect_setequal(
      trimws(strsplit(sub("^general =~ ", "", lines[1]), "+", fixed = TRUE)[[1]]),
      active
    )
    expect_true(all(grepl("^(Visual|Textual|Speed) =~ ", lines[-1])))

    m <- sieve_fit_measures(r$fit)
    # Nine items give 45 non-redundant covariance moments; the model estimates
    # 9 general loadings, 9 group loadings, and 9 residual variances.
    p_items <- length(active)
    expect_equal(
      as.integer(m["df"]),
      as.integer(p_items * (p_items + 1) / 2 - 3 * p_items)
    )
    expect_equal(as.integer(m["df"]), 18L)
    expect_true(m["cfi"] > 0.95 && m["cfi"] <= 1)
    expect_true(m["rmsea_lo"] <= m["rmsea"] && m["rmsea"] <= m["rmsea_hi"])
    # The headline fit table rounds only for display (three decimals in the
    # column renderer), never in the stored fit measures.
    expect_match(uncov_text(output$fit_tbl), "formatRound(data, 3", fixed = TRUE)
    expect_match(uncov_text(output$syntax), "general =~", fixed = TRUE)

    expect_match(paste(shiny::isolate(state$codelog), collapse = "\n"),
      "orthogonal = TRUE",
      fixed = TRUE
    )
  })
})

test_that("bifactor loadings partition item variance and expose the Heywood case", {
  st <- uncov_state()
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    suppressWarnings(session$setInputs(fit = 1))
    ld <- res()$loadings
    expect_equal(nrow(ld), 9L)
    expect_setequal(unique(ld$group), c("Visual", "Textual", "Speed"))
    # Orthogonal general and group factors: uniqueness is one minus the two
    # squared standardized loadings, unclamped in the raw column.
    expect_equal(ld$uniqueness_raw, 1 - ld$general^2 - ld$group_loading^2)
    expect_identical(ld$uniqueness, pmax(0, ld$uniqueness_raw))

    # The primary nine tests give an improper bifactor solution (a negative
    # residual variance), which the panel must announce rather than hide.
    expect_true(any(ld$uniqueness_raw < 0))
    expect_true(isTRUE(res()$improper))
    expect_match(uncov_text(output$improper), "Improper solution (Heywood case)", fixed = TRUE)
    expect_match(uncov_text(output$load_tbl), "Group loading", fixed = TRUE)
  })
})

test_that("the bifactor indices table and the unidimensionality verdict follow the estimates", {
  st <- uncov_state()
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    suppressWarnings(session$setInputs(fit = 1))
    ix <- res()$indices
    expect_equal(nrow(ix), 4L)
    expect_identical(ix$factor, c("general", "Visual", "Textual", "Speed"))
    expect_true(all(c("ECV", "omega", "omega_H", "omega_HS", "PUC", "H") %in% names(ix)))

    gen <- ix[ix$factor == "general", ]
    # PUC re-derived: the share of item correlations that span two group
    # factors, 1 - sum(choose(k_g, 2)) / choose(p, 2) with three groups of three.
    expect_equal(gen$PUC, 1 - (3 * choose(3, 2)) / choose(9, 2))
    expect_equal(gen$PUC, 0.75)
    # ECV and H re-derived from the standardized loadings of the same fit:
    # ECV is the general share of common variance, H is Hancock and Mueller's
    # (2001) construct replicability.
    ld <- res()$loadings
    gen_sq <- sum(ld$general^2)
    grp_sq <- sum(ld$group_loading^2)
    expect_equal(gen$ECV, gen_sq / (gen_sq + grp_sq))
    expect_equal(gen$H, 1 / (1 + 1 / sum(ld$general^2 / (1 - ld$general^2))))
    expect_true(all(ix$ECV > 0 & ix$ECV < 1))
    expect_true(gen$omega_H < gen$omega) # the group factors add reliable variance
    expect_true(is.na(gen$omega_HS)) # omegaHS is a subscale quantity only
    expect_true(all(is.na(ix$PUC[ix$factor != "general"])))

    # The panel states the three numbers and the conventions for reading
    # them, with their sources, and gives no verdict (the conventions are
    # tested in test-bifactor-bootstrap.R).
    verdict <- uncov_text(output$essential)
    expect_false(grepl("Essentially unidimensional", verdict, fixed = TRUE))
    expect_false(grepl("Not clearly unidimensional", verdict, fixed = TRUE))
    # Each convention is named once, with its source, where its index is
    # defined; the reading points there and cites none.
    expect_match(verdict, "are each named once, with its source, where the index is defined", fixed = TRUE)
    expect_false(grepl("Rodriguez, Reise, and Haviland (2016)", verdict, fixed = TRUE))
    # On these data the general factor does not dominate (ECV well below .70).
    expect_lt(gen$ECV, 0.70)
    expect_match(verdict, sprintf("ECV = %.2f", gen$ECV), fixed = TRUE)
    # The label names the composite the coefficient describes: an unweighted
    # sum of the items as scaled, which is not the model based omega in the
    # indices table underneath. Pin the number, not only the label: the one
    # error the note exists to prevent is the panel reporting some other
    # omega under the raw total heading, so assert the displayed value equals
    # DMAR's robust omega (observed denominator, DMAR's default) on the same
    # complete cases, and that it genuinely differs from the model based
    # omega beneath it, which is what makes the explanatory note necessary.
    expect_match(verdict, "Omega of the raw total score", fixed = TRUE)
    ref_om <- DMAR::reliability_omega(
      data = active_complete(),
      ci_method = "none"
    )
    ref_est <- ref_om$value[ref_om$term == "estimate"]
    # The value renders immediately after the label's closing tag, as
    # "Robust omega (observed denominator) = value", which appears nowhere
    # else on the panel, so this pins the number displayed under the raw
    # total heading.
    expect_match(verdict,
      sprintf("Robust omega (observed denominator) = %.2f (point estimate", ref_est),
      fixed = TRUE
    )
    expect_gt(abs(ref_est - gen$omega), 0.10)
    # These nine ability tests are on wildly different scales (t10_addition
    # runs 30 to 171), so the raw total is dominated by one item and the panel
    # must say so rather than leave two discrepant omegas side by side.
    sds <- vapply(active_complete(), stats::sd, numeric(1), na.rm = TRUE)
    expect_gt(max(sds) / min(sds), 2)
    expect_match(verdict, "Note on the two omegas", fixed = TRUE)

    expect_match(uncov_text(output$idx_tbl), "omegaHS", fixed = TRUE)
    expect_equal(dim(active_complete()), c(301L, 9L))
    # The bootstrap is opt-in: until it is pressed the panel says so.
    expect_match(uncov_text(output$idx_ci_note), "Press Bootstrap index CIs", fixed = TRUE)
  })
})

test_that("the bifactor tab refuses to fit before types are confirmed", {
  st <- uncov_state(confirmed = FALSE)
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    session$setInputs(fit = 1)
    expect_null(res()) # the req() gate stopped the fit
    expect_match(uncov_text(output$gate), "Item types await confirmation")
  })
})

test_that("bifactor syntax needs a construct with at least three active items", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  expect_match(sieve_bifactor_plan(meta)$model, "^general =~ ")
  # Only two active items per construct: no group factor qualifies.
  thin <- meta
  thin$status <- "parked"
  thin$status[c(1, 2, 4, 5, 7, 8)] <- "active"
  expect_identical(sieve_bifactor_plan(thin)$model, "")
  # With min_group lowered the same ledger does produce three group factors.
  expect_length(strsplit(sieve_bifactor_plan(thin, min_group = 2L)$model, "\n")[[1]], 4L)
  # An empty ledger yields empty syntax rather than an error.
  none <- meta
  none$status <- "parked"
  expect_identical(sieve_bifactor_plan(none)$model, "")

  out <- sieve_fit_bifactor(fx$data[, fx$item_cols, drop = FALSE], none)
  expect_match(out$error, "No included item is assigned to a construct", fixed = TRUE)
  expect_null(out$fit)
})

# ---- app_config.R, app_ui.R, run_app.R: the shell ---------------------------

test_that("app_sys resolves paths inside the installed package and the config reads", {
  cfg <- app_sys("golem-config.yml")
  expect_true(nzchar(cfg))
  expect_true(file.exists(cfg))
  expect_identical(basename(cfg), "golem-config.yml")
  expect_identical(cfg, system.file("golem-config.yml", package = "SIEVER"))

  www <- app_sys("app", "www")
  expect_true(dir.exists(www))
  expect_true(all(c("custom.css", "sieve.js") %in% basename(list.files(www))))

  # A file that is not shipped resolves to the empty string, not an error.
  expect_identical(app_sys("no-such-file.yml"), "")


})

test_that("app_ui builds the pipeline navbar with the tabs in their intended order", {
  ui <- app_ui(list())
  expect_s3_class(ui, "shiny.tag.list")
  html <- paste(as.character(ui), collapse = "")

  # Core tabs precede the More menu's specialist tools. Every tool remains
  # mounted and has its bookmarkable value, including the nested links.
  labels <- vapply(sieve_page_catalog()[sieve_workspace_pages()], function(x) x$label, character(1))
  at <- vapply(labels, function(l) {
    # An optional tab's label sits in a span.
    regexpr(paste0(">", l, "(</span>)?[[:space:]]*</a>"), html)
  }, integer(1))
  expect_true(all(at > 0)) # every pipeline tab is present
  expect_identical(order(at), seq_along(labels)) # and in pipeline order

  # Each tab carries its bookmarkable value, and the module UI is mounted.
  for (v in c(
    "ingest", "screen", "content", "retention", "efa", "winnow", "optimize",
    "cfa", "sem", "reliab", "bifactor", "validity", "finalize",
    "irt", "repro"
  )) {
    expect_true(grepl(sprintf('data-value="%s"', v), html, fixed = TRUE))
  }
  expect_match(html, 'id="ws_structure"', fixed = TRUE)
  expect_false(grepl(">[[:space:]]*More[[:space:]]*<", html))
  # Namespaced inputs from the mounted modules appear in the shell.
  expect_true(grepl('id="retention-criterion"', html, fixed = TRUE))
  expect_true(grepl('id="project-assistant"', html, fixed = TRUE))
  expect_match(html, "Scientific task assistant", fixed = TRUE)
  expect_true(grepl("About and Disclaimer", html, fixed = TRUE))
})

test_that("run_app builds a shiny app object without starting a server", {
  app <- run_app()
  expect_s3_class(app, "shiny.appobj")
  expect_true(all(c(
    "httpHandler", "serverFuncSource", "onStart", "options",
    "appOptions"
  ) %in% names(app)))
  # The server half is app_server, with the standard Shiny signature.
  server_fun <- app$serverFuncSource()
  expect_true(is.function(server_fun))
  expect_identical(names(formals(server_fun)), c("input", "output", "session"))
  expect_identical(deparse(body(server_fun)), deparse(body(app_server)))
  expect_null(app$onStart)
  expect_identical(app$options, list())

  # Options passed through reach the app object rather than launching anything.
  app2 <- run_app(options = list(port = 7654L))
  expect_identical(app2$options$port, 7654L)
})

test_that("the parallel analysis criterion changes the retained count when the references disagree", {
  # Keep production-valid reference counts; pin only the automatic-run timing estimate.
  local_mocked_bindings(sieve_pa_units = function(...) 0.000001)
  # The guard the HS9 fixture cannot provide: there the mean and the 95th
  # percentile reference retain the same number of factors, so a build that
  # ignored the criterion entirely would still pass. These data carry two
  # strong factors plus a third so weak that its empirical eigenvalue falls
  # BETWEEN the two references, which is where Horn's (1965) mean rule and
  # Glorfeld's (1995) percentile rule must disagree: the mean retains it, the
  # percentile does not.
  #
  # Both counts are read from ONE module session, so they come from a single
  # set of random draws. That is what makes the comparison deterministic, and
  # it is possible because the simulation is cached upstream of the
  # criterion, so toggling the control does not redraw the reference.
  skip_if_not_installed("psych")
  set.seed(1)
  n <- 220
  f1 <- stats::rnorm(n)
  f2 <- stats::rnorm(n)
  f3 <- stats::rnorm(n)
  col <- function(f, load) load * f + sqrt(1 - load^2) * stats::rnorm(n)
  d <- data.frame(
    a1 = col(f1, .80), a2 = col(f1, .78), a3 = col(f1, .76),
    b1 = col(f2, .78), b2 = col(f2, .76), b3 = col(f2, .74),
    c1 = col(f3, .26), c2 = col(f3, .24), c3 = col(f3, .22)
  )
  meta <- sieve_build_meta(
    d, names(d),
    list(
      A = names(d)[1:3], B = names(d)[4:6],
      C = names(d)[7:9]
    )
  )
  s <- sieve_new_state()
  s$raw <- d
  s$items <- d
  s$id_vars <- character(0)
  s$meta <- meta
  s$confirmed <- TRUE
  s$palette <- "nd"
  s$thresholds <- sieve_default_thresholds()

  shiny::testServer(mod_retention_server, args = list(state = s), {
    session$setInputs(
      cor_type = "pearson", criterion = "mean",
      n_iter = 200, max_k = 4
    )
    p_mean <- pa()
    expect_false(is.null(p_mean))
    raw_first <- pa_raw()

    session$setInputs(criterion = "p95")
    p_p95 <- pa()

    # The simulation was not redrawn: the criterion reads the cached object.
    expect_identical(pa_raw()$fa.values, raw_first$fa.values)
    expect_identical(p_mean$fa.values, p_p95$fa.values)

    # The percentile reference is a genuine quantile: at or above the mean
    # everywhere, and not psych's fa.simr (whose "r" is for resampled).
    expect_identical(p_mean$sieve_reference, "fa.sim")
    expect_identical(p_p95$sieve_reference, "quantile_95")
    expect_equal(unname(p_mean$sieve_ref_values), unname(p_mean$fa.sim))
    expect_true(all(p_p95$sieve_ref_values >= p_mean$fa.sim - 1e-10))
    expect_false(isTRUE(all.equal(
      unname(p_p95$sieve_ref_values),
      unname(p_p95$fa.simr)
    )))

    # The precondition that makes the two rules separate, stated explicitly
    # so a failure says which part broke.
    e3 <- p_mean$fa.values[3]
    expect_gt(e3, p_mean$sieve_ref_values[3])
    expect_lt(e3, p_p95$sieve_ref_values[3])

    # The control governs the answer.
    expect_identical(as.integer(p_mean$nfact), 3L)
    expect_identical(as.integer(p_p95$nfact), 2L)
  })
})

# ---- Screening definitions and the local item utilities --------------------

test_that("the screening tab names the correlation each statistic uses and defines the careless indices where they are shown", {
  ui <- gsub("\\s+", " ", gsub("<[^>]+>", " ", as.character(mod_screen_ui("screen"))))
  from <- regexpr("Corrected item total correlation within construct", ui, fixed = TRUE)
  to <- regexpr("Careless / insufficient-effort responding screens", ui, fixed = TRUE)
  expect_true(from > 0 && to > from)
  item_total <- substr(ui, from, to)
  expect_match(item_total, "Pearson correlation", fixed = TRUE)
  expect_match(item_total, "observed scores", fixed = TRUE)
  expect_match(item_total, "no model is fit here", fixed = TRUE)
  careless <- substr(ui, to, nchar(ui))
  expect_match(careless, "each is defined below with the cutoff in force here", fixed = TRUE)
  st <- uncov_state()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    note <- uncov_text(output$cor_note)
    expect_match(note,
      "Matrix shown: Pearson correlations among 9 active items, on the observed item scores. Complete cases: n = 301 of 301.",
      fixed = TRUE
    )
    # The definitions and the cutoffs in force are rendered in this note with
    # the numbers of the data, not in static help.
    defs <- uncov_text(output$careless_note)
    expect_match(defs, "Longest run of exactly equal raw responses", fixed = TRUE)
    expect_match(defs, "SD of raw responses to the selected items", fixed = TRUE)
    expect_match(defs, "D2 = (x - xbar)' S^-1 (x - xbar)", fixed = TRUE)
    expect_match(defs, "Flag cutoff: at least 5 consecutive equal answers", fixed = TRUE)
    expect_match(defs, "Flagged at SD &lt;= 0", fixed = TRUE)
    expect_match(defs, "flag cutoff is p &lt; 0.001", fixed = TRUE)
    # Ordered items with five categories: the polychoric matrix, named as such.
    fx <- sieve_hs9_fixture()
    five <- as.data.frame(lapply(fx$data[, fx$item_cols], function(v) {
      as.integer(cut(v, breaks = unique(stats::quantile(v, 0:5 / 5)), include.lowest = TRUE))
    }))
    m <- shiny::isolate(state$meta)
    m$type <- "ordered"
    state$items <- five
    state$meta <- m
    sieve_confirm_types(state)
    session$flushReact()
    expect_identical(cor_mat()$kind, "polychoric")
    expect_match(uncov_text(output$cor_note),
      "Matrix shown: polychoric correlations among 9 active items, estimated from the ordered responses (psych).",
      fixed = TRUE
    )
    # Ordered items whose values psych cannot treat as categories: the note
    # says what was requested, what is shown, and the engine's reason, and
    # never that psych is unavailable.
    state$items <- fx$data[, fx$item_cols, drop = FALSE]
    sieve_confirm_types(state)
    session$flushReact()
    cm <- cor_mat()
    expect_identical(cm$kind, "pearson")
    expect_identical(cm$requested, "polychoric")
    expect_false(is.na(cm$reason))
    note <- uncov_text(output$cor_note)
    expect_match(note, "Requested: polychoric correlations. Used: Pearson correlations.", fixed = TRUE)
    expect_match(note, "could not be computed:", fixed = TRUE)
    expect_match(note, "Matrix shown: Pearson correlations among 9 active items", fixed = TRUE)
    expect_false(grepl("unavailable", note, fixed = TRUE))
  })
})

test_that("the local item utilities say that nothing leaves the machine and when an external model is called", {
  # The Scientific task assistant keeps the entry to an external model and
  # its gate. The response bounds and the reverse coding are in the Ingest
  # tab's item grid (its Response bounds and Reverse? rows above the grid,
  # and each item's cells), and the wording checks with their statement on
  # the Content Validity tab, which says that they run on this machine only.
  ai <- gsub("\\s+", " ", gsub("<[^>]+>", " ", as.character(mod_content_ui("content"))))
  content <- gsub("\\s+", " ", gsub("<[^>]+>", " ", as.character(mod_content_ui("content"))))
  ingest_html <- as.character(mod_ingest_ui("ingest"))
  ingest <- gsub("\\s+", " ", gsub("<[^>]+>", " ", ingest_html))
  # The Ingest tab has no Prepare items panel or module and names none.
  ns_siever <- asNamespace("SIEVER")
  expect_false(exists("mod_ingest_prepare_ui", envir = ns_siever, inherits = FALSE))
  expect_false(exists("mod_ingest_prepare_server", envir = ns_siever, inherits = FALSE))
  expect_match(ingest_html, 'data-value="grid"', fixed = TRUE)
  expect_false(grepl('data-value="prepare"', ingest_html, fixed = TRUE))
  expect_false(grepl("Prepare items", ingest, fixed = TRUE))
  # Its controls for a set of items are the rows above the item grid.
  for (id in c(
    "bounds_lo", "bounds_hi", "types_scope", "bounds_apply", "bounds_clear",
    "bounds_labels", "keying_to", "keying_apply"
  )) {
    expect_match(ingest_html, paste0('id="ingest-', id, '"'), fixed = TRUE)
  }
  # The wording checks say they run on this machine only, with no model,
  # and are never sent for external review.
  expect_match(content,
    "plain string heuristic run on this machine only; nothing leaves it and no model is involved",
    fixed = TRUE
  )


  # Both kinds of local work distinguish themselves from the shared external
  # path: the bounds and the reverse coding (the panel's "Runs on this
  # machine only ... and never for this") need no provider, and the wording
  # checks are never sent.

  expect_match(content,
    "Optional external review is available from Scientific task assistant with your own key and payload authorization, and never for these checks",
    fixed = TRUE
  )
  st <- uncov_state()


  # The global entry point opens an inspectable payload and its own initially
  # unchecked authorization, rather than treating a local gate as consent.
  shown <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    showModal = function(ui, ...) shown$ui <- ui,
    .package = "shiny"
  )
  testthat::local_mocked_bindings(sieve_assistant_send = function(...) {
    stop("Opening the assistant must not send an external request")
  })
  shiny::testServer(mod_project_server, args = list(state = st), {
    session$setInputs(assistant = 1)
    dialog <- paste(as.character(shown$ui), collapse = "")
    for (id in c("assist_prepare_external", "assist_payload", "assist_key", "assist_consent", "assist_send")) {
      expect_match(dialog, paste0('id="', session$ns(id), '"'), fixed = TRUE)
    }
    expect_match(dialog, "I authorize sending this exact disclosed payload to the selected provider", fixed = TRUE)
    consent_tag <- regmatches(
      dialog,
      regexpr(
        paste0(
          '<input[^>]*id="',
          session$ns("assist_consent"),
          '"[^>]*>'
        ),
        dialog
      )
    )
    expect_length(consent_tag, 1L)
    expect_false(grepl("checked", consent_tag, fixed = TRUE))
    expect_identical(output$assist_payload, "No external payload prepared.")
    expect_identical(output$assist_response, "No provider response. Local tasks remain available.")
  })
})


# ---- Items tab: recompute only what changed ----------------------------------

# A loaded, confirmed state around a bundled example.
items_state <- function(confirmed = TRUE, full = TRUE) {
  fx <- if (full) sieve_hs_fixture() else sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols, drop = FALSE]
  s <- siever_test_study(fx, confirmed = confirmed, configure = function(s) {
    s$roles$role[s$roles$role == "auxiliary"] <- "criterion"
  })
  s
}
items_inputs <- function(session) {
  session$setInputs(
    coef = "omega", factor_cov = "correlated",
    load_low = .4, salient = .3, communality_low = .3,
    item_total_low = .3, redundancy_high = .8
  )
}

test_that("the Items tab computes nothing before confirmation, and a label, wording, facet or threshold edit recomputes nothing", {
  skip_if_not_installed("DMAR")
  # The automatic overall fit below is decided from the default rates: a
  # rate measured by an earlier test in this process (on a loaded machine)
  # would change the decision, so the measured rates are cleared first.
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  # One worker: the per item refits are counted through a mocked binding,
  # which a forked worker's call would not reach.
  withr::local_options(sieve.cores = 1L)
  seen <- new.env()
  seen$diag <- 0L
  seen$fit <- 0L
  seen$omega <- 0L
  # The Items tab's robust omega is sieve_item_omega_point().
  real <- list(diag = sieve_item_diagnostics, fit = sieve_run_cfa, omega = sieve_item_omega_point)
  local_mocked_bindings(
    sieve_item_diagnostics = function(...) {
      seen$diag <- seen$diag + 1L
      real$diag(...)
    },
    sieve_run_cfa = function(...) {
      seen$fit <- seen$fit + 1L
      real$fit(...)
    },
    sieve_item_omega_point = function(...) {
      seen$omega <- seen$omega + 1L
      real$omega(...)
    },
    withProgress = function(expr, ...) expr
  )
  st <- items_state(confirmed = FALSE)
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    # Loaded, not confirmed: the outputs are evaluated (a test session
    # evaluates every output; the application evaluates a hidden tab's
    # debounced reactives the same way), and nothing is computed.
    items_inputs(session)
    session$elapse(400)
    expect_error(diag(), class = "shiny.silent.error")
    expect_error(overall_fit(), class = "shiny.silent.error")
    expect_identical(c(seen$diag, seen$fit), c(0L, 0L))

    sieve_confirm_types(state)
    session$flushReact()
    session$elapse(400)
    d <- diag()
    expect_false(is.null(d$items))
    expect_identical(c(seen$diag, seen$fit), c(1L, 1L)) # 24 continuous items under ML with FIML (costed as MLR): fit by itself
    expect_true(fit_plan()$automatic)
    expect_false(is.null(overall_fit()))
    expect_match(uncov_text(output$overall_fit), "Fit on the current pool of 24 items (ML with FIML", fixed = TRUE)
    omega_calls <- seen$omega
    expect_gt(omega_calls, 0L)
    key1 <- diag_key()

    # A display label, a wording, a facet: nothing the analysis uses.
    m <- shiny::isolate(state$meta)
    m$label[1] <- "A new display label"
    if ("wording" %in% names(m)) m$wording[2] <- "A new wording"
    m$facet[3] <- "a facet"
    state$meta <- m
    session$flushReact()
    session$elapse(400)
    invisible(output$pool)
    invisible(output$overall_fit)
    invisible(output$rel_summary)
    expect_identical(diag_key(), key1)
    expect_identical(c(seen$diag, seen$fit, seen$omega), c(1L, 1L, omega_calls))
    expect_identical(diag(), d)
    expect_match(uncov_text(output$pool), "A new display label", fixed = TRUE)

    # A threshold recolors the flags and recomputes nothing.
    flagged_at <- function(cut) {
      session$setInputs(load_low = cut)
      session$elapse(400)
      shown <- display()
      shown$Item[grepl("load", shown$Flags, fixed = TRUE)]
    }
    low <- flagged_at(0.4)
    high <- flagged_at(0.95)
    expect_true(all(low %in% high))
    expect_gt(length(high), length(low))
    expect_identical(c(seen$diag, seen$fit, seen$omega), c(1L, 1L, omega_calls))
    expect_identical(flagged_at(0.4), low)

    # A reverse toggle changes the responses: a new analysis, computed once,
    # and only the construct holding the item has its reliability refit.
    item <- m$item[1]
    cn <- m$construct[1]
    shiny::isolate(sieve_apply_reverse(state, item))
    sieve_confirm_types(state)
    session$flushReact()
    session$elapse(400)
    invisible(output$pool)
    invisible(output$overall_fit)
    expect_false(identical(diag_key(), key1))
    # The diagnostics follow (quick); the overall model is from before and
    # waits for Update (the run policy).
    expect_identical(c(seen$diag, seen$fit), c(2L, 1L))
    expect_true(isTRUE(fit_run()$stale))
    d2 <- diag()
    expect_identical(d2$recomputed, cn)
    n_c <- sum(m$status == "active" & m$construct == cn)
    expect_identical(seen$omega - omega_calls, if (n_c >= 4L) 1L + n_c else 1L)
    # The loading of a reversed item changes sign; the others are unchanged
    # (to the precision of the oblimin rotation's random starts).
    expect_equal(d2$items$loading_target[d2$items$item == item],
      -d$items$loading_target[d$items$item == item],
      tolerance = 1e-3
    )
    others <- d$items$item[d$items$construct != cn]
    expect_equal(d2$items$loading_target[match(others, d2$items$item)],
      d$items$loading_target[match(others, d$items$item)],
      tolerance = 1e-3
    )

    # A reassignment touches the two constructs concerned and no other.
    omega_calls <- seen$omega
    m <- shiny::isolate(state$meta)
    to <- setdiff(unique(m$construct), cn)[1]
    session$setInputs(cell_edit = list(item = item, field = "construct", value = to))
    session$elapse(400)
    sieve_confirm_types(state)
    invisible(output$pool)
    d3 <- diag()
    expect_identical(seen$diag, 3L)
    expect_setequal(d3$recomputed, c(cn, to))
    n_from <- sum(m$status == "active" & m$construct == cn) - 1L
    n_to <- sum(m$status == "active" & m$construct == to) + 1L
    expect_identical(
      seen$omega - omega_calls,
      sum(vapply(c(n_from, n_to), function(n) if (n >= 4L) 1L + n else 1L, integer(1)))
    )
  })
})

test_that("an error inside the Items diagnostics or an edit is shown in the status line, not lost", {
  skip_if_not_installed("DMAR")
  local_mocked_bindings(
    sieve_item_diagnostics = function(...) stop("the engine stopped here"),
    withProgress = function(expr, ...) expr
  )
  st <- items_state(full = FALSE)
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    items_inputs(session)
    session$elapse(400)
    expect_null(diag())
    expect_identical(diag_run()$error, "the engine stopped here")
    note <- uncov_text(output$status_line)
    expect_match(note,
      "The item diagnostics stopped with an error and nothing is shown for them: the engine stopped here",
      fixed = TRUE
    )
    expect_match(note, "text-danger", fixed = TRUE)
    expect_no_error(output$pool)
    expect_identical(nrow(display()), 9L)
    # An edit whose code stops: said, with the item. (with_mocked_bindings,
    # scoped to the block: the test server's environment is not a call
    # frame, so a local_ mock made here would outlive the test.)
    with_mocked_bindings(sieve_log = function(...) stop("the log could not be written"), {
      session$setInputs(cell_edit = list(item = "t2_cubes", field = "construct", value = "Elsewhere"))
      expect_match(uncov_text(output$status_line),
        "The edit of t2_cubes stopped with an error (check its row in the table): the log could not be written",
        fixed = TRUE
      )
    })
  })
})

test_that("the overall model on the Items tab waits for Update above two seconds, keeps through a label edit, and says it is from before after the pool changes", {
  skip_if_not_installed("DMAR")
  withr::local_options(sieve.cores = 2L)
  seen <- new.env()
  seen$fit <- 0L
  real_fit <- sieve_run_cfa
  local_mocked_bindings(
    sieve_run_cfa = function(...) {
      seen$fit <- seen$fit + 1L
      real_fit(...)
    },
    sieve_fit_wait = function(p, estimator = "MLR", what = c("cfa", "efa"), k = 1L, ...) 100,
    withProgress = function(expr, ...) expr
  )
  st <- items_state(full = FALSE)
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    items_inputs(session)
    session$elapse(400)
    expect_false(fit_plan()$automatic)
    expect_identical(fit_run()$status, "not run")
    expect_false(isTRUE(fit_run()$stale))
    expect_null(overall_fit())
    expect_identical(seen$fit, 0L)
    txt <- uncov_text(output$overall_fit)
    expect_match(txt, "Overall measurement model: not fit yet.", fixed = TRUE)
    expect_match(txt,
      "The correlated-factor model of the 9 included items under ML with FIML is estimated to take about 2 minutes",
      fixed = TRUE
    )
    expect_match(txt, "waits for Update above", fixed = TRUE)
    expect_match(uncov_text(output$status_line), '-update_go"', fixed = TRUE)
    # The per item diagnostics do not wait for it.
    expect_false(is.null(diag()$items))
    expect_match(uncov_text(output$rel_summary), "Current reliability by construct", fixed = TRUE)

    session$setInputs(fit_go = 1)
    expect_identical(seen$fit, 1L)
    expect_identical(fit_run()$status, "ran")
    expect_false(is.null(overall_fit()))
    txt <- uncov_text(output$overall_fit)
    expect_match(txt, "RMSEA", fixed = TRUE)
    expect_match(txt, "Fit on the current pool of 9 items (ML with FIML", fixed = TRUE)

    # A label edit keeps the fit.
    m <- shiny::isolate(state$meta)
    m$label[1] <- "Another label"
    state$meta <- m
    session$flushReact()
    session$elapse(400)
    invisible(output$overall_fit)
    expect_identical(fit_run()$status, "ran")
    expect_identical(seen$fit, 1L)

    # An exclusion changes the pool: stale, offered again, not refit by itself.
    m$status[9] <- "parked"
    state$meta <- m
    session$flushReact()
    session$elapse(400)
    expect_identical(fit_run()$status, "not run")
    expect_true(fit_run()$stale)
    expect_identical(seen$fit, 1L)
    txt <- uncov_text(output$overall_fit)
    expect_match(txt, "from before, the item pool changed", fixed = TRUE)
    expect_match(uncov_text(output$status_line), "Update (", fixed = TRUE)
    session$setInputs(fit_go = 2)
    expect_identical(seen$fit, 2L)
    expect_match(uncov_text(output$overall_fit), "Fit on the current pool of 8 items", fixed = TRUE)

    # Another model choice is a model not fit yet, not a stale one.
    session$setInputs(factor_cov = "orthogonal")
    expect_identical(fit_run()$status, "not run")
    expect_false(fit_run()$stale)
    expect_identical(seen$fit, 2L)
    expect_match(uncov_text(output$overall_fit), "not fit yet", fixed = TRUE)
  })
})

test_that("at two seconds or less a first overall model on the Items tab is fit by itself, once per pool", {
  skip_if_not_installed("DMAR")
  withr::local_options(sieve.cores = 2L)
  # The default estimate for nine continuous items under ML with FIML is well under
  # five seconds.
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  seen <- new.env()
  seen$fit <- 0L
  real_fit <- sieve_run_cfa
  local_mocked_bindings(
    sieve_run_cfa = function(...) {
      seen$fit <- seen$fit + 1L
      real_fit(...)
    },
    withProgress = function(expr, ...) expr
  )
  shiny::testServer(mod_winnow_server, args = list(state = items_state(full = FALSE)), {
    items_inputs(session)
    session$elapse(400)
    expect_true(fit_plan()$automatic)
    expect_false(is.null(overall_fit()))
    expect_identical(seen$fit, 1L)
    expect_match(uncov_text(output$overall_fit), "Fit on the current pool of 9 items (ML with FIML", fixed = TRUE)
    m <- shiny::isolate(state$meta)
    m$label[2] <- "Changed"
    state$meta <- m
    session$flushReact()
    session$elapse(400)
    invisible(output$overall_fit)
    expect_identical(seen$fit, 1L)
  })
})


# ---- Visible work: estimates first, buttons above ten seconds ---------------

test_that("the page shows busy indicators, the About dialog explains them, and the Ingest chip says what to click next", {
  ui <- as.character(app_ui(NULL))
  expect_match(ui, "shinyBusySpinners = 'true'", fixed = TRUE)
  expect_match(ui, "shinyBusyPulse = 'true'", fixed = TRUE)
  expect_match(ui, "--shiny-spinner-color:#0C2340", fixed = TRUE)
  expect_match(ui, "--shiny-pulse-background:linear-gradient(90deg, #0c2340, #d39f10)", fixed = TRUE)
  # The About dialog, as the server shows it, explains each state the
  # indicators show: the spinner and the bar along the top while it
  # computes, nothing while it waits for the researcher, and red text when
  # a calculation failed.
  shown <- new.env()
  local_mocked_bindings(
    showModal = function(ui, session = NULL) assign("ui", ui, envir = shown),
    .package = "SIEVER"
  )
  shiny::testServer(app_server, {
    session$setInputs(show_about = 1)
  })
  about <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(shown$ui), collapse = " ")))
  expect_match(about, "spinner", ignore.case = TRUE)
  expect_match(about, "bar[^.]*top of the page", ignore.case = TRUE)
  expect_match(about, "computing", ignore.case = TRUE)
  expect_match(about, "Nothing shown[^.]*waiting", ignore.case = TRUE)
  expect_match(about, "Red text[^.]*fail", ignore.case = TRUE)

  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1)
    expect_true(shiny::isolate(state$confirmed))
    chip <- uncov_text(output$status_chip)
    # The estimator label states the resolved estimator, not "ML or MLR": ML
    # with FIML (DMAR's default); the theoretical basis names the CFA first.
    expect_match(chip, "types confirmed (ML with FIML)", fixed = TRUE)
    expect_match(chip, paste(
      "All tabs unlocked. Open CFA to fit the theoretical structure,",
      "or Screening to look at the items first."
    ), fixed = TRUE)
    # The run policy names every result that may run by itself, once, in
    # the page guide, not in the status line as well.
    expect_false(grepl("A first result runs by itself", chip, fixed = TRUE))
    guide <- uncov_text(shiny::isolate(sieve_page_status_ui(sieve_page_status(state, "ingest"), state)))
    expect_match(guide, "the CFA and the Reliability table; the EFA tab's selected solution", fixed = TRUE)
  })
})

test_that("in the browser, the Reliability and CFA tabs compute nothing while hidden: the script holds neither step after a load or a ledger edit until each tab is shown", {
  skip_if_no_browser()
  # An observer is not suspended while its tab is hidden. If the Reliability
  # tab's planning selector and the CFA tab's planning inputs were prefilled
  # by observers that read the fitted results, loading data (and every
  # ledger edit after it) would fit every construct's reliability models and
  # the CFA with neither tab shown: on 68 ordered items about three minutes
  # before the Ingest chip could update, and 90 seconds per label edit. The
  # recorded script says what was computed, so it is read here from the
  # Reproducible Code tab.
  script_text <- function(app) {
    siever_open_tab(app, "repro")
    # The Report sub-tab leads the page, so the Session script sub-tab is
    # opened first: an output of a hidden sub-tab is not computed.
    compat_open_subtab(app, "repro", "Session script", timeout = 60 * 1000)
    paste(app$get_value(output = "repro-code"), collapse = "\n")
  }
  reliability_step <- function(txt) grepl("h_interval <- function", txt, fixed = TRUE) ||
    grepl("reliability_omega(", txt, fixed = TRUE)
  cfa_step <- function(txt) grepl("cfa_fit <- lavaan::cfa(", txt, fixed = TRUE)
  siever_with_app(load_example = TRUE, function(app) {
    expect_match(app$get_html("#ingest-status_chip"), "types confirmed", fixed = TRUE)
    txt <- script_text(app)
    expect_match(txt, "Load bundled example", fixed = TRUE)
    expect_false(reliability_step(txt))
    expect_false(cfa_step(txt))
    # A label edit on the Ingest grid: still nothing from the hidden tabs.
    siever_open_tab(app, "ingest")
    app$set_inputs(
      `ingest-cell_edit` = list(item = "t2_cubes", field = "label", value = "Cubes, relabeled"),
      allow_no_input_binding_ = TRUE, priority_ = "event"
    )
    siever_wait_render(app, timeout = 60 * 1000)
    txt <- script_text(app)
    expect_false(reliability_step(txt))
    expect_false(cfa_step(txt))
    # This nine-item example is estimated within two seconds. Showing each
    # main tab runs its model and records its step without a button click.
    siever_open_tab(app, "reliab")
    siever_wait_render(app, timeout = 120 * 1000)
    expect_match(app$get_html("#reliab-omega_tbl"), "Visual", fixed = TRUE)
    txt <- script_text(app)
    expect_true(reliability_step(txt))
    expect_false(cfa_step(txt))
    siever_open_tab(app, "cfa")
    app$click(selector = ".tab-pane[data-value='cfa'] a.nav-link[data-value='Fit']")
    siever_wait_render(app, timeout = 120 * 1000)
    # DT draws its rows after the Shiny output has been bound.
    app$wait_for_js("document.querySelector('#cfa-fit_tbl tbody')?.textContent.includes('CFI')",
      timeout = 120 * 1000)
    expect_match(app$get_html("#cfa-fit_tbl"), "CFI", fixed = TRUE)
    expect_true(cfa_step(script_text(app)))
    expect_identical(siever_n_output_errors(app), 0L)
  })
})

test_that("requested CFA fits state the estimate, keep labels current, and wait above two seconds", {
  withr::local_options(sieve.cores = 2L)
  seen <- new.env()
  seen$fit <- 0L
  seen$said <- character(0)
  real_run <- sieve_run_cfa
  local_mocked_bindings(
    sieve_run_cfa = function(...) {
      seen$fit <- seen$fit + 1L
      real_run(...)
    },
    withProgress = function(expr, ..., message = NULL, detail = NULL) {
      seen$said <- c(seen$said, paste(message, detail))
      expr
    }
  )
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  # A module test has no visible root navigation tab, so the button remains
  # the explicit trigger. A real-browser test above covers automatic fitting
  # for this small model on the visible main CFA tab.
  shiny::testServer(mod_cfa_server, args = list(state = items_state(full = FALSE)), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    expect_true(is.null(fit()))
    expect_identical(seen$fit, 0L)
    session$setInputs(fit_go = 1)
    expect_false(is.null(fit()))
    expect_identical(seen$fit, 1L)
    expect_match(seen$said, paste0(
      "^CFA: ML, 9 items \\(under 2 seconds\\) ",
      "Fitting the correlated-factors model with factor variance = 1 identification in lavaan.$"
    ))
    expect_no_error(output$fit_tbl)
    m <- shiny::isolate(state$meta)
    m$label[1] <- "Another label"
    state$meta <- m
    session$flushReact()
    invisible(output$fit_tbl)
    expect_identical(seen$fit, 1L) # a label is not part of the model
    sieve_remove_cases(state, 1L, "manual update coverage")
    session$flushReact()
    invisible(output$fit_tbl)
    expect_identical(seen$fit, 1L) # the pool is: the fit is stale, and refit from the button
    session$setInputs(fit_go = 2)
    expect_identical(seen$fit, 2L)
  })
  # Above ten seconds: the gate states the estimate and offers the button;
  # nothing is fit until it is pressed; the diagram and the tables wait.
  seen$fit <- 0L
  seen$said <- character(0)
  local_mocked_bindings(sieve_fit_wait = function(p, estimator = "MLR", what = c("cfa", "efa"), k = 1L, ...) 100)
  shiny::testServer(mod_cfa_server, args = list(state = items_state(full = FALSE)), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    run <- cfa_run()
    expect_true(run$not_run)
    expect_null(run$fit)
    expect_identical(seen$fit, 0L)
    expect_length(seen$said, 0L)
    gate <- uncov_text(output$gate)
    expect_match(gate, "The model is not fit yet.", fixed = TRUE)
    expect_match(gate,
      "Fitting the correlated-factors model of 9 items under ML is estimated to take about 2 minutes",
      fixed = TRUE
    )
    expect_match(gate, '-fit_go"', fixed = TRUE)
    expect_match(gate, "Fit the model", fixed = TRUE)
    expect_false(grepl("could not be fit", gate, fixed = TRUE))
    expect_error(output$fit_tbl, class = "shiny.silent.error")
    expect_match(uncov_text(output$model_note), "Confirmatory measurement model", fixed = TRUE)
    session$setInputs(fit_go = 1)
    expect_identical(seen$fit, 1L)
    expect_false(cfa_run()$not_run %||% FALSE)
    expect_false(is.null(fit()))
    expect_match(seen$said, "CFA: ML, 9 items (about 2 minutes)", fixed = TRUE)
    expect_no_error(output$fit_tbl)
    expect_false(grepl("not fit yet", uncov_text(output$gate), fixed = TRUE))
    # A label keeps the fit; an exclusion makes it stale and offers Refit.
    m <- shiny::isolate(state$meta)
    m$label[2] <- "Another label"
    state$meta <- m
    session$flushReact()
    expect_false(is.null(fit()))
    expect_identical(seen$fit, 1L)
    sieve_remove_cases(state, 1L, "manual update coverage")
    session$flushReact()
    run <- cfa_run()
    expect_false(isTRUE(run$not_run)) # the earlier fit stays on screen under the line
    expect_identical(seen$fit, 1L)
    gate <- uncov_text(output$gate)
    # The status line offers the tab's one Update.
    expect_match(gate, "changed since this model was fit.", fixed = TRUE)
    expect_match(gate, "Update (", fixed = TRUE)
    session$setInputs(fit_go = 2)
    expect_identical(seen$fit, 2L)
    expect_false(is.null(fit()))
  })
})

test_that("the EFA tab's number of factors comparison waits for its button with its estimate and runs asynchronously", {
  withr::local_options(sieve.cores = 2L)
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(
    sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE),
    withProgress = function(expr, ...) expr
  )
  shiny::testServer(mod_efa_server, args = list(state = items_state(full = FALSE)), {
    session$setInputs(
      n_factors = 3, rotation = "oblimin", suppress = 0.3, sort_loadings = TRUE,
      count_to = 5, count_criterion = "BIC", efa_go = 1
    )
    # The pattern matrix does not wait for it, and it does not run by itself
    # (several unrotated fits take seconds even for nine items).
    expect_false(is.null(efa()))
    expect_identical(count_run()$status, "not run")
    expect_length(log$calls, 0L)
    note <- uncov_text(output$count_note)
    expect_match(note, "Not computed yet.", fixed = TRUE)
    expect_match(note, "Fitting the exploratory model at k = 1 to 5 under ML with FIML on 9 items", fixed = TRUE)
    # ML with FIML is estimated as MLR with FIML (sieve_efa_dmar_wait()); one pattern, complete data.
    expect_match(note, sieve_efa_wait_label(sieve_efa_count_wait(9, "ML", 301, 5L, "data", B = 0L, patterns = 1L)), fixed = TRUE)
    expect_match(note, '-count_go"', fixed = TRUE)
    # The table is served server side, so the mock session sees its header
    # only: a one-column Note in place of the comparison's columns.
    expect_match(uncov_text(output$count_tbl), "<th>Note<", fixed = TRUE)
    expect_error(count_display(), class = "shiny.silent.error")
    session$setInputs(count_go = 1)
    expect_length(log$calls, 1L)
    expect_identical(count_run()$status, "running")
    expect_null(count_run()$table)
    log$process$alive <- FALSE
    efa_dmar_settle(session)
    expect_identical(count_run()$status, "ran")
    expect_identical(count_run()$table$factors, 1:5)
    expect_match(uncov_text(output$count_note), "fit unrotated by DMAR", fixed = TRUE)
    expect_true(any(grepl("DMAR::factor_count(", shiny::isolate(state$codelog), fixed = TRUE)))
    # The count of factors does not depend on the rotation: another
    # rotation refits the selected solution, never the comparison.
    session$setInputs(rotation = "varimax", efa_go = 2)
    expect_identical(count_run()$status, "ran")
    expect_length(log$calls, 1L)
  })
})

