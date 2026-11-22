# Model fits on request. In a study of 500 respondents by 68 ordered items,
# CFA, Reliability, Validity, SEM and IRT tabs that fit their models at
# Confirm, after every ledger edit, and on tab view, with nothing saying what
# is running, leave the study unusable. The rule tested here, on the bundled
# nine tests and on such a study (a private project file in the folder
# SIEVER_PRIVATE_PROFILE_DIR names, skipped where it is unset or the file is
# absent): opening a module and confirming the item types fits nothing;
# every fit has a Run button whose label states the estimated time; Run fits
# once; a label or wording edit leaves the result current; a reverse toggle
# or an exclusion marks it stale with one sentence naming what changed, and
# the result stays on screen below that line; Run with nothing changed
# reuses the result and says so; an engine error reaches the status line and
# a notification; a WLSMV fit whose weight matrix would not fit in memory is
# refused before it starts, with the size and the two ways forward.
#
# Fits are counted by mocking lavaan's own lavaan() in its namespace (every
# lavaan::cfa(), lavaan::sem(), lavaan::efa() and DMAR call goes through it;
# one model fit makes several calls, because lavaan fits the baseline models
# for the fit measures) and SIEVER's sieve_run_model(), through which every
# SIEVER fit goes. "No fit" is zero calls of both.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

test_that("fast primary CFA fits do not start a hidden workflow's CFA", {
  seen <- new.env(parent = emptyenv())
  seen$cfa <- 0L
  seen$validity <- 0L
  testthat::local_mocked_bindings(sieve_demand_note = function(...) invisible(NULL), .package = "SIEVER")
  shiny::testServer(function(input, output, session) {
    state <- sieve_new_state()
    state$confirmed <- TRUE
    wire <- function(module) {
      scoped <- session$makeScope(module)
      sieve_demand_wire(scoped$input, scoped, state, "fit_go", "A small CFA",
        key = shiny::reactive(list(choices = list(module = module))),
        plan = shiny::reactive(list(seconds = .1, p = 3L, estimator = "MLR")),
        compute = function(...) {
          seen[[module]] <- seen[[module]] + 1L
          list(value = module)
        }, kind = "cfa"
      )
    }
    primary <- wire("cfa")
    validity <- wire("validity")
  }, {
    session$setInputs(nav = "ingest")
    expect_identical(seen$cfa, 0L)
    expect_identical(seen$validity, 0L)
    session$setInputs(nav = "cfa")
    expect_identical(seen$cfa, 1L)
    expect_identical(seen$validity, 0L)
    session$setInputs(nav = "validity")
    expect_identical(seen$validity, 0L)
    session$setInputs(`validity-fit_go` = 1L)
    expect_identical(seen$validity, 1L)
  })
})

od_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
}

# The bundled nine tests, confirmed or not, continuous or cut into five
# ordered categories, with the variable roles declared so the external
# columns are offered where the roles are read.
od_nine <- function(confirmed = TRUE, ordered = FALSE) {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = confirmed, labels = TRUE,
    theory = TRUE, name = fx$name, configure = function(s) {
  s$roles <- sieve_declared_roles(fx$data, fx$item_cols)
  s$roles$role[s$roles$role == "auxiliary"] <- "criterion"
  s$estimator <- "MLR"
if (ordered) {
    shiny::isolate({
      s$items <- as.data.frame(lapply(s$items, function(v) as.integer(cut(as.numeric(v), breaks = 5))))
      m <- s$meta
      m$type <- "ordered"
      s$meta <- m
      s$estimator <- "WLSMV"
    })
  }
  })
  s
}

# A private study: 500 respondents, 68 ordered items, one construct with
# six facets, confirmed under WLSMV. Opened as the application opens it,
# then set to await confirmation so that Confirm can be timed.
od_private_study_path <- function() siever_profile_file("ethics.siever.rds")
od_private_study <- function() {
  skip_if_not(file.exists(od_private_study_path()), "the private 68-item study is not in SIEVER_PRIVATE_PROFILE_DIR")
  requireNamespace("haven", quietly = TRUE)
  snap <- sieve_project_read(od_private_study_path())
  s <- sieve_new_state()
  sieve_project_restore(s, snap, source = "ethics.siever.rds")
  s$confirmed <- FALSE
  s
}

# Count every lavaan model fit and every SIEVER fit made while a test runs,
# and keep the notifications shown. Installed in the caller's frame.
od_count <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$lavaan <- 0L
  seen$runs <- 0L
  seen$notes <- character(0)
  real_lavaan <- lavaan::lavaan
  testthat::local_mocked_bindings(
    lavaan = function(...) {
      seen$lavaan <- seen$lavaan + 1L
      real_lavaan(...)
    },
    .package = "lavaan", .env = env
  )
  real_run <- sieve_run_model
  testthat::local_mocked_bindings(
    sieve_run_model = function(...) {
      seen$runs <- seen$runs + 1L
      real_run(...)
    },
    sieve_notify = function(message, type = "message", duration = 8) {
      seen$notes <- c(seen$notes, message)
      invisible(NULL)
    },
    .env = env
  )
  seen
}
od_fits <- function(seen) c(lavaan = seen$lavaan, runs = seen$runs)

# Ledger edits made from outside a reactive context, with the flush the
# application makes after each.
od_label <- function(state, session, i = 1L) {
  m <- shiny::isolate(state$meta)
  m$label[i] <- paste(m$label[i], "(renamed)")
  m$wording[i] <- "Reworded for the test."
  state$meta <- m
  session$flushReact()
}
od_exclude <- function(state, session, items) {
  m <- shiny::isolate(state$meta)
  m$status[m$item %in% items] <- "parked"
  state$meta <- m
  session$flushReact()
}
od_reverse <- function(state, session, item) {
  out <- sieve_apply_reverse(state, item, origin = "test")
  sieve_confirm_types(state)
  session$flushReact()
  out
}
od_cfa_inputs <- function(session) {
  session$setInputs(
    structure = "first", ident = "varone", layout = "tree2",
    what = "std", min_path = 0, resid = TRUE, covs = TRUE
  )
}
od_reliab_inputs <- function(session, ...) {
  defaults <- list(
    denominator = "model_implied", ci_method = "mlr", missing = "listwise",
    conf_level = 0.95, B = 1000, boot_seed = NA, show_alpha = TRUE,
    score_method = "Bartlett"
  )
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}

# ---- the stale sentence ----------------------------------------------------

test_that("the stale sentence names what changed, item by item, and ignores labels and wording", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  then <- sieve_ledger_analytic(meta)
  expect_identical(names(then), c("item", "construct", "facet", "type", "status", "reverse"))
  # Labels and wording are not analytic.
  m <- meta
  m$label[1] <- "Another label"
  m$wording[2] <- "Another wording"
  expect_null(sieve_stale_sentence(then, sieve_ledger_analytic(m)))
  expect_identical(sieve_meta_key(m), sieve_meta_key(meta))
  # One reverse, two exclusions: the example sentence.
  m <- meta
  m$reverse[2] <- TRUE
  m$status[c(4, 9)] <- "parked"
  s <- sieve_stale_sentence(then, sieve_ledger_analytic(m))
  expect_identical(s, sprintf(
    "%s reverse scored; 2 items excluded (%s, %s)",
    meta$item[2], meta$item[4], meta$item[9]
  ))
  # A reversal undone, an inclusion, a move, a facet, a type, items that
  # came and went.
  before <- m
  m <- before
  m$reverse[2] <- FALSE
  m$status[4] <- "active"
  m$construct[1] <- "Speed"
  m$facet[5] <- "New facet"
  m$type[6] <- "ordered"
  m <- rbind(m, m[7, ])
  m$item[nrow(m)] <- "t99_new"
  m <- m[m$item != meta$item[8], ]
  s <- sieve_stale_sentence(sieve_ledger_analytic(before), sieve_ledger_analytic(m))
  expect_match(s, sprintf("%s reversal undone", meta$item[2]), fixed = TRUE)
  expect_match(s, sprintf("%s included", meta$item[4]), fixed = TRUE)
  expect_match(s, sprintf("%s moved to construct Speed", meta$item[1]), fixed = TRUE)
  expect_match(s, sprintf("%s moved to facet New facet", meta$item[5]), fixed = TRUE)
  expect_match(s, sprintf("%s retyped as ordered", meta$item[6]), fixed = TRUE)
  expect_match(s, "t99_new added to the ledger", fixed = TRUE)
  # An item that left the ledger is "no longer in the ledger"; "removed" is the respondents' word.
  expect_match(s, sprintf("%s no longer in the ledger", meta$item[8]), fixed = TRUE)
  # More than five items are counted, with the first five named.
  m <- meta
  m$status[1:7] <- "parked"
  s <- sieve_stale_sentence(then, sieve_ledger_analytic(m))
  expect_match(s, "^7 items excluded \\(.* and 2 more\\)$")
  # Another dataset, and values that changed with the ledger unchanged.
  expect_identical(sieve_stale_sentence(then, then, "D1", "D2"), "a different dataset was loaded")
  expect_identical(sieve_stale_sentence(then, then, "D1", "D1", "k1", "k2"), "the item values changed")
  expect_null(sieve_stale_sentence(then, then, "D1", "D1", "k1", "k1"))
  expect_match(sieve_stale_sentence(NULL, then), "was not recorded", fixed = TRUE)
})

# ---- the WLSMV weight-matrix guard -------------------------------------------

test_that("the weight matrix size is lavaan's own, and the guard refuses 303 five-category items and passes 26", {
  # Six items with four categories: 18 thresholds and 15 correlations, the
  # dimension of the weight matrix lavaan holds for the fit.
  fx <- sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols[1:6]]
  d[] <- lapply(d, function(v) as.integer(cut(v, 4)))
  model <- paste0(
    "V =~ ", paste(fx$item_cols[1:3], collapse = " + "), "\n",
    "T =~ ", paste(fx$item_cols[4:6], collapse = " + ")
  )
  fit <- lavaan::cfa(model, d, ordered = names(d), estimator = "WLSMV", std.lv = TRUE)
  gamma <- lavaan::lavInspect(fit, "gamma")
  expect_identical(dim(gamma), c(33L, 33L))
  expect_equal(sieve_wlsmv_weight_bytes(6, 4), 8 * 33^2)
  expect_equal(sieve_wlsmv_weight_bytes(6, 4), 8 * nrow(gamma)^2)
  expect_equal(sieve_wlsmv_weight_bytes(6, rep(4, 6)), 8 * 33^2)
  # Two cases: 303 five-category items need about 17.6 GB (46,965
  # statistics: 1,212 thresholds and 45,753 correlations); 26 need 1.5 MB.
  big <- sieve_wlsmv_weight_bytes(303, 5)
  expect_equal(big, 8 * (303 * 4 + 303 * 302 / 2)^2)
  expect_equal(big, 8 * 46965^2)
  expect_gt(big, 17e9)
  expect_lt(big, 18e9)
  small <- sieve_wlsmv_weight_bytes(26, 5)
  expect_equal(small, 8 * (26 * 4 + 26 * 25 / 2)^2)
  expect_equal(small, 8 * 429^2)
  expect_lt(small, 2e6)
  # A continuous item contributes a mean and a variance.
  expect_equal(sieve_wlsmv_weight_bytes(2, c(5, NA)), 8 * (4 + 2 + 1)^2)
  expect_error(sieve_wlsmv_weight_bytes(-1, 5), "non-negative")
  # The limit is the one every fit is held to unless the option sets
  # another, not a fixed 2 GB on one copy of the matrix.
  expect_identical(sieve_wlsmv_limit(), sieve_memory_limit())
  withr::with_options(list(sieve.wlsmv_limit = 1e6), expect_identical(sieve_wlsmv_limit(), 1e6))

  # The guard on data: 303 items of five categories are refused with the size
  # and both remedies; 26 pass; a continuous (MLR) fit is never refused.
  # Forty rows that use all five categories of every item.
  wide <- as.data.frame(matrix(rep_len(1:5, 40 * 303), 40, 303))
  names(wide) <- sprintf("item_%03d", seq_len(303))
  refused <- sieve_wlsmv_guard(names(wide), wide, estimator = "WLSMV")
  expect_match(refused, "303 items (303 of them ordered or binary)", fixed = TRUE)
  expect_match(refused, "46,965 sample statistics (1,212 thresholds and 45,753 correlations)", fixed = TRUE)
  expect_match(refused, "about 17.6 GB", fixed = TRUE)
  expect_match(refused, sprintf("an estimated %s at its peak, more than the", sieve_bytes_text(sieve_wlsmv_peak_bytes(303, 5, 40))), fixed = TRUE)
  expect_match(refused, "Analyze one construct at a time", fixed = TRUE)
  expect_match(refused, "treat the items as continuous and fit them by maximum likelihood", fixed = TRUE)
  expect_match(refused, "That is a different model", fixed = TRUE)
  expect_match(refused, "Rhemtulla", fixed = TRUE)
  expect_null(sieve_wlsmv_guard(names(wide)[1:26], wide, estimator = "WLSMV"))
  expect_null(sieve_wlsmv_guard(names(wide), wide, estimator = "MLR"))
  expect_null(sieve_wlsmv_guard(names(wide), wide, ordered = character(0), estimator = "WLSMV"))
  for (est in c("DWLS", "WLSM", "ULSMV")) {
    expect_match(sieve_wlsmv_guard(names(wide), wide, estimator = est), sprintf("a %s fit", est), fixed = TRUE)
  }

  # Through the wrappers: the fit is refused before lavaan starts, as a
  # failed status with the message, and the EFA by number of factors says
  # the same in every row.
  meta <- data.frame(
    item = names(wide), label = names(wide), construct = "One",
    facet = NA_character_, type = "ordered", status = "active",
    reverse = FALSE, stringsAsFactors = FALSE
  )
  seen <- od_count()
  run <- sieve_run_cfa(sieve_cfa_spec(meta), wide)
  expect_identical(run$status$code, "failed")
  expect_null(run$fit)
  expect_match(run$error, "about 17.6 GB", fixed = TRUE)
  expect_identical(seen$lavaan, 0L)
  byk <- sieve_efa_fit_by_k(wide, estimator = "WLSMV", ordered = names(wide), max_k = 2L)
  expect_true(all(byk$status == "failed"))
  expect_true(all(grepl("about 17.6 GB", byk$message, fixed = TRUE)))
  expect_identical(seen$lavaan, 0L)
  # Twenty-six items pass the guard and reach the engine.
  small_meta <- meta[1:26, ]
  run <- sieve_run_cfa(sieve_cfa_spec(small_meta), wide[, 1:26])
  expect_false(grepl("GB", run$error %||% "", fixed = TRUE))
  expect_gt(seen$lavaan, 0L)
})

# ---- CFA tab ----------------------------------------------------------------

test_that("CFA tab, nine tests: nothing at open or Confirm, Run fits once, a label edit keeps it, a reverse toggle says so, Run again reuses", {
  seen <- od_count()
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  st <- od_nine(confirmed = FALSE)
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    od_cfa_inputs(session)
    expect_match(od_text(output$gate), "Item types await confirmation", fixed = TRUE)
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))
    # Confirm: nothing is fit; the gate states the estimate with the button.
    state$confirmed <- TRUE
    session$flushReact()
    gate <- od_text(output$gate)
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))
    expect_match(gate, "The model is not fit yet.", fixed = TRUE)
    # Follow the item types gives continuous items ML with FIML (DMAR's
    # default).
    expect_match(gate, "Fitting the correlated-factors model of 9 items under ML is estimated to take", fixed = TRUE)
    expect_match(gate, "Fit the model (", fixed = TRUE)
    expect_match(as.character(output$gate$html), '-fit_go"', fixed = TRUE)
    expect_true(cfa_run()$not_run)
    expect_null(fit())
    expect_error(output$fit_tbl, class = "shiny.silent.error")
    expect_error(output$est_tbl, class = "shiny.silent.error")
    # Every output of the tab, evaluated as a mock session does: still no fit.
    for (id in c("model_note", "vb_rmsea", "phi_tbl", "mi_tbl", "bs_note", "plan_rmsea_result")) {
      invisible(tryCatch(output[[id]], error = function(e) NULL))
    }
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))
    expect_match(od_text(output$model_note), "Confirmatory measurement model", fixed = TRUE)

    # Run: one SIEVER fit, under a progress message naming it, and a
    # notification saying what ran and how long it took.
    session$setInputs(fit_go = 1)
    expect_identical(seen$runs, 1L)
    expect_gt(seen$lavaan, 0L)
    expect_s4_class(fit(), "lavaan")
    expect_identical(cfa_run()$status$code, "ok")
    expect_false(isTRUE(cfa_run()$not_run))
    expect_match(seen$notes[length(seen$notes)], "^CFA: ML, 9 items finished in [0-9.]+ seconds\\.$")
    expect_identical(demand$status()$state, "current")
    # A current fit is stated in the one-line method statement above the
    # tabs (output$method_line), which names the model it holds; the gate
    # holds no line and no button then, and the model note is below the
    # diagram.
    expect_match(
      od_text(output$method_line),
      "correlated-factors CFA, 9 items on 3 factors .*; ML with FIML, .* Fit at [0-9]{2}:[0-9]{2} in [0-9.]+ seconds, current\\."
    )
    gate <- od_text(output$gate)
    expect_false(grepl("not fit yet", gate, fixed = TRUE))
    expect_false(grepl("nothing analytic has changed", gate, fixed = TRUE))
    expect_false(grepl("fit_go", paste(as.character(unlist(output$gate)), collapse = ""), fixed = TRUE))
    expect_no_error(output$fit_tbl)
    expect_no_error(output$est_tbl)
    expect_no_error(output$phi_tbl)
    expect_no_error(output$mi_tbl)
    # The fit measures were read once, with the fit, and are kept with it.
    expect_false(is.null(cfa_run()$measures))
    expect_identical(fm(), cfa_run()$measures)
    after_run <- od_fits(seen)

    # A label and a wording edit: the result is current and nothing is
    # computed, not even lavaan's baseline models for the fit measures.
    od_label(state, session, 1L)
    for (id in c("gate", "fit_tbl", "est_tbl", "phi_tbl", "mi_tbl", "vb_rmsea")) {
      invisible(output[[id]])
    }
    expect_identical(demand$status()$state, "current")
    expect_identical(od_fits(seen), after_run)

    # Run with nothing changed: the result is reused and the notification
    # says so.
    session$setInputs(fit_go = 2)
    expect_identical(od_fits(seen), after_run)
    expect_match(seen$notes[length(seen$notes)], "^Nothing analytic has changed since CFA: ML, 9 items ran at")

    # A reverse toggle: stale, with the sentence, the result still shown,
    # and the Refit button.
    it <- shiny::isolate(state$meta$item[2])
    out <- od_reverse(state, session, it)
    expect_length(out$applied, 1L)
    expect_identical(demand$status()$state, "stale")
    expect_identical(demand$status()$sentence, sprintf("%s reverse scored", it))
    gate <- od_text(output$gate)
    # The status line offers the tab's one Update, not a Refit button.
    expect_match(gate, "The data or the item pool changed since this model was fit.", fixed = TRUE)
    expect_match(gate, sprintf("What changed: %s reverse scored. The result below is from before", it), fixed = TRUE)
    expect_match(gate, "Update (", fixed = TRUE)
    expect_s4_class(fit(), "lavaan") # the earlier fit stays below the line
    expect_no_error(output$fit_tbl)
    expect_identical(
      seen$runs,
      after_run[["runs"]]
    ) # the reversal service itself makes one lavaan call (fct_transform.R), the tab none
    # Inclusion changes refit an already requested model automatically.
    od_exclude(state, session, shiny::isolate(state$meta$item[9]))
    expect_identical(demand$status()$state, "current")
    expect_identical(seen$runs, 2L)
    # Refit: one more SIEVER fit, current again, on the 8 items.
    session$setInputs(fit_go = 3)
    expect_identical(seen$runs, 2L)
    expect_identical(demand$status()$state, "current")
    expect_length(lavaan::lavNames(fit(), "ov"), 8L)
    # Another identification is another model: not fit yet, the first one
    # kept, and shown again when selected again.
    session$setInputs(ident = "marker")
    expect_identical(demand$status()$state, "not_run")
    expect_true(cfa_run()$not_run)
    session$setInputs(ident = "varone")
    expect_identical(demand$status()$state, "current")
    expect_identical(seen$runs, 2L)
  })
})

test_that("CFA tab: an engine error reaches the status line and a notification that stays, and the next Run tries again", {
  seen <- od_count()
  calls <- 0L
  real <- sieve_run_cfa
  testthat::local_mocked_bindings(sieve_run_cfa = function(...) {
    calls <<- calls + 1L
    if (calls == 1L) stop("the engine stopped for the test")
    real(...)
  })
  st <- od_nine()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    od_cfa_inputs(session)
    session$setInputs(fit_go = 1)
    expect_identical(demand$status()$state, "failed")
    expect_identical(demand$status()$error, "the engine stopped for the test")
    gate <- od_text(output$gate)
    expect_match(gate, "CFA: ML, 9 items stopped with an error at", fixed = TRUE)
    expect_match(gate, "the engine stopped for the test", fixed = TRUE)
    expect_match(gate, "Correct the cause, then press Fit the model", fixed = TRUE)
    expect_match(seen$notes[length(seen$notes)], "stopped with an error after .* the engine stopped for the test")
    expect_null(fit())
    expect_error(output$fit_tbl, class = "shiny.silent.error")
    session$setInputs(fit_go = 2)
    expect_identical(demand$status()$state, "current")
    expect_s4_class(fit(), "lavaan")
  })
  # The engine refusing the model (no solution) is announced the same way,
  # with the module's own alert below the line.
  st <- od_nine()
  shiny::isolate({
    m <- st$meta
    m$status[m$construct != "Visual"] <- "parked"
    m$status[1:2] <- "parked" # one Visual item left: no model
    st$meta <- m
  })
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    od_cfa_inputs(session)
    session$setInputs(fit_go = 1)
    run <- cfa_run()
    expect_identical(run$status$code, "failed")
    expect_match(od_text(output$gate), "There is no measurement model to fit", fixed = TRUE)
    expect_match(seen$notes[length(seen$notes)], "produced no solution after", fixed = TRUE)
  })
})

test_that("CFA tab: the weight guard refuses the Run and records the refusal", {
  seen <- od_count()
  withr::local_options(sieve.wlsmv_limit = 1e4)
  st <- od_nine(ordered = TRUE)
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    od_cfa_inputs(session)
    gate <- od_text(output$gate)
    expect_match(gate, "Not fit. Not fit: the weight matrix of a WLSMV fit of these 9 items", fixed = TRUE)
    expect_match(gate, "Analyze one construct at a time", fixed = TRUE)
    session$setInputs(fit_go = 1)
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))
    expect_identical(demand$status()$state, "not_run")
    expect_match(seen$notes[length(seen$notes)], "weight matrix of a WLSMV fit", fixed = TRUE)
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("CFA was not fit: Not fit: the weight matrix of a WLSMV fit", log, fixed = TRUE)))
    recorded <- paste(gsub("^# ", "", log[startsWith(log, "# ")]), collapse = " ")
    expect_match(recorded, "Analyze one construct at a time", fixed = TRUE)
    expect_match(recorded, "treat the items as continuous and fit them by maximum likelihood", fixed = TRUE)
    expect_match(recorded, "Rhemtulla, Brosseau-Liard & Savalei, 2012", fixed = TRUE)
  })
})

test_that("CFA tab, the private 68-item study: Confirm fits nothing, the estimate is on the button, one WLSMV fit on Run, a label edit keeps it, a reverse toggle names the item", {
  skip_on_cran()
  seen <- od_count()
  st <- od_private_study()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    od_cfa_inputs(session)
    started <- proc.time()[["elapsed"]]
    state$confirmed <- TRUE
    session$flushReact()
    for (id in c(
      "gate",
      "model_note",
      "vb_rmsea",
      "fit_tbl",
      "phi_tbl",
      "est_tbl",
      "mi_tbl",
      "bs_note",
      "plan_rmsea_result"
    )) {
      invisible(tryCatch(output[[id]], error = function(e) NULL))
    }
    confirm_seconds <- proc.time()[["elapsed"]] - started
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))
    gate <- od_text(output$gate)
    expect_match(gate,
      "Fitting the correlated-factors model of 68 items under WLSMV is estimated to take",
      fixed = TRUE
    )
    expect_match(gate, "Fit the model (", fixed = TRUE)
    cat(sprintf("\n[68-item CFA] Confirm and every output: %.2f s, no fit\n", confirm_seconds))
    # Run: one WLSMV fit of the 68 items.
    started <- proc.time()[["elapsed"]]
    session$setInputs(fit_go = 1)
    fit_seconds <- proc.time()[["elapsed"]] - started
    expect_identical(seen$runs, 1L)
    expect_s4_class(fit(), "lavaan")
    expect_identical(cfa_run()$spec$estimator, "WLSMV")
    expect_length(lavaan::lavNames(fit(), "ov"), 68L)
    expect_match(seen$notes[length(seen$notes)], "^CFA: WLSMV, 68 items finished in")
    cat(sprintf("[68-item CFA] Run: %.2f s, %d SIEVER fit\n", fit_seconds, seen$runs))
    after_run <- od_fits(seen)
    # A label edit computes nothing, and the fit panel reads the kept measures.
    started <- proc.time()[["elapsed"]]
    od_label(state, session, 1L)
    invisible(output$gate)
    invisible(output$fit_tbl)
    invisible(output$vb_rmsea)
    label_seconds <- proc.time()[["elapsed"]] - started
    expect_identical(od_fits(seen), after_run)
    expect_identical(demand$status()$state, "current")
    cat(sprintf("[68-item CFA] label edit and the fit panel: %.2f s, no fit\n", label_seconds))
    # Run again with nothing changed: reused.
    session$setInputs(fit_go = 2)
    expect_identical(od_fits(seen), after_run)
    # A reverse toggle names the item.
    it <- "item_120"
    out <- od_reverse(state, session, it)
    expect_length(out$applied, 1L)
    expect_identical(demand$status()$state, "stale")
    expect_identical(demand$status()$sentence, "item_120 reverse scored")
    expect_match(od_text(output$gate),
      "What changed: item_120 reverse scored. The result below is from before",
      fixed = TRUE
    )
    expect_identical(seen$runs, after_run[["runs"]])
  })
})

# ---- Reliability tab --------------------------------------------------------

test_that("Reliability tab, nine tests: nothing at Confirm, the menu switch computes nothing, Run computes once, alpha is a display choice, a changed level waits again", {
  seen <- od_count()
  constructs <- 0L
  real_construct <- sieve_reliability_construct
  testthat::local_mocked_bindings(sieve_reliability_construct = function(...) {
    constructs <<- constructs + 1L
    real_construct(...)
  })
  # Ordered items: the interval menu moves the selection from robust ML to
  # the counted bootstrap at Confirm, which must not run the whole analysis
  # twice.
  st <- od_nine(confirmed = FALSE, ordered = TRUE)
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    od_reliab_inputs(session)
    expect_match(od_text(output$gate), "Item types await confirmation", fixed = TRUE)
    state$confirmed <- TRUE
    session$flushReact()
    for (id in c(
      "gate", "method_note", "omega_tbl", "status_notes", "footnote", "detail_tbl",
      "omega_plot", "boot_note", "plan_result", "scores_note", "scores_tbl"
    )) {
      invisible(tryCatch(output[[id]], error = function(e) NULL))
    }
    # (The first method on the menu is the recommended interval, BCa for
    # categorical omega.)
    expect_identical(menu()$selected, "recommended")
    expect_identical(constructs, 0L)
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))
    gate <- od_text(output$gate)
    expect_match(gate, "The reliability table is not computed yet.", fixed = TRUE)
    expect_match(gate,
      "Computing omega, H and alpha for 3 constructs (9 items, WLSMV) is estimated to take",
      fixed = TRUE
    )
    expect_match(gate, "Compute reliability (", fixed = TRUE)
    expect_error(output$omega_tbl, class = "shiny.silent.error")
    expect_match(od_text(output$boot_note), "Press Compute reliability (above the table) first", fixed = TRUE)
    # The menu switch is heard by the settings and computes nothing.
    session$setInputs(ci_method = "percentile")
    expect_identical(constructs, 0L)

    session$setInputs(run_go = 1)
    expect_identical(constructs, 3L)
    expect_gt(seen$lavaan, 0L)
    df <- reliab()
    expect_identical(df$construct, c("Visual", "Textual", "Speed"))
    # (Labels name their target and use DMAR's denominator wording.)
    expect_true(all(df$coefficient == categorical_omega_label("model_implied")))
    expect_true(all(df$uncertainty == "pending"))
    expect_true("alpha" %in% names(df))
    expect_match(seen$notes[length(seen$notes)], "^Reliability: 3 constructs, 9 items finished in")
    expect_match(od_text(output$gate), "Reliability: 3 constructs, 9 items at [0-9]{2}:[0-9]{2} in")
    # (The column header is the symbol of Coefficient Alpha.)
    expect_match(od_text(output$omega_tbl), "\u03b1 (comparison)", fixed = TRUE)
    after_run <- c(constructs, od_fits(seen))
    # The checkbox hides the column without a recomputation.
    session$setInputs(show_alpha = FALSE)
    expect_false(grepl("\u03b1 (comparison)", od_text(output$omega_tbl), fixed = TRUE))
    expect_identical(c(constructs, od_fits(seen)), after_run)
    # A label edit keeps the table; Run again reuses it.
    od_label(state, session, 1L)
    invisible(output$omega_tbl)
    invisible(output$status_notes)
    expect_identical(demand$status()$state, "current")
    expect_identical(c(constructs, od_fits(seen)), after_run)
    session$setInputs(run_go = 2)
    expect_identical(c(constructs, od_fits(seen)), after_run)
    expect_match(
      seen$notes[length(seen$notes)],
      "^Nothing analytic has changed since Reliability: 3 constructs, 9 items ran at"
    )
    # Another confidence level is another request: it waits, and the
    # earlier table returns with the earlier level.
    session$setInputs(conf_level = 0.9)
    expect_identical(demand$status()$state, "not_run")
    expect_error(output$omega_tbl, class = "shiny.silent.error")
    expect_identical(c(constructs, od_fits(seen)), after_run)
    session$setInputs(conf_level = 0.95)
    expect_identical(demand$status()$state, "current")
    expect_identical(reliab()$omega, df$omega)
    # A reverse toggle: stale with the sentence, the table still shown.
    it <- shiny::isolate(state$meta$item[1])
    od_reverse(state, session, it)
    expect_identical(demand$status()$state, "stale")
    expect_identical(demand$status()$sentence, sprintf("%s reverse scored", it))
    gate <- od_text(output$gate)
    # (One Update per page: the status line names what is from before and
    # offers the page's Update in place of Recompute reliability.)
    expect_match(gate, "The data or the item pool changed since the reliability table was computed", fixed = TRUE)
    expect_match(gate, "Update (", fixed = TRUE)
    expect_no_error(output$omega_tbl)
    expect_identical(c(constructs, seen$runs), unname(after_run[c(1L, 3L)]))
    session$setInputs(run_go = 3)
    expect_identical(constructs, 6L)
    expect_identical(demand$status()$state, "current")
  })
})

test_that("Reliability tab: the factor scores wait for their own button, with the method text shown first", {
  seen <- od_count()
  st <- od_nine()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    od_reliab_inputs(session)
    note <- od_text(output$scores_note)
    expect_match(note, "The measurement model behind the scores is not fit yet.", fixed = TRUE)
    expect_match(note, "Compute factor scores (", fixed = TRUE)
    expect_match(note, "Bartlett scores", fixed = TRUE)
    expect_match(note, "Skrondal", fixed = TRUE)
    expect_error(output$scores_tbl, class = "shiny.silent.error")
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))
    session$setInputs(scores_go = 1)
    expect_identical(seen$runs, 1L)
    fs <- fscores()
    expect_identical(dim(fs), c(301L, 3L))
    expect_match(od_text(output$scores_note), "301 rows", fixed = TRUE)
    expect_no_error(output$scores_tbl)
    session$setInputs(score_method = "regression")
    expect_identical(seen$runs, 1L) # the method is a display choice
    expect_identical(dim(fscores()), c(301L, 3L))
    od_label(state, session, 2L)
    invisible(output$scores_tbl)
    expect_identical(seen$runs, 1L)
    expect_identical(demand_scores$status()$state, "current")
  })
})

test_that("Reliability tab, the private 68-item study: Confirm computes nothing and the button states the estimate", {
  seen <- od_count()
  st <- od_private_study()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    od_reliab_inputs(session)
    started <- proc.time()[["elapsed"]]
    state$confirmed <- TRUE
    session$flushReact()
    for (id in c(
      "gate", "method_note", "omega_tbl", "status_notes", "footnote", "detail_tbl",
      "omega_plot", "boot_note", "plan_result", "scores_note", "scores_tbl"
    )) {
      invisible(tryCatch(output[[id]], error = function(e) NULL))
    }
    seconds <- proc.time()[["elapsed"]] - started
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))
    # (The recommended interval is the default.)
    expect_identical(menu()$selected, "recommended")
    gate <- od_text(output$gate)
    expect_match(gate,
      "Computing omega, H and alpha for 1 construct (68 items, WLSMV) is estimated to take",
      fixed = TRUE
    )
    expect_match(gate, "Compute reliability (", fixed = TRUE)
    cat(sprintf("\n[68-item Reliability] Confirm and every output: %.2f s, no fit\n", seconds))
    # A reverse toggle before any run: still nothing, and not stale (there
    # is nothing to be stale).
    od_reverse(state, session, "item_120")
    invisible(tryCatch(output$gate, error = function(e) NULL))
    expect_identical(demand$status()$state, "not_run")
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))
  })
})

# ---- Validity tab -----------------------------------------------------------

test_that("Validity tab: criteria stored as numerals in text are offered, a non-numeric one is refused by name, and the three models wait for their buttons", {
  seen <- od_count()
  st <- od_nine()
  shiny::isolate({
    raw <- st$raw
    raw$age_text <- as.character(raw$age_years) # numerals in text
    raw$notes_text <- as.character(raw$age_years)
    raw$notes_text[1:5] <- "n/a" # not numbers
    raw$flag_one <- "1" # one value throughout
    st$raw <- raw
  })
  out <- sieve_apply_roles(st, c(age_text = "criterion", notes_text = "criterion", flag_one = "criterion"))
  shiny::isolate(sieve_confirm_types(st))
  expect_null(out$refused)
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(score_method = "mean", htmt_on = TRUE, marker = "")
    expect_setequal(aux_numeric(), c("age_years", "age_text"))
    refused <- aux_refused()
    expect_identical(sort(names(refused)), c("flag_one", "notes_text"))
    expect_match(refused[["notes_text"]],
      "Criterion 'notes_text' holds 5 values that are not numbers (for example \"n/a\")",
      fixed = TRUE
    )
    expect_match(refused[["flag_one"]], "takes the same value (1) for every case", fixed = TRUE)
    # Nothing is fit on view: the gate states the estimate with the button,
    # the model tables say what to press, and the cheap panels are live.
    for (id in c(
      "gate", "phi_tbl", "ave_tbl", "htmt_tbl", "harman_tbl", "ulmf_note", "ulmf_tbl",
      "crit_note", "sem_note", "direction_verdict"
    )) {
      invisible(tryCatch(output[[id]], error = function(e) NULL))
    }
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))
    gate <- od_text(output$gate)
    expect_match(gate, "The correlated-factors model is not fit yet.", fixed = TRUE)
    expect_match(gate, "Fit the measurement model (", fixed = TRUE)
    # The tables are served server side, so the mock session sees their
    # headers only: a one-column Note in place of the correlations and the
    # AVE, and the live panels with their own columns.
    expect_match(od_text(output$phi_tbl), "Note", fixed = TRUE)
    expect_false(grepl("Construct 1", od_text(output$phi_tbl), fixed = TRUE))
    expect_match(od_text(output$ave_tbl), "Note", fixed = TRUE)
    expect_error(std_solution(), class = "shiny.silent.error")
    expect_error(ave_df(), class = "shiny.silent.error")
    expect_match(od_text(output$htmt_tbl), "Construct pair", fixed = TRUE) # DMAR::htmt is immediate
    expect_match(od_text(output$harman_tbl), "Statistic", fixed = TRUE)
    expect_identical(harman_df()$Statistic[2], "Proportion variance, factor 1")
    expect_match(od_text(output$ulmf_note), "Fit the method-factor model (", fixed = TRUE)
    expect_match(od_text(output$ulmf_tbl), "Note", fixed = TRUE)
    expect_null(demand_ulmf$result())
    # The criterion correlations are live, with the refused criteria named.
    # A factor score asked for before the
    # model is fit is unavailable, with the Fit action and the explicit
    # mean-score choice in the note; no mean score is substituted, and
    # nothing is fit.
    session$setInputs(criteria = c("age_years", "age_text"))
    expect_identical(nrow(crit_df()), 6L)
    session$setInputs(score_method = "bartlett")
    expect_null(construct_scores()$scores)
    expect_match(construct_scores()$error, "which is not fit", fixed = TRUE)
    expect_error(crit_df(), "which is not fit")
    note <- od_text(output$crit_note)
    expect_match(note, "Not offered as a criterion: ", fixed = TRUE)
    expect_match(note, "notes_text", fixed = TRUE)
    expect_match(note, "The construct scores are unavailable.", fixed = TRUE)
    expect_match(note, "Use mean scores instead", fixed = TRUE)
    expect_match(od_text(output$sem_note), "Fit the structural model (", fixed = TRUE)
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))

    # The measurement model: one fit; the tables fill; Bartlett scores come
    # from it; a label edit keeps everything.
    session$setInputs(fit_go = 1)
    expect_identical(seen$runs, 1L)
    expect_identical(cfa_run()$status$code, "ok")
    expect_identical(construct_scores()$method, "Bartlett factor score")
    expect_match(od_text(output$phi_tbl), "Construct 1", fixed = TRUE)
    expect_identical(nrow(ave_df()), 3L)
    expect_match(od_text(output$ave_tbl), "Fornell and Larcker (1981) compare", fixed = TRUE)
    after <- od_fits(seen)
    od_label(state, session, 1L)
    invisible(output$phi_tbl)
    invisible(output$ave_tbl)
    invisible(output$crit_tbl)
    expect_identical(od_fits(seen), after)
    expect_identical(demand$status()$state, "current")

    # The structural model with one criterion (the two columns hold the same
    # numbers, so both together would make the model singular): one SIEVER
    # fit, kept under the selection; another selection waits; the first
    # returns.
    session$setInputs(criteria = "age_text")
    session$setInputs(sem_go = 1)
    expect_identical(seen$runs, 2L)
    expect_identical(sem_run()$status$code, "ok")
    expect_match(od_text(output$sem_tbl), "Criterion", fixed = TRUE)
    expect_match(od_text(output$sem_note), "Structural model: ML, 9 items, 1 criterion at", fixed = TRUE)
    session$setInputs(criteria = c("age_years", "age_text"))
    expect_identical(demand_sem$status()$state, "not_run")
    expect_match(od_text(output$sem_note), "The structural model is not fit yet.", fixed = TRUE)
    session$setInputs(criteria = "age_text")
    expect_identical(demand_sem$status()$state, "current")
    expect_identical(seen$runs, 2L)

    # The method-factor model: one fit (the substantive model is current and
    # reused), the comparison kept with it.
    session$setInputs(ulmf_go = 1)
    expect_identical(seen$runs, 3L)
    r <- demand_ulmf$result()
    expect_identical(r$method$spec$kind, "method_factor")
    expect_identical(r$substantive$spec$key, cfa_run()$spec$key)
    expect_true(r$comparison$ok)
    expect_false(is.null(r$comparison$value$table))
    expect_match(od_text(output$ulmf_tbl), "Statistic", fixed = TRUE)
    after <- od_fits(seen)
    od_label(state, session, 2L)
    invisible(output$ulmf_tbl)
    expect_identical(od_fits(seen), after)

    # A reverse toggle marks the three results stale with the same sentence.
    it <- shiny::isolate(state$meta$item[3])
    od_reverse(state, session, it)
    for (d in list(demand, demand_sem, demand_ulmf)) {
      expect_identical(d$status()$state, "stale")
      expect_identical(d$status()$sentence, sprintf("%s reverse scored", it))
    }
    expect_match(od_text(output$gate), sprintf("What changed: %s reverse scored", it), fixed = TRUE)
    expect_identical(seen$runs, after[["runs"]])
  })
})

# ---- SEM tab ------------------------------------------------------------------

test_that("SEM tab: outcomes stored as numerals in text are offered, a non-numeric one is refused by name, and the model (with its effects) waits for the button", {
  seen <- od_count()
  st <- od_nine()
  shiny::isolate({
    raw <- st$raw
    raw$age_text <- as.character(raw$age_years)
    raw$notes_text <- as.character(raw$age_years)
    raw$notes_text[1:3] <- "n/a"
    st$raw <- raw
  })
  sieve_apply_roles(st, c(age_years = "criterion", age_text = "criterion", notes_text = "criterion"))
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(layout = "tree2", min_path = 0)
    expect_setequal(aux_numeric(), c("age_years", "age_text"))
    expect_match(aux_refused()[["notes_text"]],
      "Criterion 'notes_text' holds 3 values that are not numbers",
      fixed = TRUE
    )
    gate <- od_text(output$gate)
    expect_match(gate, "Not offered as an outcome: Criterion 'notes_text'", fixed = TRUE)
    expect_match(gate, "Select at least one outcome", fixed = TRUE)
    # A mediator (Textual on both sides) and an observed outcome from text.
    session$setInputs(outcomes = c("Textual", "age_text"), predictors = c("Visual", "Textual"))
    for (id in c("gate", "vb_rmsea", "paths_tbl", "r2_tbl", "effects_note", "effects_tbl")) {
      invisible(tryCatch(output[[id]], error = function(e) NULL))
    }
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))
    gate <- od_text(output$gate)
    expect_match(gate, "The structural model is not fit yet.", fixed = TRUE)
    expect_match(gate, "under ML (with labeled paths for the indirect and total effects)", fixed = TRUE)
    expect_match(gate, "Fit the structural model (", fixed = TRUE)
    expect_error(output$paths_tbl, class = "shiny.silent.error")
    expect_match(od_text(output$effects_note), "not fit yet", fixed = TRUE)
    # Run: one SIEVER fit, the model with labeled paths that define the
    # effects, and one notification.
    session$setInputs(fit_go = 1)
    expect_identical(seen$runs, 1L)
    expect_identical(path_run()$status$code, "ok")
    expect_identical(med_run()$status$code, "ok")
    expect_true(length(med_run()$spec$mediators) > 0L)
    expect_match(seen$notes[length(seen$notes)], "^Structural model: ML, 6 items, 1 external outcome finished in")
    expect_no_error(output$paths_tbl)
    expect_no_error(output$effects_tbl)
    expect_match(od_text(output$r2_tbl), "R-squared", fixed = TRUE)
    expect_identical(fm(), path_run()$measures)
    after <- od_fits(seen)
    od_label(state, session, 1L)
    invisible(output$paths_tbl)
    invisible(output$effects_tbl)
    expect_identical(od_fits(seen), after)
    expect_identical(demand$status()$state, "current")
    session$setInputs(fit_go = 2)
    expect_identical(od_fits(seen), after)
    expect_match(seen$notes[length(seen$notes)], "^Nothing analytic has changed since")
    it <- shiny::isolate(state$meta$item[4])
    od_reverse(state, session, it)
    expect_identical(demand$status()$state, "stale")
    expect_identical(demand$status()$sentence, sprintf("%s reverse scored", it))
    expect_match(od_text(output$gate), sprintf("What changed: %s reverse scored", it), fixed = TRUE)
    expect_s4_class(fit(), "lavaan")
    expect_identical(seen$runs, after[["runs"]])
    # The engine refusing the model is announced below the line.
    session$setInputs(predictors = c("Visual", "Textual", "Speed"), outcomes = "Visual")
    expect_identical(demand$status()$state, "not_run")
  })
})

# ---- IRT tab --------------------------------------------------------------------

test_that("IRT tab: nothing on view, Run fits once, the metric is a display choice, a scope fit earlier returns without a refit", {
  seen <- od_count()
  grms <- 0L
  real_grm <- DMAR::irt_grm
  testthat::local_mocked_bindings(irt_grm = function(...) {
    grms <<- grms + 1L
    real_grm(...)
  }, .package = "DMAR")
  st <- od_nine(ordered = TRUE)
  shiny::testServer(mod_irt_server, args = list(state = st), {
    session$setInputs(
      scope = "__all__", metric = "normal_ogive", estimator = "WLSMV",
      theta_min = -4, theta_max = 4, theta_n = 81, info_threshold = 10
    )
    for (id in c("gate", "model_note", "param_tbl", "fit_tbl", "test_info_note", "coverage_tbl", "peak_tbl")) {
      invisible(tryCatch(output[[id]], error = function(e) NULL))
    }
    expect_identical(grms, 0L)
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))
    expect_null(grm_fit())
    gate <- od_text(output$gate)
    expect_match(gate, "The graded response model is not fit yet.", fixed = TRUE)
    expect_match(gate,
      "Fitting the graded response model of the 9 items in all active items under WLSMV is estimated to take",
      fixed = TRUE
    )
    expect_match(gate, "Fit the graded response model (", fixed = TRUE)
    expect_match(od_text(output$param_tbl), "Note", fixed = TRUE)
    expect_null(param_table())
    session$setInputs(fit_go = 1)
    expect_identical(grms, 1L)
    g <- grm_fit()
    expect_null(g$error)
    expect_identical(attr(g$grm, "estimator"), "WLSMV")
    expect_match(
      seen$notes[length(seen$notes)],
      "^Graded response model: WLSMV, 9 items \\(all active items\\) finished in"
    )
    expect_match(od_text(output$gate), "Graded response model: WLSMV, 9 items (all active items) at", fixed = TRUE)
    expect_no_error(output$param_tbl)
    session$setInputs(metric = "logistic")
    expect_identical(grms, 1L)
    expect_identical(param_table()$metric, "logistic")
    od_label(state, session, 1L)
    invisible(output$param_tbl)
    expect_identical(grms, 1L)
    expect_identical(demand$status()$state, "current")
    session$setInputs(fit_go = 2)
    expect_identical(grms, 1L)
    expect_match(seen$notes[length(seen$notes)], "^Nothing analytic has changed since")
    # Another scope waits; back to the first, it is shown without a refit.
    session$setInputs(scope = "Visual")
    expect_identical(demand$status()$state, "not_run")
    expect_null(grm_fit())
    session$setInputs(fit_go = 3)
    expect_identical(grms, 2L)
    expect_length(grm_fit()$items, 3L)
    session$setInputs(scope = "__all__")
    expect_identical(demand$status()$state, "current")
    expect_length(grm_fit()$items, 9L)
    expect_identical(grms, 2L)
    # An exclusion refits the requested model and keeps the parameters visible.
    it <- shiny::isolate(state$meta$item[9])
    od_exclude(state, session, it)
    expect_identical(demand$status()$state, "current")
    expect_null(demand$status()$sentence)
    expect_length(grm_fit()$items, 8L)
    expect_no_error(output$param_tbl)
    expect_identical(grms, 3L)
  })
  # Continuous items: the gate refuses before any button, and the button
  # does nothing.
  st <- od_nine()
  shiny::testServer(mod_irt_server, args = list(state = st), {
    session$setInputs(scope = "__all__", metric = "normal_ogive", estimator = "WLSMV")
    expect_match(od_text(output$gate), "requires ordered categorical responses", fixed = TRUE)
    session$setInputs(fit_go = 1)
    expect_identical(grms, 3L)
    expect_null(grm_fit())
  })
})

test_that("IRT tab, the private 68-item study: nothing on view, and the estimate names the construct", {
  seen <- od_count()
  st <- od_private_study()
  shiny::testServer(mod_irt_server, args = list(state = st), {
    session$setInputs(
      scope = "Ethics", metric = "normal_ogive", estimator = "WLSMV",
      theta_min = -4, theta_max = 4, theta_n = 81, info_threshold = 10
    )
    started <- proc.time()[["elapsed"]]
    state$confirmed <- TRUE
    session$flushReact()
    for (id in c("gate", "model_note", "param_tbl", "fit_tbl", "test_info_note", "coverage_tbl", "peak_tbl")) {
      invisible(tryCatch(output[[id]], error = function(e) NULL))
    }
    seconds <- proc.time()[["elapsed"]] - started
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))
    expect_null(grm_fit())
    expect_match(od_text(output$gate),
      "Fitting the graded response model of the 68 items in Ethics under WLSMV is estimated to take",
      fixed = TRUE
    )
    cat(sprintf("\n[68-item IRT] Confirm and every output: %.2f s, no fit\n", seconds))
  })
})

# ---- Bifactor tab ----------------------------------------------------------------

test_that("Bifactor tab: the button states the estimate, Fit again reuses, the result stays on screen under the stale line, and the bootstrap refuses it", {
  seen <- od_count()
  st <- od_nine()
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    session$setInputs(idx_B = 40, idx_seed = 7)
    status <- od_text(output$status)
    expect_match(status, "The bifactor model is not fit yet.", fixed = TRUE)
    expect_match(status, "Fit bifactor model (", fixed = TRUE)
    # One element on the page carries the button id: the status line's button,
    # with the estimate. A static copy in the sidebar under the same id would
    # make a click by id in the browser find two elements ("Multiple HTML
    # elements found with selector #bifactor-fit").
    html <- output$status$html
    expect_identical(lengths(regmatches(html, gregexpr(sprintf('id="%s"', session$ns("fit")), html, fixed = TRUE))), 1L)
    expect_false(grepl('id="bifactor-fit"', as.character(mod_bifactor_ui("bifactor")), fixed = TRUE))
    expect_null(res())
    expect_identical(od_fits(seen), c(lavaan = 0L, runs = 0L))
    suppressWarnings(session$setInputs(fit = 1))
    expect_identical(seen$runs, 1L) # the model; the total score omega is DMAR's own fit
    expect_gt(seen$lavaan, 0L)
    r <- res()
    expect_s4_class(r$fit, "lavaan")
    expect_false(is.null(r$measures))
    expect_false(is.null(r$total_omega))
    expect_match(seen$notes[length(seen$notes)], "^Bifactor model: ML, 9 items finished in")
    expect_match(od_text(output$essential), "Omega of the raw total score", fixed = TRUE)
    after <- od_fits(seen)
    od_label(state, session, 1L)
    invisible(output$idx_tbl)
    invisible(output$essential)
    expect_identical(od_fits(seen), after)
    suppressWarnings(session$setInputs(fit = 2))
    expect_identical(od_fits(seen), after)
    expect_match(seen$notes[length(seen$notes)], "^Nothing analytic has changed since")
    # A reversal, not an exclusion, makes the fit stale here: excluding one
    # of the nine leaves a construct of two items and two group factors,
    # which the bifactor model refuses.
    it <- shiny::isolate(state$meta$item[9])
    od_reverse(state, session, it)
    expect_identical(demand$status()$state, "stale")
    expect_identical(shown()$stale, sprintf("%s reverse scored", it))
    status <- od_text(output$status)
    # The stale line carries the page's one Update.
    expect_match(status, sprintf("What changed: %s reverse scored. Shown below as computed (Bifactor model", it), fixed = TRUE)
    expect_match(status, "Update (", fixed = TRUE)
    expect_no_error(output$idx_tbl)
    expect_no_error(output$load_tbl)
    session$setInputs(idx_boot = 1)
    expect_false(boot_running())
    expect_match(idx_ci()$error,
      sprintf(
        "changed after the model shown here was fitted (%s reverse scored)",
        it
      ),
      fixed = TRUE
    )
    expect_identical(od_fits(seen), after)
    suppressWarnings(session$setInputs(fit = 3))
    expect_identical(demand$status()$state, "current")
    expect_identical(seen$runs, 2L)
  })
})

# ---- the line describes the result it holds ---------------------------------

test_that("the stale line and the reuse notification describe the result they hold, not the plan the inputs make now", {
  seen <- od_count()
  st <- od_nine()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    od_cfa_inputs(session)
    session$setInputs(fit_go = 1)
    expect_identical(demand$status()$state, "current")
    expect_identical(demand$status()$progress, "CFA: ML, 9 items")
    # A respondent exclusion leaves the nine-item model from before.
    sieve_set_case_status(state, 1L, "excluded", "Currency test")
    session$flushReact()
    gate <- od_text(output$gate)
    expect_match(gate, "The result below is from before (CFA: ML, 9 items at", fixed = TRUE)
    expect_false(grepl("8 items at", gate, fixed = TRUE))
    # The tab's one Update.
    expect_match(gate, "Update (", fixed = TRUE)
    # A different dataset in the same session: the 24 tests replace the
    # nine, as a load does. A result of other data is not an earlier
    # version of this one: it is withheld, status not run and no fit, and
    # the line says where it is kept, naming the data and the fit it was,
    # rather than showing the nine-item fit below it as from before.
    fx <- sieve_hs_fixture()
    state$dataset_name <- fx$name
    state$raw <- fx$data
    state$items <- fx$data[, fx$item_cols, drop = FALSE]
    state$roles <- sieve_declared_roles(fx$data, fx$item_cols)
    state$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
    state$theory <- list(k = fx$theory_k, label = fx$theory_lbl, map = fx$theory_map)
    sieve_new_dataset(state)
    sieve_confirm_types(state)
    session$flushReact()
    expect_identical(demand$status()$state, "not_run")
    expect_true(demand$status()$withheld)
    expect_match(demand$status()$sentence,
      "The result of the data loaded before (Holzinger and Swineford, primary 9 tests (n = 301, from DMAR): CFA: ML, 9 items at",
      fixed = TRUE
    )
    expect_match(demand$status()$sentence, "is kept in the analysis registry and the saved-results report, not shown for these data.", fixed = TRUE)
    gate <- od_text(output$gate)
    expect_match(gate, "The model is not fit yet.", fixed = TRUE)
    expect_match(gate, "is kept in the analysis registry and the saved-results report", fixed = TRUE)
    expect_false(grepl("24 items at", gate, fixed = TRUE))
    expect_false(grepl("from before", gate, fixed = TRUE))
    expect_null(demand$result())
    expect_null(fit())
    expect_error(output$fit_tbl, class = "shiny.silent.error")
    # The withheld nine-item result is still held (it comes back with its data).
    expect_length(demand$store(), 1L)
    # Refit on the 24: current, and named as such.
    session$setInputs(fit_go = 2)
    expect_identical(demand$status()$state, "current")
    expect_identical(demand$status()$progress, "CFA: ML, 24 items")
    expect_length(setdiff(lavaan::lavNames(fit(), "ov"), lavaan::lavInspect(fit(), "options")$aux), 24L)
    # The current fit is stated in the method line above the tabs, which
    # names the 24 items it holds.
    expect_match(od_text(output$method_line), "CFA, 24 items on .* Fit at [0-9]{2}:[0-9]{2} in")
    session$setInputs(fit_go = 3)
    expect_match(seen$notes[length(seen$notes)], "^Nothing analytic has changed since CFA: ML, 24 items ran at")
  })
})

test_that("the first estimate of each kind is of the order measured on the 68-item study, relative to the rate of one CFA", {
  local_measured_rates()
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  # One WLSMV CFA of 68 items measured at 110 seconds (a profile of the
  # private study of 500 respondents and 68 ordered items) sets the CFA rate.
  sieve_rate_note("cfa_WLSMV", 110, sieve_pairs(68))
  expect_equal(sieve_demand_wait("cfa", 68, "WLSMV"), 110, tolerance = 1e-12)
  # Measured in the same runs (in parentheses, a second profile against a
  # CFA of 91 seconds): reliability 133 s (102 s), the graded response
  # model 158 s (133 s), the bifactor model with the omega of
  # the raw total score 121 s (141 s), the factor scores 30 s (33 s), the
  # method-factor model 29 s (20 s). The first estimate of each is within a
  # factor of 1.5 of the measured time.
  within <- function(estimate, measured, what) {
    expect_gt(estimate, measured / 1.5, label = sprintf("%s estimate %.0f s", what, estimate))
    expect_lt(estimate, measured * 1.5, label = sprintf("%s estimate %.0f s", what, estimate))
  }
  within(sieve_demand_wait("reliability", 68, "WLSMV"), 133, "reliability")
  within(sieve_demand_wait("irt", 68, "WLSMV"), 158, "graded response model")
  within(
    sieve_demand_wait(
      "bifactor",
      68,
      "WLSMV"
    ) + sieve_demand_wait(
      "total_omega",
      68,
      "WLSMV"
    ),
    121,
    "bifactor with the total omega"
  )
  within(sieve_demand_wait("scores", 68, "WLSMV"), 30, "factor scores")
  within(sieve_demand_wait("method_factor", 68, "WLSMV"), 29, "method factor")
  # The kind's own measured rate replaces the scaled estimate.
  sieve_demand_note("reliability", "WLSMV", 200, 68)
  expect_equal(sieve_demand_wait("reliability", 68, "WLSMV"), 200, tolerance = 1e-12)
})

test_that("an error in the plan reaches the line instead of hiding behind 'not fit yet'", {
  seen <- od_count()
  # The plan is resolved from the modeled subset. Inject the failure at
  # the duration service used only by the plan, preserving the same
  # assertion that an estimate failure reaches the actionable gate.
  testthat::local_mocked_bindings(sieve_demand_wait = function(...) stop("the specification stopped for the test"))
  st <- od_nine()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    od_cfa_inputs(session)
    gate <- od_text(output$gate)
    expect_match(gate, "The estimated time could not be computed: the specification stopped for the test", fixed = TRUE)
    expect_match(gate, "The model is not fit yet.", fixed = TRUE)
    expect_match(as.character(output$gate$html), '-fit_go"', fixed = TRUE)
  })
})

# ---- the line of a computation running in the background -------------------

test_that("a background run's line is drawn once with its Cancel; each second redraws only the time so far, and Cancel still cancels", {
  # line() takes no timer: a page output that held it, redrawn every second
  # with its Cancel button, would drop keyboard focus on Cancel to the page
  # body, and a press whose release came after a redraw would reach no
  # button. The line is drawn once per run; the time so far is the wire's
  # own text output, <id>_progress. The clock
  # is the test's, so each tick is seen in the text.
  test_frame <- environment()
  clock <- new.env(parent = emptyenv())
  clock$seconds <- 0
  notes <- new.env(parent = emptyenv())
  notes$said <- character(0)
  testthat::local_mocked_bindings(
    sieve_demand_elapsed = function(started) clock$seconds,
    sieve_notify = function(message, ...) {
      notes$said <- c(notes$said, message)
      invisible(NULL)
    },
    .env = test_frame
  )
  st <- od_nine()
  handles <- new.env(parent = emptyenv())
  handles$made <- 0L
  handles$killed <- 0L
  drawn <- new.env(parent = emptyenv())
  drawn$n <- 0L
  shiny::testServer(function(input, output, session) {
    wire <- sieve_demand_wire(input, session, st, "run", "Slow analysis",
      key = shiny::reactive(sieve_demand_key(st, items = st$meta$item[st$meta$status == "active"])),
      plan = shiny::reactive(list(
        seconds = 3, estimator = "MLR", p = 9,
        progress = "Slow analysis: 9 items", label = "3 seconds"
      )),
      compute = function(...) stop("not used"),
      background = list(
        start = function(k, p) {
          handles$made <- handles$made + 1L
          h <- new.env(parent = emptyenv())
          h$alive <- TRUE
          h$is_alive <- function() h$alive
          h$kill <- function() {
            handles$killed <- handles$killed + 1L
            h$alive <- FALSE
          }
          h$get_result <- function() list(estimate = 0.7)
          handles$last <- h
          h
        },
        finish = function(k, p, value, handle, current) value,
        in_process = function() FALSE
      )
    )
    # The page's output that holds the line, counting its draws.
    output$line <- shiny::renderUI({
      drawn$n <- drawn$n + 1L
      wire$line()
    })
  }, {
    count <- function(html, pattern) lengths(regmatches(html, gregexpr(pattern, html, fixed = TRUE)))
    progress_id <- session$ns("run_progress")
    cancel_id <- session$ns("run_cancel")
    output$line
    expect_identical(output$run_progress, "")

    # ---- A run for the inputs now -------------------------------------------
    session$setInputs(run = 1L)
    expect_identical(wire$status()$state, "running")
    html <- as.character(output$line$html)
    expect_match(od_text(output$line), "Running The model is being fit in a separate R process.", fixed = TRUE)
    expect_match(od_text(output$line), "Slow analysis: 9 items, started at", fixed = TRUE)
    expect_match(od_text(output$line), "This page and the others stay usable meanwhile", fixed = TRUE)
    # One Cancel, and the time so far in its own text output with role
    # timer, whose live region is off; the time is not in the line itself.
    expect_identical(count(html, sprintf('id="%s"', cancel_id)), 1L)
    expect_identical(count(html, sprintf('id="%s"', progress_id)), 1L)
    expect_match(html, sprintf('<span id="%s" class="shiny-text-output" role="timer" aria-live="off"></span>', progress_id),
      fixed = TRUE)
    expect_false(grepl("so far", html, fixed = TRUE))
    expect_identical(output$run_progress, "0.0 seconds so far.")
    drawn_at_start <- drawn$n
    # Each second redraws the time so far, never the line with its Cancel.
    for (s in 1:3) {
      clock$seconds <- s
      session$elapse(1000)
      expect_identical(output$run_progress, sprintf("%.1f seconds so far.", s))
      expect_identical(as.character(output$line$html), html)
      expect_identical(drawn$n, drawn_at_start)
    }
    # Past its estimate (3 seconds), the text says so; still no redraw.
    clock$seconds <- 4
    session$elapse(1000)
    expect_identical(output$run_progress,
      "4.0 seconds so far. It has run past its estimate and is still running.")
    expect_identical(drawn$n, drawn_at_start)
    # Cancel, pressed on that same button, still cancels: the process is
    # stopped, nothing is kept, the line is drawn once more (Stopped), and
    # the time so far is empty and no longer redrawn.
    session$setInputs(run_cancel = 1L)
    expect_identical(handles$killed, 1L)
    expect_null(wire$job())
    expect_identical(wire$status()$state, "not_run")
    expect_identical(length(wire$store()), 0L)
    expect_true(any(grepl("Slow analysis: 9 items was canceled after", notes$said, fixed = TRUE)))
    expect_match(od_text(output$line), "Stopped Slow analysis: 9 items was canceled at", fixed = TRUE)
    expect_identical(drawn$n, drawn_at_start + 1L)
    expect_false(grepl(cancel_id, as.character(output$line$html), fixed = TRUE))
    expect_identical(output$run_progress, "")
    clock$seconds <- 9
    session$elapse(3000)
    expect_identical(output$run_progress, "")
    expect_identical(drawn$n, drawn_at_start + 1L)

    # ---- A run still going for the inputs before a change -------------------
    clock$seconds <- 0
    session$setInputs(run = 2L)
    expect_identical(handles$made, 2L)
    od_exclude(st, session, shiny::isolate(st$meta)$item[9L])
    s <- wire$status()
    expect_identical(s$state, "not_run")
    expect_false(is.null(s$elsewhere))
    html <- as.character(output$line$html)
    expect_match(od_text(output$line),
      "Slow analysis: 9 items for the inputs before the change is still running in a separate R process, started at",
      fixed = TRUE)
    expect_match(od_text(output$line), "Its result will be kept as from before.", fixed = TRUE)
    expect_identical(count(html, sprintf('id="%s"', cancel_id)), 1L)
    expect_identical(count(html, sprintf('id="%s"', progress_id)), 1L)
    drawn_elsewhere <- drawn$n
    # Its time so far is redrawn each second, past the estimate too (the
    # estimate was for the other inputs); the line is not.
    for (s in c(2, 5)) {
      clock$seconds <- s
      session$elapse(1000)
      expect_identical(output$run_progress, sprintf("%.1f seconds so far.", s))
      expect_identical(as.character(output$line$html), html)
      expect_identical(drawn$n, drawn_elsewhere)
    }
    session$setInputs(run_cancel = 2L)
    expect_identical(handles$killed, 2L)
    expect_null(wire$status()$elsewhere)
    expect_false(grepl(cancel_id, as.character(output$line$html), fixed = TRUE))
    expect_identical(output$run_progress, "")
  })
})
