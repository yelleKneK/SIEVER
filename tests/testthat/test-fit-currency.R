# Currency on the effective inputs and exact-identity fit reuse.
#
# A key that hashed the whole item data frame and the whole ledger (every
# row's construct, facet, type, status and keying) would mark a first-order
# CFA stale on a facet reassignment, or the reversal of an excluded item,
# although its lavaan syntax and modeled columns are unchanged; and a copy
# of the correlated-factors model fit by each page would run the same
# estimation on the CFA page, the Items page, the Validity page and behind
# the Reliability page's factor scores. Tested here: sieve_demand_key() over the items a model reads (and facets only
# when the model reads them); the CFA page staying current through a facet
# edit and the keying of an excluded item and going stale on every genuine
# change (inclusion, type, keying, construct, values of a modeled column,
# the respondents); the higher-order and the direct facet CFA going stale
# on a facet edit; the session fit store returning the identical fitted
# object for the identical identity and fitting again for any difference;
# Validity's measurement model and the scores model behind Reliability
# reusing the CFA page's fit with zero engine calls and saying so; a new
# dataset emptying the store; and a project never carrying it.
#
# Engine calls are counted as test-ondemand.R counts them: lavaan's own
# lavaan() in its namespace and SIEVER's sieve_run_model(), through which
# every SIEVER fit goes.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

fc_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
}

# The bundled nine tests, confirmed, with the variable roles declared.
fc_nine <- function(confirmed = TRUE) {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = confirmed, labels = TRUE,
    theory = TRUE, name = fx$name, configure = function(s) {
  s$roles <- sieve_declared_roles(fx$data, fx$item_cols)
  s$roles$role[s$roles$role == "auxiliary"] <- "criterion"
  s$estimator <- "MLR"

  })
  s
}

# The full 24 tests with facets on two domains: Memory (t14 to t19) in
# three facets of two items, Reasoning (t20 to t24) in two facets; the
# other three domains have no facets. A higher-order model then holds two
# second-order constructs, and a direct facet model five facet factors.
fc_facets <- function() {
  fx <- sieve_hs_fixture()
  siever_test_study(fx, configure = function(s) {
    m <- s$meta
    memory <- fx$theory_map$Memory
    reasoning <- fx$theory_map$Reasoning
    m$facet[match(memory[1:2], m$item)] <- "M1"
    m$facet[match(memory[3:4], m$item)] <- "M2"
    m$facet[match(memory[5:6], m$item)] <- "M3"
    m$facet[match(reasoning[1:2], m$item)] <- "R1"
    m$facet[match(reasoning[3:5], m$item)] <- "R2"
    s$meta <- m
  })
}

# Count every lavaan model fit and every SIEVER fit made while a test runs.
fc_count <- function(env = parent.frame()) {
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
fc_fits <- function(seen) c(lavaan = seen$lavaan, runs = seen$runs)

# A ledger edit made from outside a reactive context, with the flush the
# application makes after it.
fc_edit <- function(state, session, column, items, value) {
  m <- shiny::isolate(state$meta)
  m[[column]][m$item %in% items] <- value
  state$meta <- m
  sieve_confirm_types(state)
  session$flushReact()
}
fc_cfa_inputs <- function(session, ...) {
  session$setInputs(
    structure = "first", ident = "varone", layout = "tree2",
    what = "std", min_path = 0, resid = TRUE, covs = TRUE, ...
  )
}
fc_reliab_inputs <- function(session, ...) {
  defaults <- list(
    denominator = "observed", ci_method = "recommended", missing = "listwise",
    conf_level = 0.95, B = 1000, boot_seed = NA, show_alpha = TRUE,
    score_method = "Bartlett"
  )
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}

# ---- the key over the effective inputs ---------------------------------------

test_that("sieve_demand_key covers the named items and their rows only, and the facet column only when asked", {
  s <- fc_nine()
  shiny::isolate({
    all_items <- s$meta$item
    modeled <- all_items[-9]
    whole <- sieve_demand_key(s, list(a = 1))
    k <- sieve_demand_key(s, list(a = 1), items = modeled, facets = FALSE)
    kf <- sieve_demand_key(s, list(a = 1), items = modeled, facets = TRUE)
    expect_identical(names(k), c("dataset", "data", "meta", "rows", "choices"))
    expect_identical(k$dataset, whole$dataset)
    expect_identical(k$rows, whole$rows)
    expect_false(identical(k$data, whole$data))
    expect_false(identical(k$meta, whole$meta))
    # Without the items argument the key covers the whole frame and ledger;
    # with every item and facets, it is the same key.
    expect_identical(sieve_demand_key(s, list(a = 1), items = all_items, facets = TRUE), whole)
    # A facet edit changes the key only when facets are read.
    m <- s$meta
    m$facet[1] <- "A facet"
    s$meta <- m
    expect_identical(sieve_demand_key(s, list(a = 1), items = modeled, facets = FALSE), k)
    expect_false(identical(sieve_demand_key(s, list(a = 1), items = modeled, facets = TRUE), kf))
    m$facet[1] <- NA_character_
    s$meta <- m
    # Item 9 is outside the model: its keying, its type, its construct and
    # its values change nothing; an item inside it changes everything.
    same <- function() identical(sieve_demand_key(s, list(a = 1), items = modeled, facets = FALSE), k)
    m <- s$meta
    m$reverse[9] <- TRUE
    m$type[9] <- "ordered"
    m$construct[9] <- "Visual"
    m$status[9] <- "parked"
    s$meta <- m
    expect_true(same())
    d <- s$items
    d[[9]][1] <- d[[9]][1] + 1
    s$items <- d
    expect_true(same())
    for (change in list(
      function(m) { m$reverse[1] <- TRUE; m },
      function(m) { m$type[2] <- "ordered"; m },
      function(m) { m$construct[3] <- "Speed"; m },
      function(m) { m$status[4] <- "parked"; m },
      function(m) { m$item[5] <- "renamed"; m }
    )) {
      held <- s$meta
      s$meta <- change(held)
      expect_false(same())
      s$meta <- held
      expect_true(same())
    }
    # A label edit is not analytic; a value of a modeled column and the
    # respondent rows are.
    m <- s$meta
    m$label[1] <- "Another label"
    s$meta <- m
    expect_true(same())
    d <- s$items
    d[[1]][1] <- d[[1]][1] + 1
    s$items <- d
    expect_false(same())
    d[[1]][1] <- d[[1]][1] - 1
    s$items <- d
    expect_true(same())
    s$case_index <- c(2:301, 1L)
    expect_false(same())
    s$case_index <- NULL
    expect_true(same())
    # The choices stay part of the key, and an empty item set is a key over
    # nothing, not the whole frame.
    expect_false(identical(sieve_demand_key(s, list(a = 2), items = modeled, facets = FALSE), k))
    none <- sieve_demand_key(s, list(a = 1), items = character(0), facets = FALSE)
    expect_false(identical(none$data, whole$data))
    expect_identical(none, sieve_demand_key(s, list(a = 1), items = character(0), facets = FALSE))
  })
})

# ---- the CFA page: current through what the model does not read -------------

test_that("CFA page: a facet edit and the keying of an excluded item leave a first-order fit current; every change to a modeled item or the respondents marks it stale", {
  seen <- fc_count()
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  st <- fc_nine()
  ninth <- shiny::isolate(st$meta$item[9])
  shiny::isolate({
    m <- st$meta
    m$status[9] <- "parked"
    st$meta <- m
  })
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    fc_cfa_inputs(session)
    session$setInputs(fit_go = 1)
    expect_identical(seen$runs, 1L)
    expect_identical(demand$status()$state, "current")
    expect_length(lavaan::lavNames(fit(), "ov"), 8L)
    after_fit <- fc_fits(seen)
    # A facet reassignment of a modeled item: the first-order model reads no
    # facet, so the fit is current and nothing is computed.
    first <- shiny::isolate(state$meta$item[1])
    fc_edit(state, session, "facet", first, "Perception")
    invisible(output$gate)
    invisible(output$method_line)
    expect_identical(demand$status()$state, "current")
    expect_identical(fc_fits(seen), after_fit)
    fc_edit(state, session, "facet", first, NA_character_)
    expect_identical(demand$status()$state, "current")
    # The excluded item reverse scored: its values and keying change, the
    # model reads neither.
    out <- sieve_apply_reverse(state, ninth, origin = "test")
    sieve_confirm_types(state)
    session$flushReact()
    expect_length(out$applied, 1L)
    expect_true(shiny::isolate(state$meta$reverse[9]))
    expect_identical(demand$status()$state, "current")
    expect_identical(seen$runs, 1L)
    # Its inclusion is a change of the model: stale, with the item named.
    fc_edit(state, session, "status", ninth, "active")
    expect_identical(demand$status()$state, "current")
    session$setInputs(fit_go = 2)
    expect_identical(seen$runs, 2L)
    expect_identical(demand$status()$state, "current")
    expect_length(lavaan::lavNames(fit(), "ov"), 9L)
    # A type change, a construct move, each undone: stale, then current again.
    second <- shiny::isolate(state$meta$item[2])
    fc_edit(state, session, "type", second, "ordered")
    expect_identical(demand$status()$state, "stale")
    expect_match(demand$status()$sentence, sprintf("%s retyped as ordered", second), fixed = TRUE)
    fc_edit(state, session, "type", second, "continuous")
    expect_identical(demand$status()$state, "current")
    third <- shiny::isolate(state$meta$item[3])
    fc_edit(state, session, "construct", third, "Speed")
    expect_identical(demand$status()$state, "stale")
    expect_match(demand$status()$sentence, sprintf("%s moved to construct Speed", third), fixed = TRUE)
    fc_edit(state, session, "construct", third, "Visual")
    expect_identical(demand$status()$state, "current")
    # A modeled item reverse scored: stale.
    sieve_apply_reverse(state, first, origin = "test")
    sieve_confirm_types(state)
    session$flushReact()
    expect_identical(demand$status()$state, "stale")
    expect_identical(demand$status()$sentence, sprintf("%s reverse scored", first))
    session$setInputs(fit_go = 3)
    expect_identical(seen$runs, 3L)
    expect_identical(demand$status()$state, "current")
    # A respondent removed from the analysis: stale, named as such.
    removed <- sieve_remove_cases(state, rows = 5L, reason = "a test")
    session$flushReact()
    expect_identical(removed$removed, 5L)
    expect_identical(demand$status()$state, "stale")
    expect_match(demand$status()$sentence, "1 respondent removed from the analysis (row 5 of the data as loaded)", fixed = TRUE)
    expect_identical(seen$runs, 3L)
  })
})

test_that("CFA page: a facet edit marks a higher-order and a direct facet CFA stale, and leaves the first-order CFA current", {
  seen <- fc_count()
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  st <- fc_facets()
  reasoning <- shiny::isolate(st$theory$map$Reasoning)
  moved <- reasoning[5]
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    fc_cfa_inputs(session)
    session$setInputs(fit_go = 1)
    expect_identical(seen$runs, 1L)
    expect_identical(demand$status()$state, "current")
    expect_identical(cfa_run()$spec$structure, "first_order")
    # The last Reasoning item moves from facet R2 to R1 (both facets keep at
    # least two items): the first-order model is unchanged.
    fc_edit(state, session, "facet", moved, "R1")
    expect_identical(demand$status()$state, "current")
    fc_edit(state, session, "facet", moved, "R2")
    expect_identical(demand$status()$state, "current")
    # The higher-order model (construct to facet to items) reads facets: the
    # same move marks it stale.
    session$setInputs(structure = "higher")
    expect_identical(demand$status()$state, "not_run")
    session$setInputs(fit_go = 2)
    expect_identical(seen$runs, 2L)
    expect_identical(demand$status()$state, "current")
    expect_identical(cfa_run()$spec$structure, "higher_order")
    # Under ML with FIML the observed information of this boundary solution
    # cannot be inverted, so the model is fit again with the expected
    # information, which the run says and the recorded R carries.
    run <- cfa_run()
    expect_identical(lavaan::lavInspect(run$fit, "options")$information[1L], "expected")
    expect_true(any(grepl("could not be inverted for this solution", run$warnings, fixed = TRUE)))
    expect_true(any(grepl('information = "expected"', sieve_cfa_code(attr(run$fit, "sieve_spec")), fixed = TRUE)))
    # Three first-order constructs, two second-order constructs and their
    # five facet factors.
    expect_length(cfa_run()$spec$latent, 10L)
    expect_true(all(vapply(
      c("M1", "M2", "M3", "R1", "R2"),
      function(f) any(grepl(f, cfa_run()$spec$name_map, fixed = TRUE)), logical(1)
    )))
    fc_edit(state, session, "facet", moved, "R1")
    expect_identical(demand$status()$state, "stale")
    expect_match(demand$status()$sentence, sprintf("%s moved to facet R1", moved), fixed = TRUE)
    fc_edit(state, session, "facet", moved, "R2")
    expect_identical(demand$status()$state, "current")
    # The direct facet model likewise.
    session$setInputs(structure = "first", scope = "facet", all_facets = TRUE)
    expect_identical(demand$status()$state, "not_run")
    session$setInputs(fit_go = 3)
    expect_identical(seen$runs, 3L)
    expect_identical(demand$status()$state, "current")
    expect_identical(cfa_run()$spec$structure, "facet_first_order")
    fc_edit(state, session, "facet", moved, "R1")
    expect_identical(demand$status()$state, "stale")
    fc_edit(state, session, "facet", moved, "R2")
    expect_identical(demand$status()$state, "current")
    expect_identical(seen$runs, 3L)
  })
})

# ---- the session fit store ---------------------------------------------------

test_that("the fit store returns the identical object for the identical identity and fits again for any difference", {
  seen <- fc_count()
  st <- fc_nine()
  items <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  store <- shiny::isolate(st$fit_store)
  expect_true(is.environment(store))
  expect_identical(store$dataset, shiny::isolate(st$dataset_id))
  expect_length(store$entries, 0L)
  spec <- sieve_cfa_spec(meta, reserved = names(items))
  rows <- shiny::isolate(sieve_case_index(st))
  first <- sieve_run_cfa(spec, items, store = store, page = "CFA", rows = rows)
  expect_s4_class(first$fit, "lavaan")
  expect_false(first$reused)
  expect_identical(first$page, "CFA")
  expect_match(first$when, "^[0-9]{2}:[0-9]{2}$")
  expect_true(is.finite(first$seconds) && first$seconds > 0)
  expect_identical(first$fit_id, sieve_fit_identity(spec, items, rows = rows, dataset = store$dataset))
  expect_identical(seen$runs, 1L)
  expect_length(store$entries, 1L)
  after_first <- fc_fits(seen)
  # The identical identity: the identical fitted object, no engine call,
  # the original page, time and seconds.
  again <- sieve_run_cfa(spec, items, store = store, page = "Validity", rows = rows)
  expect_identical(fc_fits(seen), after_first)
  expect_true(again$reused)
  expect_identical(again$reused_from, "CFA")
  expect_identical(again$reused_when, first$when)
  expect_identical(again$seconds, first$seconds)
  expect_identical(again$page, "CFA")
  expect_identical(again$fit, first$fit)
  expect_identical(lavaan::parameterEstimates(again$fit), lavaan::parameterEstimates(first$fit))
  expect_identical(sieve_reuse_text(again), sprintf("Reused the fit from CFA at %s", first$when))
  expect_length(store$entries, 1L)
  # What the fitting page read from the fit travels with it: the measures
  # (whose baseline models cost engine calls) are read once.
  first$measures <- sieve_fit_measures(first$fit)
  first$standardized <- sieve_capture(lavaan::standardizedSolution(first$fit))
  expect_gt(seen$lavaan, after_first[["lavaan"]])
  sieve_fit_store_keep(store, first, fit_name = "cfa_fit", model_name = "cfa_model")
  before_third <- fc_fits(seen)
  third <- sieve_run_cfa(spec, items, store = store, page = "Reliability", rows = rows)
  expect_identical(fc_fits(seen), before_third)
  expect_identical(third$measures, first$measures)
  expect_identical(third$standardized, first$standardized)
  expect_identical(third$measures, sieve_fit_measures(third$fit))
  expect_identical(third$fit_name, "cfa_fit")
  # The recorded script of a reuse: one note and the assignments that give
  # this page's names to the objects the first page's script defined; the
  # call itself when that page recorded no name.
  code <- sieve_reuse_code(third, fit_name = "score_fit", model_name = "score_model")
  expect_identical(code, c(
    sprintf("# Reused the fit from CFA at %s: the identical lavaan model on the identical data; nothing was fit again.", first$when),
    "score_model <- cfa_model", "score_fit <- cfa_fit"
  ))
  unnamed <- again
  unnamed$fit_name <- NULL
  code <- sieve_reuse_code(unnamed, fit_name = "validity_fit", model_name = "validity_model")
  expect_match(code[2], "repeated here so that the script stands alone", fixed = TRUE)
  expect_true(any(grepl("validity_fit <- lavaan::cfa(validity_model", code, fixed = TRUE)))
  # Another estimator, another missing data rule, another identification,
  # another value of a modeled column, another row set, another dataset:
  # each is a different identity and is fit again, and never stored over
  # the first.
  runs <- seen$runs
  different <- list(
    estimator = list(sieve_cfa_spec(meta, reserved = names(items), estimator = "mlr"), items, rows, store$dataset), # Auto is ML with FIML
    missing = list(sieve_cfa_spec(meta, reserved = names(items), estimator = "ml"), items, rows, store$dataset),
    identification = list(sieve_cfa_spec(meta, reserved = names(items), std_lv = FALSE), items, rows, store$dataset),
    orthogonal = list(sieve_cfa_spec(meta, reserved = names(items), orthogonal = TRUE), items, rows, store$dataset),
    value = list(spec, {
      d <- items
      d[[1]][1] <- d[[1]][1] + 1
      d
    }, rows, store$dataset),
    rows = list(spec, items, c(rows[-1], rows[1]), store$dataset)
  )
  for (name in names(different)) {
    case <- different[[name]]
    one <- sieve_run_cfa(case[[1]], case[[2]], store = store, page = name, rows = case[[3]])
    runs <- runs + 1L
    expect_identical(seen$runs, runs, info = name)
    expect_false(one$reused, info = name)
    expect_false(identical(one$fit_id, first$fit_id), info = name)
    two <- sieve_run_cfa(case[[1]], case[[2]], store = store, page = name, rows = case[[3]])
    expect_identical(seen$runs, runs, info = name)
    expect_true(two$reused, info = name)
    expect_identical(two$reused_from, name, info = name)
  }
  expect_identical(sieve_run_cfa(spec, items, store = store, page = "x", rows = rows)$reused_from, "CFA")
  expect_identical(seen$runs, runs)
  # The missing data rule alone (ML with FIML against ML on the complete
  # cases) is a different identity.
  expect_false(identical(
    sieve_fit_identity(sieve_cfa_spec(meta, reserved = names(items), estimator = "mlr"), items),
    sieve_fit_identity(sieve_cfa_spec(meta, reserved = names(items), estimator = "ml"), items)
  ))
  # A column the model does not read leaves the identity unchanged; the
  # engine versions are part of it.
  expect_identical(
    sieve_fit_identity(spec, items, rows = rows),
    sieve_fit_identity(spec, cbind(items, extra = seq_len(nrow(items))), rows = rows)
  )
  expect_false(identical(
    sieve_fit_identity(spec, items, rows = rows),
    sieve_fit_identity(spec, items, rows = rows, versions = c(lavaan = "0.0.1", DMAR = "0.0.1"))
  ))
  # Without a store the fit is made directly, with none of the store's
  # fields.
  plain <- sieve_run_cfa(spec, items)
  expect_null(plain$reused)
  expect_null(plain$fit_id)
  expect_identical(seen$runs, runs + 1L)
  # A new dataset empties the store in place and binds it to the new
  # identity.
  held <- length(store$entries)
  expect_gt(held, 1L)
  shiny::isolate({ sieve_new_dataset(st); sieve_confirm_types(st) })
  expect_identical(shiny::isolate(st$fit_store), store)
  expect_length(store$entries, 0L)
  expect_identical(store$dataset, shiny::isolate(st$dataset_id))
  # Through the exported wrapper: one estimation, the identical object.
  before <- seen$runs
  a <- sieve_fit_cfa(items, meta, store = store, page = "report")
  b <- sieve_fit_cfa(items, meta, store = store, page = "report")
  expect_identical(a, b)
  expect_identical(seen$runs, before + 1L)
})

test_that("Validity's measurement model and the scores model behind Reliability reuse the CFA page's fit with zero engine calls and say so; another estimator or missing rule fits again", {
  seen <- fc_count()
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  st <- fc_nine()
  keep <- new.env(parent = emptyenv())
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    fc_cfa_inputs(session)
    session$setInputs(fit_go = 1)
    expect_identical(seen$runs, 1L)
    expect_identical(demand$status()$state, "current")
    expect_false(cfa_run()$reused)
    expect_match(fc_text(output$method_line), "Fit at [0-9]{2}:[0-9]{2} in [0-9.]+ seconds, current\\.")
    keep$cfa <- cfa_run()
    keep$fits <- fc_fits(seen)
  })
  expect_length(shiny::isolate(st$fit_store)$entries, 1L)
  expect_identical(shiny::isolate(st$fit_store)$entries[[1]]$fit_name, "cfa_fit")
  log_before <- length(shiny::isolate(st$codelog))

  # Validity: the same correlated-factors model under the study's estimator.
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(score_method = "mean", htmt_on = FALSE, marker = "")
    expect_identical(demand$status()$state, "not_run")
    session$setInputs(fit_go = 1)
    expect_identical(fc_fits(seen), keep$fits)
    run <- cfa_run()
    expect_true(run$reused)
    expect_identical(run$reused_from, "CFA")
    expect_identical(run$reused_when, keep$cfa$when)
    expect_identical(run$fit, keep$cfa$fit)
    # The CFA page's measures, without the unbiased SRMR, which is shown
    # on the CFA and SEM pages only: Validity reads the same
    # measures whichever page fit the model first (sieve_fit_measures_at()).
    expect_identical(setdiff(names(keep$cfa$measures), names(run$measures)), c("usrmr", "usrmr_lo", "usrmr_hi"))
    expect_null(attr(run$measures, "families")$usrmr)
    expect_identical(run$measures, sieve_fit_drop_usrmr(keep$cfa$measures))
    expect_identical(run$standardized, keep$cfa$standardized)
    expect_identical(lavaan::parameterEstimates(run$fit), lavaan::parameterEstimates(keep$cfa$fit))
    expect_identical(demand$status()$state, "current")
    gate <- fc_text(output$gate)
    expect_match(gate, sprintf("Reused the fit from CFA at %s", keep$cfa$when), fixed = TRUE)
    expect_match(gate, "nothing was fit again", fixed = TRUE)
    expect_match(fc_text(output$phi_tbl), "Construct 1", fixed = TRUE)
    expect_identical(fc_fits(seen), keep$fits)
    # The script records the reuse as one step that aliases the CFA page's
    # objects.
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl(sprintf("Validity: reused the fit from CFA at %s", keep$cfa$when), log, fixed = TRUE)))
    expect_true(any(grepl("validity_fit <- cfa_fit", log, fixed = TRUE)))
    expect_true(any(grepl("validity_model <- cfa_model", log, fixed = TRUE)))
    # MLR with FIML (another estimator; Auto is ML with FIML) and ML on the
    # complete cases (another missing data rule):
    # each is fit, once.
    session$setInputs(estimator = "mlr")
    expect_identical(demand$status()$state, "not_run")
    session$setInputs(fit_go = 2)
    expect_identical(seen$runs, 2L)
    expect_false(cfa_run()$reused)
    expect_identical(cfa_run()$spec$estimator, "MLR")
    expect_identical(cfa_run()$spec$missing, "fiml")
    session$setInputs(estimator = "ml")
    session$setInputs(fit_go = 3)
    expect_identical(seen$runs, 3L)
    expect_false(cfa_run()$reused)
    expect_identical(cfa_run()$spec$missing, "listwise")
    # Back to the study's estimator: the kept result, still the CFA page's.
    session$setInputs(estimator = "study")
    expect_identical(demand$status()$state, "current")
    expect_true(cfa_run()$reused)
    keep$fits <- fc_fits(seen)
  })
  expect_length(shiny::isolate(st$fit_store)$entries, 3L)

  # Reliability: the measurement model behind the factor scores.
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    fc_reliab_inputs(session)
    expect_identical(demand_scores$status()$state, "not_run")
    session$setInputs(scores_go = 1)
    expect_identical(fc_fits(seen), keep$fits)
    run <- demand_scores$result()
    expect_true(run$reused)
    expect_identical(run$reused_from, "CFA")
    expect_identical(run$fit, keep$cfa$fit)
    expect_identical(score_fit(), keep$cfa$fit)
    expect_identical(lavaan::parameterEstimates(score_fit()), lavaan::parameterEstimates(keep$cfa$fit))
    expect_identical(dim(fscores()), c(301L, 3L))
    note <- fc_text(output$scores_note)
    expect_match(note, sprintf("Reused the fit from CFA at %s", keep$cfa$when), fixed = TRUE)
    expect_identical(fc_fits(seen), keep$fits)
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("score_fit <- cfa_fit", log, fixed = TRUE)))
    expect_false(any(grepl("score_fit <- lavaan::cfa(", log, fixed = TRUE)))
  })
  expect_length(shiny::isolate(st$fit_store)$entries, 3L)

  # A new dataset empties the store: the same model on the new data is fit.
  shiny::isolate({ sieve_new_dataset(st); sieve_confirm_types(st) })
  expect_length(shiny::isolate(st$fit_store)$entries, 0L)
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(score_method = "mean", htmt_on = FALSE, marker = "")
    session$setInputs(fit_go = 1)
    expect_identical(seen$runs, 4L)
    expect_false(cfa_run()$reused)
  })
})

test_that("the CFA page reuses a fit made first on another page, names it in the method line, and registers its own analysis", {
  seen <- fc_count()
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  st <- fc_nine()
  keep <- new.env(parent = emptyenv())
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(score_method = "mean", htmt_on = FALSE, marker = "")
    session$setInputs(fit_go = 1)
    expect_identical(seen$runs, 1L)
    expect_false(cfa_run()$reused)
    keep$validity <- cfa_run()
    keep$fits <- fc_fits(seen)
  })
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    fc_cfa_inputs(session)
    session$setInputs(fit_go = 1)
    # No SIEVER fit. (The CFA page's own modification-index table reads the
    # fit through lavaan::modindices(), whose extended object is one
    # lavaan() call, and through lavaan::lavTestScore() for a candidate
    # modindices() left without an index, one such call each, none of them
    # a refit; that is this page's panel, not an estimation of the model,
    # and the Validity page never makes it.)
    expect_identical(seen$runs, keep$fits[["runs"]])
    run <- cfa_run()
    expect_true(run$reused)
    expect_identical(run$reused_from, "Validity")
    expect_identical(run$fit, keep$validity$fit)
    # The Validity page's measures with the unbiased SRMR added, which the
    # CFA page shows and Validity does not: computed once here
    # on the reused fit, no refit, and nothing else differs.
    expect_null(attr(keep$validity$measures, "families")$usrmr)
    expect_identical(setdiff(names(run$measures), names(keep$validity$measures)), c("usrmr", "usrmr_lo", "usrmr_hi"))
    expect_true(attr(run$measures, "families")$usrmr$computed)
    expect_identical(sieve_fit_drop_usrmr(run$measures), keep$validity$measures)
    expect_identical(run$measures, sieve_fit_with_usrmr(keep$validity$measures, keep$validity$fit))
    expect_identical(fm(), run$measures)
    expect_match(
      fc_text(output$method_line),
      sprintf("Reused the fit from Validity at %s \\([0-9.]+ seconds then\\), current\\.", keep$validity$when)
    )
    expect_no_error(output$fit_tbl)
    expect_no_error(output$est_tbl)
    expect_identical(seen$runs, keep$fits[["runs"]])
    # The analysis is the CFA page's own record; the script repeats the call
    # (the Validity page records the fit's call with its tables, not at the
    # fit, so no name was available to alias).
    expect_match(run$analysis_id, "^A[0-9]+$")
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl(sprintf("reused the fit from Validity at %s", keep$validity$when), log, fixed = TRUE)))
    expect_true(any(grepl("cfa_fit <- lavaan::cfa(cfa_model", log, fixed = TRUE)))
  })
})

test_that("the Items page's overall model is stored with its page, and the CFA page reuses it", {
  seen <- fc_count()
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  st <- fc_nine()
  keep <- new.env(parent = emptyenv())
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(factor_cov = "correlated")
    run <- fit_run()
    if (identical(run$status, "not run")) {
      session$setInputs(fit_go = 1)
      run <- fit_run()
    }
    expect_identical(run$status, "ran")
    expect_null(run$error)
    expect_s4_class(run$fit, "lavaan")
    keep$fit <- run$fit
    keep$runs <- seen$runs
  })
  expect_identical(keep$runs, 1L)
  store <- shiny::isolate(st$fit_store)
  expect_length(store$entries, 1L)
  expect_identical(store$entries[[1]]$page, "Items")
  expect_identical(store$entries[[1]]$fit, keep$fit)
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    fc_cfa_inputs(session)
    session$setInputs(fit_go = 1)
    expect_identical(seen$runs, keep$runs)
    expect_true(cfa_run()$reused)
    expect_identical(cfa_run()$reused_from, "Items")
    expect_identical(fit(), keep$fit)
    expect_match(fc_text(output$method_line), "Reused the fit from Items at [0-9]{2}:[0-9]{2}")
  })
})

test_that("shown, the CFA page adopts the Items page's identical fit without a click, also when its measured estimate leaves fitting to the button", {
  local_measured_rates()
  seen <- fc_count()
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  st <- fc_nine()
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(factor_cov = "correlated")
    run <- fit_run()
    if (identical(run$status, "not run")) {
      session$setInputs(fit_go = 1)
      run <- fit_run()
    }
    expect_s4_class(run$fit, "lavaan")
  })
  runs <- seen$runs
  # A slow measured time of the overall model puts the CFA page's estimate
  # above six seconds at this machine's rates, so the page does not fit by
  # itself (sieve_runs_by_itself()).
  sieve_rate_note("cfa_MLR", 60, sieve_fit_units(9, "MLR"))
  expect_gt(sieve_demand_wait("cfa", 9, "MLR"), sieve_auto_slack() * sieve_auto_seconds())
  shiny::testServer(function(input, output, session) mod_cfa_server("cfa", st), {
    session$setInputs(`cfa-structure` = "first", `cfa-ident` = "varone", `cfa-layout` = "tree2",
      `cfa-what` = "std", `cfa-min_path` = 0, `cfa-resid` = TRUE, `cfa-covs` = TRUE)
    expect_identical(shiny::isolate(st$page_results$cfa[["cfa-fit_go"]])$state, "not_run")
    session$setInputs(nav = "structure", ws_structure = "cfa")
    expect_identical(seen$runs, runs)
    expect_identical(shiny::isolate(st$page_results$cfa[["cfa-fit_go"]])$state, "current")
    expect_match(fc_text(output[["cfa-method_line"]]),
      "Reused the fit from Items at [0-9]{2}:[0-9]{2} \\([0-9.]+ seconds? then\\), current\\.")
    expect_false(grepl("waits for you", fc_text(output[["cfa-gate"]]), fixed = TRUE))
  })
})

test_that("a saved project never carries the fit store, and a reopened one starts with an empty store", {
  st <- fc_nine()
  items <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  store <- shiny::isolate(st$fit_store)
  run <- sieve_run_cfa(sieve_cfa_spec(meta, reserved = names(items)), items, store = store, page = "CFA")
  expect_s4_class(run$fit, "lavaan")
  expect_length(store$entries, 1L)
  snap <- sieve_project_snapshot(st)
  expect_identical(snap$dropped, character(0))
  expect_false("fit_store" %in% names(snap$other))
  expect_false(grepl("fit_store", paste(deparse(snap), collapse = ""), fixed = TRUE))
  fresh <- sieve_new_state()
  sieve_project_restore(fresh, snap, source = "test")
  opened <- shiny::isolate(fresh$fit_store)
  expect_true(is.environment(opened))
  expect_length(opened$entries, 0L)
  expect_identical(opened$dataset, shiny::isolate(fresh$dataset_id))
  # The session that saved keeps its own store.
  expect_length(store$entries, 1L)
})
