# The unbiased SRMR of Maydeu-Olivares (2017) with its interval at the RMSEA
# level, from DMAR::sem_residual_fit(), beside the SRMR on the CFA and SEM
# pages, in the methods draft and in the recorded R, so the SRMR is not
# shown as a point estimate only.

usr_hs9 <- function() {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  list(fx = fx, meta = meta, items = fx$data[, fx$item_cols])
}

usr_count <- function(x, pattern) {
  at <- gregexpr(pattern, paste(x, collapse = " "), fixed = TRUE)[[1]]
  sum(at > 0L)
}

usr_dmar <- function(fit, conf_level) {
  x <- DMAR::sem_residual_fit(fit, conf_level = conf_level)
  stats::setNames(suppressWarnings(as.numeric(x$value)), x$term)
}

test_that("the default CFA holds DMAR's unbiased SRMR with its 90% interval beside the SRMR", {
  h <- usr_hs9()
  run <- sieve_run_cfa(sieve_cfa_spec(h$meta), h$items)
  fit <- run$fit
  oracle <- usr_dmar(fit, 0.90)
  u <- sieve_fit_usrmr(fit)
  expect_true(u$available)
  expect_true(u$computed)
  expect_identical(u$conf_level, 0.90)
  expect_identical(u$value, unname(oracle["unbiased_srmr"]))
  expect_identical(u$se, unname(oracle["unbiased_srmr_se"]))
  expect_identical(c(u$lower, u$upper), unname(oracle[c("unbiased_srmr_ci_lower", "unbiased_srmr_ci_upper")]))
  # The values on this fit: SRMR .065, unbiased .056 [.038, .073].
  expect_identical(sprintf("%.3f", c(u$srmr, u$value, u$lower, u$upper)), c("0.065", "0.056", "0.038", "0.073"))

  # In the measures a page shows: after the SRMR, labeled with the level.
  m <- sieve_fit_measures(fit, residual = TRUE)
  expect_identical(unname(m[c("usrmr", "usrmr_lo", "usrmr_hi")]), c(u$value, u$lower, u$upper))
  expect_equal(unname(m["srmr"]), u$srmr, tolerance = 1e-10)
  tab <- sieve_fit_table(m)
  at <- match("SRMR", tab$Statistic)
  expect_identical(tab$Statistic[at + 0:3], c(
    "SRMR", "Unbiased SRMR", "Unbiased SRMR 90% CI lower", "Unbiased SRMR 90% CI upper"
  ))
  note <- paste(sieve_fit_note(m), collapse = " ")
  expect_match(note, "The unbiased SRMR (Maydeu-Olivares, 2017) estimates the population SRMR", fixed = TRUE)
  expect_match(note, "its 90% confidence interval (Maydeu-Olivares, Shi & Rosseel, 2018) is at the level of the RMSEA interval", fixed = TRUE)
  expect_identical(sieve_usrmr_words(m), "unbiased SRMR = 0.056, 90% CI [0.038, 0.073]")
  # Measures read without it (every other page) hold the same values otherwise.
  plain <- sieve_fit_measures(fit)
  expect_false(any(c("usrmr", "usrmr_lo", "usrmr_hi") %in% names(plain)))
  expect_false("Unbiased SRMR" %in% sieve_fit_table(plain)$Statistic)
  expect_identical(c(plain), c(m)[names(plain)])
  # Reused measures gain it once, at their level.
  again <- sieve_fit_measures_at(plain, fit, 0.90, residual = TRUE)
  expect_identical(unname(again["usrmr"]), u$value)
  expect_identical(sieve_fit_measures_at(again, fit, 0.90, residual = TRUE), again)

  # At the RMSEA level the option sets, as DMAR forms it there.
  withr::local_options(sieve.rmsea_conf_level = 0.95)
  m95 <- sieve_fit_measures(fit, conf_level = sieve_default_rmsea_conf_level(), residual = TRUE)
  oracle95 <- usr_dmar(fit, 0.95)
  expect_identical(unname(m95[c("usrmr_lo", "usrmr_hi")]), unname(oracle95[c("unbiased_srmr_ci_lower", "unbiased_srmr_ci_upper")]))
  expect_identical(sieve_fit_label(m95, "usrmr_lo"), "Unbiased SRMR 95% CI lower")

  # The recorded R reproduces the value.
  code <- sieve_usrmr_code(u, "cfa_fit")
  expect_identical(code, "DMAR::sem_residual_fit(cfa_fit, conf_level = 0.9)  # the unbiased SRMR (Maydeu-Olivares, 2017) with its 90% interval, shown beside the SRMR")
  env <- new.env(parent = globalenv())
  env$cfa_fit <- fit
  replayed <- eval(parse(text = code), env)
  expect_identical(replayed$value[replayed$term == "unbiased_srmr"], u$value)
})

test_that("where DMAR does not compute it, DMAR's reason is given verbatim and no number is shown", {
  h <- usr_hs9()
  spec <- sieve_cfa_spec(h$meta)
  # Incomplete data under FIML.
  items <- h$items
  items[c(4L, 60L), 1L] <- NA
  fit <- sieve_run_cfa(spec, items)$fit
  refusal <- tryCatch(DMAR::sem_residual_fit(fit), error = conditionMessage)
  expect_match(refusal, "requires complete data", fixed = TRUE)
  u <- sieve_fit_usrmr(fit)
  expect_true(u$computed)
  expect_false(u$available)
  expect_identical(u$reason, gsub("\\s+", " ", refusal))
  m <- sieve_fit_measures(fit, residual = TRUE)
  expect_true(all(is.na(m[c("usrmr", "usrmr_lo", "usrmr_hi")])))
  expect_identical(sieve_usrmr_words(m), "unbiased SRMR n/a")
  expect_true(any(sieve_fit_note(m) == paste("The unbiased SRMR and its interval are not reported for this fit:", u$reason)))
  reasons <- attr(sieve_fit_family_table(m), "reasons")
  expect_true(any(reasons == paste0("Unbiased SRMR: ", u$reason)))
  code <- sieve_usrmr_code(u, "cfa_fit")
  expect_true(all(startsWith(code, "#")))
  expect_match(paste(code, collapse = " "), "not reported for this fit", fixed = TRUE)

  # Ordered items (WLSMV) and several groups.
  ordered <- as.data.frame(lapply(h$items, function(v) as.integer(cut(v, 4))))
  wl <- lavaan::cfa("V =~ t1_visual_perception + t2_cubes + t4_lozenges", data = ordered,
    ordered = TRUE, std.lv = TRUE)
  expect_identical(sieve_fit_usrmr(wl)$reason, tryCatch(DMAR::sem_residual_fit(wl), error = conditionMessage))
  groups <- lavaan::cfa("V =~ t1_visual_perception + t2_cubes + t4_lozenges", data = h$fx$data,
    group = "school", std.lv = TRUE)
  expect_identical(sieve_fit_usrmr(groups)$reason, tryCatch(DMAR::sem_residual_fit(groups), error = conditionMessage))
  # A model with no solution says why.
  none <- sieve_fit_families(NULL, residual = TRUE)
  expect_false(none$usrmr$available)
  expect_identical(none$usrmr$reason, "There is no fitted model.")
})

test_that("a fit with an auxiliary variable is summarized by the same model without it, as DMAR advises", {
  h <- usr_hs9()
  d <- h$fx$data[, c(h$fx$item_cols, "age_years")]
  spec <- sieve_spec_engine_args(sieve_cfa_spec(h$meta, reserved = names(d)), list(aux = "age_years"))
  fit <- sieve_run_cfa(spec, d)$fit
  # DMAR itself refuses a fit with an auxiliary variable, with no number.
  expect_match(tryCatch(DMAR::sem_residual_fit(fit), error = conditionMessage), "auxiliary variables (age_years)", fixed = TRUE)
  u <- sieve_fit_usrmr(fit)
  expect_true(u$available)
  expect_identical(u$without_aux$aux, "age_years")
  plain <- sieve_run_cfa(sieve_cfa_spec(h$meta), h$items)$fit
  expect_equal(c(u$value, u$lower, u$upper), unname(unlist(sieve_fit_usrmr(plain)[c("value", "lower", "upper")])), tolerance = 1e-5)
  expect_match(u$residual, "of the model's own variables, fit to the same respondents without the auxiliary variable age_years", fixed = TRUE)
  # The recorded R refits the model without it and reproduces the value.
  code <- sieve_usrmr_code(u, "cfa_fit")
  expect_false(any(grepl("ordered = NULL", code, fixed = TRUE)))
  env <- new.env(parent = globalenv())
  env$cfa_fit <- fit
  env$cfa_model <- spec$model
  replayed <- eval(parse(text = code), env)
  expect_equal(replayed$value[replayed$term == "unbiased_srmr"], u$value, tolerance = 1e-10)
  # With a model variable missing, DMAR's refusal stands.
  d$t2_cubes[3L] <- NA
  missing_fit <- sieve_run_cfa(spec, d)$fit
  u2 <- sieve_fit_usrmr(missing_fit)
  expect_false(u2$available)
  expect_null(u2$without_aux)
  expect_identical(u2$reason, gsub("\\s+", " ", tryCatch(DMAR::sem_residual_fit(missing_fit), error = conditionMessage)))
})

test_that("a model too large to compute it with the fit leaves it for its button, with the estimate", {
  # The estimated time grows with the cube of the residual covariances.
  expect_lt(sieve_usrmr_seconds(9), 0.1)
  expect_lt(sieve_usrmr_seconds(24), 2)
  expect_gt(sieve_usrmr_seconds(40), 2)
  expect_gt(sieve_usrmr_seconds(80), 200)
  h <- usr_hs9()
  fit <- sieve_run_cfa(sieve_cfa_spec(h$meta), h$items)$fit
  withr::local_options(sieve.auto_run = "never")
  u <- sieve_fit_usrmr(fit)
  expect_false(u$computed)
  expect_false(u$available)
  expect_match(u$reason, "forms the asymptotic covariance matrix of the 45 standardized residual covariances of these 9 variables", fixed = TRUE)
  # The reason is the policy, said in grammatical words (never "an
  # estimated under 2 seconds" or "an estimated an unknown time").
  expect_true(startsWith(u$reason, "options(sieve.auto_run = \"never\") leaves every computation to its button. DMAR::sem_residual_fit()"))
  expect_match(u$reason, "which is estimated to take under 2 seconds here", fixed = TRUE)
  expect_false(grepl("an estimated (under|an|about)", u$reason))
  m <- sieve_fit_set_usrmr(sieve_fit_measures(fit), u)
  expect_identical(sieve_usrmr_words(m), "unbiased SRMR not computed")
  expect_true(any(startsWith(sieve_fit_note(m), "The unbiased SRMR and its interval were not computed with the fit: options(sieve.auto_run")))
  code <- sieve_usrmr_code(u, "cfa_fit")
  expect_identical(code[1L], "# The unbiased SRMR with its 90% interval was not computed with the fit (estimated time: under 2 seconds); to compute it:")
  expect_identical(code[2L], "# DMAR::sem_residual_fit(cfa_fit, conf_level = 0.9)")
  # Asked for, it is computed whatever the estimate.
  expect_true(sieve_fit_usrmr(fit, compute = "always")$available)
  withr::local_options(sieve.auto_run = "always")
  expect_true(sieve_fit_usrmr(fit)$computed)
  # Under the default policy, a model whose estimate is over its two
  # seconds says so.
  withr::local_options(sieve.auto_run = NULL)
  local_mocked_bindings(sieve_usrmr_seconds = function(p) 150)
  big <- sieve_fit_usrmr(fit)
  expect_false(big$computed)
  expect_match(big$reason, "which is estimated to take about 3 minutes here (the time grows with the cube of their number), longer than the 2 seconds the run policy lets a result run by itself.", fixed = TRUE)
})

test_that("a fit DMAR refuses gets DMAR's reason at once, not a wait estimate and a button", {
  # Every estimate over the run policy, as for a model of 36 or more items.
  local_mocked_bindings(sieve_usrmr_seconds = function(p) 150)
  h <- usr_hs9()
  spec <- sieve_cfa_spec(h$meta)
  verbatim <- function(f) gsub("\\s+", " ", tryCatch(DMAR::sem_residual_fit(f), error = conditionMessage))
  # None of these is left "not computed" with a wait estimate and a button
  # that would only return DMAR's refusal: each gives DMAR's reason at once.
  items <- h$items
  items[c(4L, 60L), 1L] <- NA
  fiml <- sieve_run_cfa(spec, items)$fit
  ordered <- as.data.frame(lapply(h$items, function(v) as.integer(cut(v, 4))))
  wl <- lavaan::cfa("V =~ t1_visual_perception + t2_cubes + t4_lozenges", data = ordered,
    ordered = TRUE, std.lv = TRUE)
  groups <- lavaan::cfa("V =~ t1_visual_perception + t2_cubes + t4_lozenges", data = h$fx$data,
    group = "school", std.lv = TRUE)
  d <- h$fx$data[, c(h$fx$item_cols, "age_years")]
  aux_spec <- sieve_spec_engine_args(sieve_cfa_spec(h$meta, reserved = names(d)), list(aux = "age_years"))
  d_missing <- d
  d_missing$t2_cubes[3L] <- NA
  aux_missing <- sieve_run_cfa(aux_spec, d_missing)$fit
  for (f in list(fiml, wl, groups, aux_missing)) {
    expect_true(sieve_usrmr_refused(f))
    u <- sieve_fit_usrmr(f)
    expect_true(u$computed)
    expect_false(u$available)
    expect_identical(u$reason, verbatim(f))
    expect_false(grepl("estimated", u$reason, fixed = TRUE))
    expect_null(sieve_usrmr_button(u))
  }
  # A fit DMAR computes, or one whose auxiliary variable it would summarize
  # by the same model without it, still waits for its button.
  complete <- sieve_run_cfa(spec, h$items)$fit
  aux_complete <- sieve_run_cfa(aux_spec, d)$fit
  for (f in list(complete, aux_complete)) {
    expect_false(sieve_usrmr_refused(f))
    u <- sieve_fit_usrmr(f)
    expect_false(u$computed)
    expect_match(u$reason, "is estimated to take about 3 minutes here", fixed = TRUE)
    expect_false(is.null(sieve_usrmr_button(u)))
  }
})

test_that("a page that does not show the unbiased SRMR reads measures without it, whichever page fit first", {
  h <- usr_hs9()
  fit <- sieve_run_cfa(sieve_cfa_spec(h$meta), h$items)$fit
  # The CFA page's measures hold it, and the fit store keeps them.
  held <- sieve_fit_measures(fit, residual = TRUE)
  expect_true("usrmr" %in% names(held))
  # A page that reuses them at its own level (the Validity page) reads them
  # without it, so its method line does not depend on which page ran first.
  reused <- sieve_fit_measures_at(held, fit, 0.90)
  expect_false(any(c("usrmr", "usrmr_lo", "usrmr_hi") %in% names(reused)))
  expect_null(attr(reused, "families")$usrmr)
  fresh <- sieve_fit_measures_at(NULL, fit, 0.90)
  expect_identical(c(reused), c(fresh))
  expect_identical(attr(reused, "family"), attr(fresh, "family"))
  # Identical in every attribute: measures read without it hold no usrmr
  # element in their families, as the measures it was dropped from do not,
  # so a page's measures do not differ by which page fit first. Added back,
  # it is where a page that computed it first holds it.
  expect_false("usrmr" %in% names(attr(fresh, "families")))
  expect_identical(reused, fresh)
  expect_identical(sieve_fit_with_usrmr(fresh, fit), held)
  expect_identical(
    sieve_method_text(fit, "Correlated-factors CFA", measures = reused),
    sieve_method_text(fit, "Correlated-factors CFA", measures = fresh)
  )
  expect_false(grepl("unbiased", sieve_method_text(fit, "Correlated-factors CFA", measures = reused), fixed = TRUE))
  # Measures without it are returned unchanged.
  expect_identical(sieve_fit_drop_usrmr(fresh), fresh)
  expect_null(sieve_fit_drop_usrmr(NULL))
})

test_that("the unbiased SRMR and its interval are cited when, and only when, a record holds a value", {
  h <- usr_hs9()
  fit <- sieve_run_cfa(sieve_cfa_spec(h$meta), h$items)$fit
  spec <- attr(fit, "sieve_spec")
  with_value <- sieve_fit_measures(fit, residual = TRUE)
  keys <- sieve_spec_methods(spec, list(fit = with_value), fitted = TRUE)
  expect_true(all(c("usrmr", "usrmr_interval") %in% keys))
  expect_false(any(c("usrmr", "usrmr_interval") %in% sieve_spec_methods(spec, list(fit = sieve_fit_measures(fit)), fitted = TRUE)))
  refused <- sieve_fit_set_usrmr(sieve_fit_measures(fit), list(
    value = NA_real_, lower = NA_real_, upper = NA_real_, available = FALSE, computed = TRUE,
    conf_level = 0.9, reason = "DMAR refused."
  ))
  expect_false(any(c("usrmr", "usrmr_interval") %in% sieve_spec_methods(spec, list(fit = refused), fitted = TRUE)))
  bib <- sieve_method_bibliography(c("usrmr", "usrmr_interval"), software = FALSE)
  expect_true(any(grepl("Maydeu-Olivares, A. (2017). Assessing the size of model misfit in structural equation models. Psychometrika, 82(3), 533--558. https://doi.org/10.1007/s11336-016-9552-7", bib, fixed = TRUE)))
  expect_true(any(grepl("Maydeu-Olivares, A., Shi, D., & Rosseel, Y. (2018). Assessing fit in structural equation models: A Monte-Carlo evaluation of RMSEA versus SRMR confidence intervals and tests of close fit. Structural Equation Modeling, 25(3), 389--402. https://doi.org/10.1080/10705511.2017.1389611", bib, fixed = TRUE)))
})

test_that("the methods draft reports the unbiased SRMR with its interval beside the SRMR, and cites both", {
  # Not on the CRAN check's clock: each draft refits the CFA and the
  # reliability of every construct (about four seconds).
  skip_on_cran()
  h <- usr_hs9()
  # The draft is an explicit request: it does not follow the run policy, so
  # it computes the value under sieve.auto_run = "never" and for a model of
  # any size.
  d <- withr::with_options(list(sieve.auto_run = "never"), sieve_methods_draft(h$items, h$meta, "the bundled example"))
  expect_match(d, "SRMR = 0.065 (unbiased SRMR = 0.056, 90% CI [0.038, 0.073])", fixed = TRUE)
  expect_match(d, "The unbiased SRMR (Maydeu-Olivares, 2017)", fixed = TRUE)
  expect_match(d, "(Maydeu-Olivares, Shi & Rosseel, 2018)", fixed = TRUE)
  # None held: the draft's own value, said once, with no record named.
  lines <- strsplit(d, "\n", fixed = TRUE)[[1]]
  fit_line <- grep("fit as follows", lines, value = TRUE, fixed = TRUE)
  expect_length(fit_line, 1L)
  expect_false(grepl("taken from that record", fit_line, fixed = TRUE))
  expect_false(grepl("not reported", fit_line, fixed = TRUE))
  expect_false(grepl("not computed", fit_line, fixed = TRUE))
  # The works cited in the text are in the draft's references.
  refs <- lines[seq(match("## References for This Draft", lines), length(lines))]
  expect_true(any(startsWith(refs, "- Maydeu-Olivares, A. (2017). Assessing the size of model misfit")))
  expect_true(any(startsWith(refs, "- Maydeu-Olivares, A., Shi, D., & Rosseel, Y. (2018). Assessing fit in structural equation models")))
  # Left out on request, and the draft says so; nothing is cited for it.
  off <- sieve_methods_draft(h$items, h$meta, "the bundled example", unbiased_srmr = FALSE)
  expect_match(off, "SRMR = 0.065. ", fixed = TRUE)
  expect_match(off, "The unbiased SRMR and its interval were not computed for this draft (unbiased_srmr = FALSE).", fixed = TRUE)
  expect_false(grepl("Maydeu-Olivares", off, fixed = TRUE))
})

test_that("the CFA tab shows the unbiased SRMR, records its call, and offers a large model's on a button", {
  # Not on the CRAN check's clock: two fits of the CFA tab (6 to 12 seconds
  # each).
  skip_on_cran()
  test_frame <- environment()
  st <- siever_test_study()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", layout = "tree2", what = "std",
      min_path = 0, resid = TRUE, covs = TRUE)
    session$setInputs(fit_go = 1L)
    m <- fm()
    expect_identical(sprintf("%.3f", m[c("usrmr", "usrmr_lo", "usrmr_hi")]), c("0.056", "0.038", "0.073"), ignore_attr = TRUE)
    box <- paste(as.character(output$vb_srmr$html), collapse = " ")
    expect_match(box, "unbiased SRMR = 0.056, 90% CI [0.038, 0.073]", fixed = TRUE)
    expect_match(paste(as.character(output$overview_fit$html), collapse = " "), "unbiased SRMR = 0.056", fixed = TRUE)
    expect_null(output$usrmr_action)
    # The bundled example declares age an auxiliary variable, but every
    # analyzed item is observed in every row, so the fit leaves it out
    # (sieve_auxiliary_apply()) and DMAR computes the value from the fit
    # itself, with no refit. The refit for a fit that
    # does carry an auxiliary variable is tested above, on sieve_run_cfa().
    expect_null(cfa_run()$spec$engine_args$aux)
    expect_null(attr(m, "families")$usrmr$without_aux)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "# Auxiliary variable age_years is not used", fixed = TRUE)
    expect_match(log, "DMAR::sem_residual_fit(cfa_fit, conf_level = 0.9)  # the unbiased SRMR", fixed = TRUE)
    expect_match(log, 'missing = "fiml", test = "standard"', fixed = TRUE)
    expect_false(grepl("cfa_fit_model_only", log, fixed = TRUE))
    # The Show code readout under the fit table follows the record: the
    # call that gave the value.
    readout <- paste(as.character(output$fit_code$html), collapse = " ")
    expect_match(readout, "DMAR::sem_residual_fit(cfa_fit, conf_level = 0.9)", fixed = TRUE)
    expect_false(grepl("cfa_fit_model_only", readout, fixed = TRUE))
  })

  # A model whose estimate is over the run policy's two seconds: the
  # measures say so, and the button computes it for the fit on screen.
  local_mocked_bindings(sieve_usrmr_seconds = function(p) 150, .env = test_frame)
  st2 <- siever_test_study()
  shiny::testServer(mod_cfa_server, args = list(state = st2), {
    session$setInputs(structure = "first", ident = "varone", layout = "tree2", what = "std",
      min_path = 0, resid = TRUE, covs = TRUE)
    session$setInputs(fit_go = 1L)
    expect_false(attr(fm(), "families")$usrmr$computed)
    expect_match(paste(as.character(output$vb_srmr$html), collapse = " "), "unbiased SRMR not computed", fixed = TRUE)
    id <- cfa_run()$analysis_id
    # The saved-results report says why its Unbiased SRMR row is empty.
    before <- sieve_readable_saved_report(state, id)
    expect_match(before, "The unbiased SRMR and its interval were not computed with the fit: DMAR::sem_residual_fit()", fixed = TRUE)
    expect_false(grepl("Maydeu-Olivares, A. (2017)", before, fixed = TRUE))
    action <- paste(as.character(output$usrmr_action$html), collapse = " ")
    expect_match(action, "Compute the Unbiased SRMR and Its 90% Interval (about 3 minutes)", fixed = TRUE)
    session$setInputs(usrmr_action_go = 1L)
    u <- attr(fm(), "families")$usrmr
    expect_true(u$computed)
    expect_identical(sprintf("%.3f", u$value), "0.056")
    expect_match(paste(as.character(output$vb_srmr$html), collapse = " "), "unbiased SRMR = 0.056, 90% CI [0.038, 0.073]", fixed = TRUE)
    expect_null(output$usrmr_action)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "CFA: the unbiased SRMR with its 90% interval, on request", fixed = TRUE)
    # The analysis record holds the value computed on the button, so the
    # saved-results report shows it and cites it.
    record <- shiny::isolate(state$analyses)[[id]]
    expect_identical(unname(record$estimates$fit["usrmr"]), u$value)
    expect_true(all(c("usrmr", "usrmr_interval") %in% record$methods))
    after <- sieve_readable_saved_report(state, id)
    # The row as print() formats the column (three significant digits,
    # fixed or scientific as the column's other values decide).
    row_of <- function(report) grep("^\\s*Unbiased SRMR\\s", strsplit(report, "\n", fixed = TRUE)[[1]], value = TRUE)
    shown <- c(sprintf("%.4f", u$value), sprintf("%.2e", u$value))
    expect_length(row_of(after), 1L)
    expect_true(any(vapply(shown, grepl, logical(1), x = row_of(after), fixed = TRUE)))
    expect_false(any(vapply(shown, grepl, logical(1), x = row_of(before), fixed = TRUE)))
    expect_false(grepl("were not computed with the fit", after, fixed = TRUE))
    expect_match(after, "Maydeu-Olivares, A. (2017)", fixed = TRUE)
  })
})

test_that("the user guide says the bundled example is fit without its auxiliary, so its unbiased SRMR is that fit's own", {
  # The guide does not call the bundled example a fit summarized by its
  # refit without age: a declared auxiliary is left out of a fit whose
  # analyzed items are all observed (sieve_auxiliary_apply()).
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  guide <- gsub("[[:space:]]+", " ", paste(readLines(file.path(root, "vignettes", "user-guide.Rmd"),
    warn = FALSE, encoding = "UTF-8"), collapse = " "))
  expect_false(grepl("is such a fit", guide, fixed = TRUE))
  expect_match(guide, paste(
    "A fit that carries auxiliary variables while its model variables are all observed in the analyzed rows",
    "is summarized, as DMAR advises, by the same model fit to the same respondents without them"), fixed = TRUE)
  expect_match(guide, paste(
    "SIEVER's pages leave a declared auxiliary out of a fit whose analyzed items are all observed",
    "(Ingest's Variable roles panel), so the bundled example, which declares age an auxiliary, is fit",
    "without it, and its unbiased SRMR is that fit's own."), fixed = TRUE)
})

test_that("the SEM method line carries the unbiased SRMR beside the SRMR", {
  h <- usr_hs9()
  # A fit read by the SEM page's helpers: the method words.
  fit <- sieve_run_cfa(sieve_cfa_spec(h$meta), h$items)$fit
  m <- sieve_fit_measures(fit, residual = TRUE)
  text <- sieve_method_text(fit, "Structural model", measures = m)
  expect_match(text, "SRMR 0.065 (unbiased SRMR = 0.056, 90% CI [0.038, 0.073]).", fixed = TRUE)
  expect_false(grepl("unbiased", sieve_method_text(fit, "Structural model", measures = sieve_fit_measures(fit)), fixed = TRUE))
})

test_that("the SEM page's analysis record holds the unbiased SRMR the page shows, so the saved-results report gives and cites it", {
  # Not on the CRAN check's clock: one fit of the SEM tab.
  skip_on_cran()
  st <- siever_test_study()
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(outcomes = "Speed", predictors = c("Visual", "Textual"))
    session$setInputs(fit_go = 1L)
    u <- attr(fm(), "families")$usrmr
    expect_true(u$available)
    id <- path_run()$analysis_id
    record <- shiny::isolate(state$analyses)[[id]]
    # The record holds the measures with it, so a value computed with the
    # fit is on the page, in the record, in the report and in its references.
    expect_identical(unname(record$estimates$fit["usrmr"]), u$value)
    expect_identical(attr(record$estimates$fit, "families")$usrmr$value, u$value)
    expect_true(all(c("usrmr", "usrmr_interval") %in% record$methods))
    report <- sieve_readable_saved_report(state, id)
    expect_length(grep("^\\s*Unbiased SRMR\\s", strsplit(report, "\n", fixed = TRUE)[[1]]), 1L)
    expect_match(report, "Maydeu-Olivares, A. (2017)", fixed = TRUE)
  })
})

test_that("the methods draft takes the CFA tab's unbiased SRMR for the identical fit, and its button states a long estimate", {
  # Not on the CRAN check's clock: one fit of the CFA tab and two drafts.
  skip_on_cran()
  test_frame <- environment()
  # A study without the bundled example's auxiliary variable, so the CFA
  # tab fits the draft's own model.
  st <- siever_test_study(configure = function(s) {
    roles <- sieve_declared_roles(s$raw, names(s$items))
    roles$role[roles$role == "auxiliary"] <- "criterion"
    s$roles <- roles
  })
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", layout = "tree2", what = "std",
      min_path = 0, resid = TRUE, covs = TRUE)
    session$setInputs(fit_go = 1L)
    expect_true(attr(fm(), "families")$usrmr$available)
  })
  items <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  analyses <- shiny::isolate(st$analyses)
  revision <- sieve_revision(st)
  id <- names(analyses)[vapply(analyses, function(r) identical(r$kind, "cfa"), logical(1))]
  expect_length(id, 1L)
  held <- sieve_draft_usrmr_held(analyses, sieve_cfa_spec(meta, reserved = names(items)), revision, 0.90)
  expect_identical(held$id, id)
  # A value the CFA tab computed for the identical fit is not computed again
  # (DMAR::sem_residual_fit() takes minutes for a large model).
  local_mocked_bindings(sieve_fit_usrmr = function(...) stop("computed again"), .env = test_frame)
  d <- sieve_methods_draft(items, meta, "the bundled example", revision = revision, analyses = analyses)
  expect_match(d, "SRMR = 0.065 (unbiased SRMR = 0.056, 90% CI [0.038, 0.073])", fixed = TRUE)
  expect_match(d, sprintf("The unbiased SRMR and its interval are those the CFA tab computed for this identical fit (the same model, data, respondents and engines; analysis %s), taken from that record.", id), fixed = TRUE)
  expect_match(d, "The unbiased SRMR (Maydeu-Olivares, 2017)", fixed = TRUE)
  fit_line <- grep("fit as follows", strsplit(d, "\n", fixed = TRUE)[[1]], value = TRUE, fixed = TRUE)
  expect_identical(usr_count(fit_line, "The unbiased SRMR and its interval"), 1L)
  expect_false(grepl("not reported", fit_line, fixed = TRUE))
  expect_false(grepl("not computed", fit_line, fixed = TRUE))
  # The provenance names both things the draft takes from a record, and its
  # statement that every number was computed when the draft was assembled
  # names the two exceptions.
  expect_match(d, "apart from the two exceptions below", fixed = TRUE)
  expect_match(d, paste0(
    "The draft does not read the results shown on the CFA or other tabs, with two exceptions, each taken ",
    "from a record of the same computation on these data and named where it appears: a bootstrap interval ",
    "the Reliability tab ran for the same request, and the unbiased SRMR, or DMAR's refusal of it, the CFA ",
    "tab computed for this identical fit (the same model, data, respondents and engines)."
  ), fixed = TRUE)
  # Another identity is not taken: another estimator, other respondents, or
  # a value left for the tab's button.
  expect_null(sieve_draft_usrmr_held(analyses, sieve_cfa_spec(meta, reserved = names(items), estimator = "ml"), revision, 0.90))
  other <- revision
  other$rows_key <- sieve_hash(2:301)
  expect_null(sieve_draft_usrmr_held(analyses, sieve_cfa_spec(meta, reserved = names(items)), other, 0.90))
  expect_null(sieve_draft_usrmr_held(analyses, sieve_cfa_spec(meta, reserved = names(items)), revision, 0.95))
  pending <- analyses
  pending[[id]]$estimates$fit <- sieve_fit_set_usrmr(pending[[id]]$estimates$fit,
    list(value = NA_real_, lower = NA_real_, upper = NA_real_, available = FALSE, computed = FALSE,
      seconds = 150, conf_level = 0.9, reason = "left for its button"))
  expect_null(sieve_draft_usrmr_held(pending, sieve_cfa_spec(meta, reserved = names(items)), revision, 0.90))
  expect_error(sieve_methods_draft(items, meta, "the bundled example", revision = revision, analyses = pending), "computed again")

  # The button states the estimate when the draft will compute it and it
  # exceeds the run policy's two seconds.
  local_mocked_bindings(sieve_usrmr_seconds = function(p) 150, .env = test_frame)
  slow <- sieve_draft_usrmr_plan(items, meta, revision = revision, analyses = pending)
  expect_true(slow$slow)
  expect_identical(slow$p, 9L)
  expect_identical(sieve_draft_button_label(slow), "Run a new analysis and draft (the unbiased SRMR alone: about 3 minutes)")
  # Held for the identical fit: nothing to wait for.
  quick <- sieve_draft_usrmr_plan(items, meta, revision = revision, analyses = analyses)
  expect_identical(quick$held, id)
  expect_identical(sieve_draft_button_label(quick), "Run a new analysis and draft")
  # DMAR refuses incomplete data under FIML at once, so no wait is stated.
  incomplete <- items
  incomplete[3L, 2L] <- NA
  refused <- sieve_draft_usrmr_plan(incomplete, meta, revision = revision, analyses = NULL)
  expect_true(refused$refused)
  expect_identical(sieve_draft_button_label(refused), "Run a new analysis and draft")
  # The Report page's button reads the plan, and its text says what the draft
  # computes and when it states the time.
  shiny::isolate(st$analyses <- pending)
  shiny::testServer(mod_repro_server, args = list(state = st), {
    session$flushReact()
    expect_true(draft_plan()$slow)
    expect_identical(sieve_draft_button_label(draft_plan()), "Run a new analysis and draft (the unbiased SRMR alone: about 3 minutes)")
  })
  ui <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(mod_repro_ui("repro")), collapse = "")))
  expect_match(ui, "the CFA fit with the RMSEA interval and the unbiased SRMR with its interval", fixed = TRUE)
  expect_match(ui, "the button states that time when it exceeds two seconds", fixed = TRUE)
  # What the draft reads is said once, as the draft's provenance says it,
  # never as a value taken from the CFA tab's record beside a sentence that
  # says the draft reads no result of that tab.
  expect_match(ui, paste0(
    "The draft does not read the results shown on the CFA tab or any other, with two exceptions, each taken ",
    "from a record of the same computation on the same data and named where it appears: a bootstrap interval ",
    "the Reliability tab ran for the same request, and the unbiased SRMR, or DMAR's refusal of it, the CFA tab ",
    "computed for the identical fit (the same model, data, respondents and engines)."
  ), fixed = TRUE)
  expect_false(grepl("The draft does not read the results shown on the CFA tab, and", ui, fixed = TRUE))
  expect_false(grepl("is taken from its record instead of being computed again", ui, fixed = TRUE))
})

test_that("the methods draft gives DMAR's refusal of the unbiased SRMR once, from the CFA tab's record or its own", {
  # Not on the CRAN check's clock: one fit of the CFA tab and two drafts.
  skip_on_cran()
  test_frame <- environment()
  # Incomplete data under FIML with no auxiliary declared, so DMAR refuses
  # the unbiased SRMR (it requires complete data) and the CFA tab's record
  # holds that refusal: computed, not available.
  st <- siever_test_study(configure = function(s) {
    roles <- sieve_declared_roles(s$raw, names(s$items))
    roles$role[roles$role == "auxiliary"] <- "criterion"
    s$roles <- roles
    it <- s$items
    it[3L, 2L] <- NA
    s$items <- it
  })
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", layout = "tree2", what = "std",
      min_path = 0, resid = TRUE, covs = TRUE)
    session$setInputs(fit_go = 1L)
    u <- attr(fm(), "families")$usrmr
    expect_true(u$computed)
    expect_false(u$available)
  })
  items <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  analyses <- shiny::isolate(st$analyses)
  revision <- sieve_revision(st)
  id <- names(analyses)[vapply(analyses, function(r) identical(r$kind, "cfa"), logical(1))]
  expect_length(id, 1L)
  held <- sieve_draft_usrmr_held(analyses, sieve_cfa_spec(meta, reserved = names(items)), revision, 0.90)
  expect_identical(held$id, id)
  reason <- held$usrmr$reason
  expect_true(is.character(reason) && nzchar(reason))
  fit_line <- function(d) grep("fit as follows", strsplit(d, "\n", fixed = TRUE)[[1]], value = TRUE, fixed = TRUE)

  # None held: the draft asks DMAR itself and gives its reason, once.
  own <- fit_line(sieve_methods_draft(items, meta, "the bundled example", revision = revision, analyses = NULL))
  expect_length(own, 1L)
  expect_match(own, "(unbiased SRMR n/a)", fixed = TRUE)
  expect_match(own, sprintf("The unbiased SRMR and its interval are not reported for this fit: %s", reason), fixed = TRUE)
  expect_identical(usr_count(own, "The unbiased SRMR and its interval"), 1L)
  expect_false(grepl("CFA tab", own, fixed = TRUE))
  expect_false(grepl("not computed", own, fixed = TRUE))

  # Held: the refusal is taken from the record, not asked for again, and one
  # sentence gives DMAR's reason and names the record, rather than one that
  # says the value is not reported and another that it was taken from the
  # record.
  local_mocked_bindings(sieve_fit_usrmr = function(...) stop("computed again"), .env = test_frame)
  taken <- fit_line(sieve_methods_draft(items, meta, "the bundled example", revision = revision, analyses = analyses))
  expect_length(taken, 1L)
  expect_match(taken, "(unbiased SRMR n/a)", fixed = TRUE)
  expect_match(taken, sprintf(
    paste0(
      "The unbiased SRMR and its interval are not reported for this fit, for the reason DMAR gave when the CFA ",
      "tab asked for them for this identical fit (the same model, data, respondents and engines; analysis %s), ",
      "taken from that record: %s"
    ),
    id, reason
  ), fixed = TRUE)
  expect_identical(usr_count(taken, "The unbiased SRMR and its interval"), 1L)
  expect_false(grepl("those the CFA tab computed", taken, fixed = TRUE))
  expect_false(grepl("not computed", taken, fixed = TRUE))
  # The button has nothing to wait for.
  expect_identical(sieve_draft_button_label(sieve_draft_usrmr_plan(items, meta, revision = revision, analyses = analyses)),
    "Run a new analysis and draft")
})
