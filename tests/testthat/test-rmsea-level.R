# Every RMSEA interval SIEVER shows, records or reports is at the RMSEA
# level, 90% by default (DMAR's default for the RMSEA interval, and
# lavaan's), labeled with its level, with an option to change it
# (sieve.rmsea_conf_level); the page's other intervals stay at 95%, and a
# page's confidence level input does not reach the RMSEA interval, as
# DMAR's conf_level does not. A 100(1 - 2 alpha) percent interval decides
# the one-sided tests of close and not close fit at level alpha (MacCallum,
# Browne & Sugawara, 1996), so the limits of the 90% interval are those
# tests at .05.
#
# The parity checks: on SIEVER's bundled nine tests, complete and with
# values removed at random, for ML, MLR and WLSMV fits, the interval SIEVER
# reads equals lavaan's fitMeasures() at that rmsea.ci.level and
# DMAR::sem_fit_measures(fit, conf_level) for the version shown, to 1e-10;
# psychonetrics' 90% interval is reproduced by the DMAR route the Network
# page uses; the recorded R reproduces the displayed interval.
#
# Author: Ken Kelley

skip_if_not_installed("lavaan")
skip_if_not_installed("DMAR")

rl_text <- function(x) {
  if (is.null(x)) return("")
  txt <- gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
  txt <- gsub("&gt;", ">", gsub("&lt;", "<", txt, fixed = TRUE), fixed = TRUE)
  gsub("&amp;", "&", txt, fixed = TRUE)
}
rl_pct <- function(level) if (level == 0.95) "95%" else "90%"
# A help page's source as one line of text: from the source tree's man/
# when the tests run beside it, else from the installed package.
rl_rd_text <- function(topic) {
  root <- compat_source_root()
  rd <- if (!is.null(root)) {
    tools::parse_Rd(file.path(root, "man", paste0(topic, ".Rd")))
  } else {
    tools::Rd_db("SIEVER")[[paste0(topic, ".Rd")]]
  }
  gsub("\\s+", " ", paste(as.character(rd), collapse = ""))
}

# The bundled nine tests, complete or with 8% of the values removed at
# random (the seed is set locally and the caller's stream is restored).
rl_items <- function(incomplete = FALSE) {
  fx <- sieve_hs9_fixture()
  X <- fx$data[, fx$item_cols, drop = FALSE]
  if (incomplete) {
    drop <- withr::with_seed(20261107L, matrix(stats::runif(prod(dim(X))) < 0.08, nrow(X)))
    X[drop] <- NA
  }
  X
}
# The same items cut into five ordered categories at their quintiles.
rl_ordered <- function(X) {
  as.data.frame(lapply(X, function(v) {
    as.integer(cut(v, stats::quantile(v, 0:5 / 5, na.rm = TRUE), include.lowest = TRUE, labels = FALSE))
  }))
}
rl_meta <- function(X, type = "continuous") {
  data.frame(
    item = names(X), label = names(X),
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
    facet = NA_character_, type = type, status = "active", reverse = FALSE,
    stringsAsFactors = FALSE
  )
}
# The fits the parity runs over: SIEVER's own CFA under each estimator
# choice, set explicitly (the subject is the interval, not the default).
rl_fits <- function() {
  cont <- list(complete = rl_items(FALSE), incomplete = rl_items(TRUE))
  out <- list()
  for (what in names(cont)) {
    X <- cont[[what]]
    for (est in c("ml", "ml_fiml", "mlr")) {
      out[[paste(est, what)]] <- sieve_fit_cfa(X, rl_meta(X), estimator = est)
    }
    O <- rl_ordered(X)
    out[[paste("wlsmv", what)]] <- sieve_fit_cfa(O, rl_meta(O, "ordered"), estimator = "wlsmv")
  }
  out
}
rl_gap <- function(a, b) max(abs(unname(a) - unname(b)))
rl_dmar_limits <- function(fit, level, version) {
  t <- suppressWarnings(DMAR::sem_fit_measures(fit, conf_level = level, version = version))
  stats::setNames(t$value, t$term)[c("rmsea_ci_lower", "rmsea_ci_upper")]
}

# The Network checks below (psychonetrics, when installed) can leave a
# graphics device open and an Rplots.pdf in the test folder; this file
# closes what it opened and removes the file it created, so later files
# see neither (test-zz-option-hygiene.R checks).
rl_devices_before <- grDevices::dev.list()
rl_rplots_before <- file.exists("Rplots.pdf")
withr::defer({
  for (d in setdiff(grDevices::dev.list(), rl_devices_before)) grDevices::dev.off(d)
  if (!rl_rplots_before && file.exists("Rplots.pdf")) unlink("Rplots.pdf")
})

test_that("the RMSEA level is 90% by default, DMAR's, apart from the 95% of the other intervals, and the option changes it", {
  expect_identical(sieve_default_rmsea_conf_level(), 0.90)
  expect_identical(sieve_default_conf_level(), 0.95)
  # DMAR's own defaults, which SIEVER follows: the RMSEA interval of
  # sem_fit_measures(), factor_count() and ss_aipe_rmsea() at 0.90, and
  # fa_exploratory()'s and irt_grm()'s rmsea_conf_level (DMAR 1.1.0, the
  # version SIEVER requires, has both).
  expect_identical(eval(formals(DMAR::sem_fit_measures)$conf_level), 0.90)
  expect_identical(eval(formals(DMAR::factor_count)$conf_level), 0.90)
  expect_identical(eval(formals(DMAR::ss_aipe_rmsea)$conf_level), 0.90)
  expect_identical(eval(formals(DMAR::fa_exploratory)$rmsea_conf_level), 0.90)
  expect_identical(eval(formals(DMAR::irt_grm)$rmsea_conf_level), 0.90)
  # Every SIEVER default that forms an RMSEA interval reads the RMSEA level.
  for (f in list(sieve_fit_families, sieve_fit_measures, sieve_fit_measures_at, sieve_fit_compare,
    sieve_compare_structures, sieve_efa_fit_by_k, sieve_network_fit, sieve_reliability_fit_summary,
    sieve_cfa_code, sieve_efa_count_call)) {
    expect_identical(formals(f)$conf_level, quote(sieve_default_rmsea_conf_level()))
  }
  expect_identical(formals(sieve_efa_dmar_call)$conf_level, quote(sieve_default_conf_level()))
  expect_identical(formals(sieve_efa_dmar_call)$rmsea_conf_level, quote(sieve_default_rmsea_conf_level()))
  expect_identical(formals(sieve_cv_fit_row)$conf_level, quote(sieve_default_conf_level()))
  expect_identical(formals(sieve_cv_fit_row)$rmsea_conf_level, quote(sieve_default_rmsea_conf_level()))
  # The option: a level strictly between 0 and 1 is taken, anything else
  # leaves 0.90 in force. A value that is set but is not a level is named in
  # a warning the first time it is read in the session, and not again.
  withr::with_options(list(sieve.rmsea_conf_level = 0.95), {
    expect_identical(sieve_default_rmsea_conf_level(), 0.95)
    expect_identical(sieve_default_conf_level(), 0.95)
  })
  withr::with_options(list(sieve.rmsea_conf_level = 0.99), expect_identical(sieve_default_rmsea_conf_level(), 0.99))
  rm(list = ls(sieve_rmsea_level_warned, all.names = TRUE), envir = sieve_rmsea_level_warned)
  withr::defer(rm(list = ls(sieve_rmsea_level_warned, all.names = TRUE), envir = sieve_rmsea_level_warned))
  for (bad in list(1, 0, 1.5, NA, "ninety", c(0.9, 0.95))) {
    withr::with_options(list(sieve.rmsea_conf_level = bad), {
      expect_warning(level <- sieve_default_rmsea_conf_level(),
        "is not used: the level must be one number strictly between 0 and 1", fixed = TRUE,
        class = "sieve_rmsea_level_warning")
      expect_identical(level, 0.90)
      expect_no_warning(expect_identical(sieve_default_rmsea_conf_level(), 0.90))
    })
  }
  withr::with_options(list(sieve.rmsea_conf_level = NULL),
    expect_no_warning(expect_identical(sieve_default_rmsea_conf_level(), 0.90)))
  expect_null(getOption("sieve.rmsea_conf_level"))
})

test_that("a mistyped RMSEA level is not used, and says so once in the session with the rule", {
  rm(list = ls(sieve_rmsea_level_warned, all.names = TRUE), envir = sieve_rmsea_level_warned)
  withr::defer(rm(list = ls(sieve_rmsea_level_warned, all.names = TRUE), envir = sieve_rmsea_level_warned))
  withr::with_options(list(sieve.rmsea_conf_level = 95), {
    expect_warning(level <- sieve_default_rmsea_conf_level(), paste0(
      "options(sieve.rmsea_conf_level = 95) is not used: the level must be one number strictly ",
      "between 0 and 1 (0.95 for a 95% interval). RMSEA intervals are at 0.90."
    ), fixed = TRUE, class = "sieve_rmsea_level_warning")
    expect_identical(level, 0.90)
    # Once in the session: a fit read with the same option says nothing more.
    X <- rl_items(FALSE)
    fit <- sieve_fit_cfa(X, rl_meta(X), estimator = "ml")
    expect_no_warning(m <- sieve_fit_measures(fit))
    expect_identical(sieve_fit_ci_level(m), 0.90)
  })
  # Another mistyped value is named in its own warning.
  withr::with_options(list(sieve.rmsea_conf_level = "0.95%"), expect_warning(sieve_default_rmsea_conf_level(),
    "options(sieve.rmsea_conf_level = \"0.95%\") is not used", fixed = TRUE))
  # Both default levels are exported, on one help page that carries the
  # policy and the rule.
  root <- compat_source_root()
  exports <- if (is.null(root)) getNamespaceExports("SIEVER") else {
    ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
    sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
  }
  expect_true(all(c("sieve_default_conf_level", "sieve_default_rmsea_conf_level") %in% exports))
  rd <- rl_rd_text("sieve_default_conf_level")
  expect_match(rd, "\\alias{sieve_default_rmsea_conf_level}", fixed = TRUE)
  expect_match(rd, "options(sieve.rmsea_conf_level = 0.95)", fixed = TRUE)
  expect_match(rd, "one number strictly between 0 and 1", fixed = TRUE)
  expect_match(rd, "MacCallum", fixed = TRUE)
})

test_that("every family's RMSEA interval is at the level asked for and equals lavaan's at that level", {
  fits <- rl_fits()
  expect_length(fits, 8L)
  columns <- sieve_fit_columns()
  checked <- 0L
  for (nm in names(fits)) {
    fit <- fits[[nm]]
    expect_true(sieve_has_solution(sieve_status_of(fit)), info = nm)
    for (level in c(0.90, 0.95)) {
      x <- sieve_fit_families(fit, conf_level = level)
      expect_identical(x$ci_level, level, info = nm)
      fm <- lavaan::fitMeasures(fit, fm.args = list(rmsea.ci.level = level))
      for (f in c("standard", "scaled", "robust")) {
        cols <- columns[[f]][c("rmsea_lo", "rmsea_hi")]
        if (!all(cols %in% names(fm))) next
        got <- x$families[[f]]$values[c("rmsea_lo", "rmsea_hi")]
        ok <- is.finite(got)
        expect_identical(is.finite(unname(fm[cols])), unname(ok), info = paste(nm, f, level))
        if (any(ok)) {
          expect_lt(rl_gap(got[ok], fm[cols][ok]), 1e-10)
          checked <- checked + 1L
        }
      }
    }
    # The tests of close and not close fit do not depend on the level.
    p_default <- lavaan::fitMeasures(fit)
    p_95 <- lavaan::fitMeasures(fit, fm.args = list(rmsea.ci.level = 0.95))
    close <- grep("^rmsea[.](notclose[.])?pvalue", names(p_default), value = TRUE)
    expect_true(length(close) >= 2L, info = nm)
    expect_identical(unname(p_95[close]), unname(p_default[close]), info = nm)
  }
  expect_gt(checked, 30L)
  # The default is the RMSEA level, 0.90 (DMAR's and lavaan's), not the 0.95
  # of the other intervals; the option moves it.
  expect_identical(sieve_fit_families(fits[["mlr complete"]])$ci_level, 0.90)
  expect_identical(sieve_fit_compare(fits[1:2])$conf_level, 0.90)
  withr::with_options(list(sieve.rmsea_conf_level = 0.95), {
    expect_identical(sieve_fit_families(fits[["mlr complete"]])$ci_level, 0.95)
    expect_identical(sieve_fit_compare(fits[1:2])$conf_level, 0.95)
  })
})

test_that("the interval shown equals lavaan's and DMAR::sem_fit_measures() for the version shown, to 1e-10", {
  fits <- rl_fits()
  shown_families <- character(0)
  for (nm in names(fits)) {
    fit <- fits[[nm]]
    for (level in c(0.90, 0.95)) {
      m <- sieve_fit_measures(fit, conf_level = level)
      family <- sieve_fit_family(m, "index")
      shown_families <- c(shown_families, family)
      expect_identical(sieve_fit_ci_level(m), level, info = nm)
      cols <- sieve_fit_columns()[[family]][c("rmsea_lo", "rmsea_hi")]
      fm <- lavaan::fitMeasures(fit, fm.args = list(rmsea.ci.level = level))
      expect_lt(rl_gap(m[c("rmsea_lo", "rmsea_hi")], fm[cols]), 1e-10)
      # DMAR's standard and robust versions are lavaan's; the version the
      # page shows is one of them for every fit here.
      expect_true(family %in% c("standard", "robust"), info = nm)
      expect_lt(rl_gap(m[c("rmsea_lo", "rmsea_hi")], rl_dmar_limits(fit, level, family)), 1e-10)
      expect_true(m[["rmsea_lo"]] <= m[["rmsea"]] + 1e-12 && m[["rmsea"]] <= m[["rmsea_hi"]] + 1e-12, info = nm)
      # Every label names the level it was computed at.
      pct <- rl_pct(level)
      expect_identical(sieve_fit_label(m, "rmsea_lo"), sprintf("RMSEA %s CI lower (%s)", pct, family))
      expect_identical(sieve_fit_label(m, "rmsea_hi"), sprintf("RMSEA %s CI upper (%s)", pct, family))
      tab <- sieve_fit_family_table(m)
      expect_true(all(sprintf("RMSEA %s CI %s", pct, c("lower", "upper")) %in% tab$measure))
    }
    # The default interval is the 90% one; DMAR's default gives the same
    # limits; the 95% interval contains it around the same estimate.
    m_default <- sieve_fit_measures(fit)
    m90 <- sieve_fit_measures(fit, conf_level = 0.90)
    m95 <- sieve_fit_measures(fit, conf_level = 0.95)
    expect_identical(m_default, m90)
    expect_lt(rl_gap(m_default[c("rmsea_lo", "rmsea_hi")],
      rl_dmar_limits(fit, eval(formals(DMAR::sem_fit_measures)$conf_level), sieve_fit_family(m_default, "index"))), 1e-10)
    expect_identical(m95[["rmsea"]], m90[["rmsea"]])
    expect_lte(m95[["rmsea_lo"]], m90[["rmsea_lo"]])
    expect_gte(m95[["rmsea_hi"]], m90[["rmsea_hi"]])
  }
  # ML on complete data shows the standard family; FIML, MLR and WLSMV the
  # robust one.
  expect_true(all(c("standard", "robust") %in% shown_families))
})

test_that("DMAR's scaled interval differs from lavaan's by design under MLR, and SIEVER's scaled column is lavaan's", {
  # DMAR puts the scaled statistic in the usual formulas (EQS and Mplus);
  # lavaan 0.7-3 refers the unscaled statistic to c times df for a
  # Yuan-Bentler or Satorra-Bentler statistic, the same point estimate and
  # another interval (DMAR's sem_fit_measures() help). SIEVER never shows
  # the scaled family under MLR when the robust one is reported, and the
  # families table holds lavaan's scaled values.
  fit <- rl_fits()[["mlr complete"]]
  x <- sieve_fit_families(fit)
  fm <- lavaan::fitMeasures(fit, fm.args = list(rmsea.ci.level = 0.90))
  expect_lt(rl_gap(x$families$scaled$values[c("rmsea_lo", "rmsea_hi")], fm[c("rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")]), 1e-12)
  dm <- rl_dmar_limits(fit, 0.90, "scaled")
  expect_gt(rl_gap(dm, fm[c("rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")]), 1e-5)
  expect_identical(sieve_fit_family(sieve_fit_select(x), "index"), "robust")
})

test_that("a page reuses measures at the RMSEA level and reads them again at another, and a level input is read safely", {
  fit <- rl_fits()[["mlr complete"]]
  m90 <- sieve_fit_measures(fit)
  expect_identical(sieve_fit_ci_level(m90), 0.90)
  expect_identical(sieve_fit_measures_at(m90, fit), m90)
  expect_identical(sieve_fit_measures_at(m90, fit, 0.90), m90)
  m95 <- sieve_fit_measures_at(m90, fit, 0.95)
  expect_identical(sieve_fit_ci_level(m95), 0.95)
  expect_lt(rl_gap(m95[c("rmsea_lo", "rmsea_hi")],
    lavaan::fitMeasures(fit, c("rmsea.ci.lower.robust", "rmsea.ci.upper.robust"), fm.args = list(rmsea.ci.level = 0.95))), 1e-12)
  # Measures read at another level are read again at the RMSEA level, so
  # the page never shows a level other than the one it states.
  expect_identical(sieve_fit_ci_level(sieve_fit_measures_at(m95, fit)), 0.90)
  expect_identical(sieve_fit_ci_level(sieve_fit_measures_at(NULL, fit, 0.99)), 0.99)
  expect_identical(sieve_valid_conf_level(0.9), 0.9)
  expect_identical(sieve_valid_conf_level(NA), 0.95)
  expect_identical(sieve_valid_conf_level(NA, sieve_default_rmsea_conf_level()), 0.90)
  expect_identical(sieve_valid_conf_level(1.5), 0.95)
  expect_identical(sieve_valid_conf_level(NULL), 0.95)
  expect_error(sieve_fit_families(fit, conf_level = 1), "strictly between 0 and 1")
  expect_identical(sieve_ci_percent(c(0.95, 0.9, 0.975, NA)), c("95%", "90%", "97.5%", ""))
  # Measures that do not say their level are labeled without one, never 90%.
  v <- c(rmsea = .05, rmsea_lo = .03, rmsea_hi = .07)
  attr(v, "family") <- "robust"
  expect_identical(sieve_fit_label(v, "rmsea_lo"), "RMSEA CI lower (robust)")
})

test_that("the CFA page shows, records and plans the RMSEA interval at 90%, and names 95% for the other intervals", {
  html <- rl_text(htmltools::renderTags(mod_cfa_ui("cfa"))$html)
  expect_match(html, "RMSEA (90% CI)", fixed = TRUE)
  expect_false(grepl("RMSEA (95% CI)", html, fixed = TRUE))
  expect_match(html, "read with its 90% confidence interval", fixed = TRUE)
  expect_match(html, "the two limits of a 90% interval settle the one-sided tests of close and of not close fit at .05", fixed = TRUE)
  expect_match(html, "the page's other intervals are at 95%", fixed = TRUE)
  # The option moves the page's label with it.
  html95 <- withr::with_options(list(sieve.rmsea_conf_level = 0.95), rl_text(htmltools::renderTags(mod_cfa_ui("cfa95"))$html))
  expect_match(html95, "RMSEA (95% CI)", fixed = TRUE)
  expect_match(html95, "read with its 95% confidence interval", fixed = TRUE)
  st <- siever_test_study(sieve_hs9_fixture())
  steps <- function(pattern) {
    log <- shiny::isolate(st$codelog) %||% character(0)
    grep(pattern, log, fixed = TRUE, value = TRUE)
  }
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      scope = "construct", structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE, estimator = "mlr"
    )
    session$setInputs(fit_go = 1)
    run <- cfa_run()
    m <- fm()
    expect_identical(sieve_fit_ci_level(m), 0.90)
    expect_identical(sieve_fit_family(m, "index"), "robust")
    fm90 <- lavaan::fitMeasures(run$fit, fm.args = list(rmsea.ci.level = 0.90))
    expect_lt(rl_gap(m[c("rmsea_lo", "rmsea_hi")], fm90[c("rmsea.ci.lower.robust", "rmsea.ci.upper.robust")]), 1e-10)
    expect_lt(rl_gap(m[c("rmsea_lo", "rmsea_hi")], rl_dmar_limits(run$fit, 0.90, "robust")), 1e-10)
    # The line above the diagram, the value box and the tables name the level.
    line <- rl_text(output$overview_fit$html)
    expect_match(line, sprintf("RMSEA (robust) = %.3f, 90%% CI [%.3f, %.3f]", m[["rmsea"]], m[["rmsea_lo"]], m[["rmsea_hi"]]), fixed = TRUE)
    expect_false(grepl("95% CI", line, fixed = TRUE))
    box <- rl_text(output$vb_rmsea$html)
    expect_match(box, sprintf("[%.3f, %.3f]", m[["rmsea_lo"]], m[["rmsea_hi"]]), fixed = TRUE)
    # The tables are built from these rows (the browser receives them from
    # the server): every interval row names its level.
    expect_true("RMSEA 90% CI lower (robust)" %in% sieve_fit_table(m)$Statistic)
    expect_true("RMSEA 90% CI upper" %in% sieve_fit_family_table(m)$measure)
    # The recorded R: the model, then lavaan's summary and fitMeasures() at
    # rmsea.ci.level = 0.9, which give the interval on screen.
    code <- sieve_cfa_code(run$spec)
    expect_true(any(grepl("rmsea.ci.level = 0.9)", code, fixed = TRUE)))
    expect_true(any(grepl("rmsea.ci.level = 0.9)", steps("lavaan::summary(cfa_fit"), fixed = TRUE)))
    readout <- rl_text(output$fit_code)
    expect_match(readout, "lavaan::fitMeasures(cfa_fit, fm.args = list(rmsea.ci.level = 0.9))", fixed = TRUE)
    expect_match(readout, "the RMSEA interval is at 90%", fixed = TRUE)
    env <- new.env(parent = globalenv())
    env$items <- shiny::isolate(st$items)
    env$d <- shiny::isolate(st$raw)
    utils::capture.output(suppressWarnings(eval(parse(text = code), env)))
    replay <- eval(str2lang("lavaan::fitMeasures(cfa_fit, fm.args = list(rmsea.ci.level = 0.9))"), env)
    expect_lt(rl_gap(m[c("rmsea_lo", "rmsea_hi")], replay[c("rmsea.ci.lower.robust", "rmsea.ci.upper.robust")]), 1e-10)
    printed <- utils::capture.output(lavaan::summary(env$cfa_fit,
      fit.measures = list(fit.measures = TRUE, rmsea.ci.level = 0.9)))
    expect_true(any(grepl("90 Percent confidence interval - lower", printed, fixed = TRUE)))
    # The planner plans the interval at the level the Fit sub-tab shows,
    # DMAR's default for ss_aipe_rmsea(), and passes it explicitly, so the
    # recorded call states the level.
    session$setInputs(plan_rmsea = 0.0525, plan_df = 24, plan_rmsea_width = 0.0375)
    shown <- rl_text(output$plan_rmsea_result$html)
    n_app <- as.integer(sub(".*Required N: ([0-9]+)\\..*", "\\1", shown))
    ref95 <- DMAR::ss_aipe_rmsea(true_rmsea = 0.0525, df = 24, width = 0.0375, conf_level = 0.95)
    ref90 <- DMAR::ss_aipe_rmsea(true_rmsea = 0.0525, df = 24, width = 0.0375, conf_level = 0.90)
    expect_identical(n_app, as.integer(ref90$value[ref90$term == "necessary_N"]))
    expect_lt(n_app, as.integer(ref95$value[ref95$term == "necessary_N"]))
    expect_match(shown, "an expected 90% confidence interval no wider than 0.037", fixed = TRUE)
    expect_match(shown, "The 90% level is the level of the RMSEA interval the Fit sub-tab reports", fixed = TRUE)
    expect_false(grepl("95%", shown, fixed = TRUE))
    session$setInputs(plan_rmsea_log = 1)
    got <- steps("ss_aipe_rmsea(")
    expect_length(got, 1L)
    expect_match(got, "conf_level = 0.9)", fixed = TRUE)
    replayed <- eval(str2lang(got))
    expect_identical(as.integer(replayed$value[replayed$term == "necessary_N"]), n_app)
    expect_true(any(grepl("n for an RMSEA 90% CI of full width", shiny::isolate(st$codelog), fixed = TRUE)))
  })
})

test_that("the compared structures carry the RMSEA level into the table, the figure and the recorded R", {
  X <- rl_items(FALSE)
  res <- sieve_compare_structures(rl_meta(X), X, estimator = "mlr")
  expect_identical(res$conf_level, 0.90)
  tab <- sieve_structures_table(res)
  expect_identical(attr(tab, "conf_level"), 0.90)
  fit <- res$structures$correlated$run$fit
  fm <- lavaan::fitMeasures(fit, fm.args = list(rmsea.ci.level = 0.90))
  row <- tab[tab$id == "correlated", ]
  expect_lt(rl_gap(c(row$rmsea_lo, row$rmsea_hi), fm[c("rmsea.ci.lower.robust", "rmsea.ci.upper.robust")]), 1e-10)
  code <- sieve_structures_code(res)
  expect_true(any(grepl("fm.args = list(rmsea.ci.level = 0.9)", code, fixed = TRUE)))
  expect_true(any(grepl("The RMSEA interval at 90%", code, fixed = TRUE)))
  # Reading the plot's labels can open the default device; a null device,
  # closed when the test ends, leaves no Rplots.pdf and no open device.
  withr::local_pdf(NULL)
  p <- sieve_structures_plot(tab)
  expect_true(any(grepl("RMSEA (robust, with its 90% interval)", levels(p$data$index), fixed = TRUE)))
  expect_match(p$labels$subtitle, "90% confidence", fixed = TRUE)
  res95 <- sieve_compare_structures(rl_meta(X), X, estimator = "mlr", conf_level = 0.95)
  expect_identical(attr(sieve_structures_table(res95), "conf_level"), 0.95)
})

test_that("the model fit by k and its recorded R are at the RMSEA level", {
  X <- rl_items(TRUE)
  byk <- sieve_efa_fit_by_k(X, estimator = "MLR", ks = 1:3, missing = "fiml")
  expect_identical(attr(byk, "conf_level"), 0.90)
  expect_identical(attr(byk, "family"), "robust")
  for (k in 1:3) {
    f <- lavaan::efa(data = X, nfactors = k, rotation = "none", estimator = "MLR", missing = "ml")
    if (inherits(f, "efaList")) f <- f[[1]]
    fm <- lavaan::fitMeasures(f, c("rmsea.ci.lower.robust", "rmsea.ci.upper.robust"), fm.args = list(rmsea.ci.level = 0.90))
    expect_lt(rl_gap(c(byk$RMSEA.lo[k], byk$RMSEA.hi[k]), fm), 1e-10)
  }
  basis <- list(requested = "pearson", used = "pearson", types = rep("continuous", ncol(X)))
  spec <- list(estimator = "MLR", ordered = NULL, missing = "fiml", label = "MLR with FIML")
  fit <- sieve_retention_fit(basis, max_k = 3L, data = X, spec = spec, byk = byk)
  expect_identical(fit$conf_level, 0.90)
  code <- sieve_fit_lavaan_code(fit)
  expect_true(any(grepl("fm.args = list(rmsea.ci.level = 0.9)", code, fixed = TRUE)))
  expect_true(any(grepl("The RMSEA interval is at 90%", code, fixed = TRUE)))
  env <- new.env(parent = globalenv())
  env$active_items <- X
  suppressWarnings(eval(parse(text = code), env))
  expect_lt(rl_gap(env$fit_by_k$rmsea_lo, fit$table$rmsea_lo), 1e-10)
  expect_lt(rl_gap(env$fit_by_k$rmsea_hi, fit$table$rmsea_hi), 1e-10)
  # 95% asked for is 95% given, and wider.
  b95 <- sieve_efa_fit_by_k(X, estimator = "MLR", ks = 2, missing = "fiml", conf_level = 0.95)
  expect_identical(attr(b95, "conf_level"), 0.95)
  expect_gt(b95$RMSEA.hi, byk$RMSEA.hi[2])
})

test_that("the EFA page asks DMAR for the RMSEA interval at the RMSEA level, beside the coefficients' 95%, with no second fit", {
  X <- rl_items(FALSE)
  rt <- sieve_efa_dmar_route("ml", list(estimator = "ML", missing = "listwise"))
  call <- sieve_efa_dmar_call(rt, 3L, "oblimin", data = X, seed = 7L)
  expect_null(call$rmsea)
  expect_identical(call$args$conf_level, 0.95)
  # The level is always passed (DMAR 1.1.0 takes it), so a counting mock
  # that wraps DMAR's function in `function(...)` receives it too.
  expect_identical(call$args$rmsea_conf_level, 0.90)
  expect_true(any(grepl("rmsea_conf_level = 0.9", sieve_efa_dmar_code(call), fixed = TRUE)))
  withr::with_options(list(sieve.rmsea_conf_level = 0.95),
    expect_identical(sieve_efa_dmar_call(rt, 3L, "oblimin", data = X, seed = 7L)$args$rmsea_conf_level, 0.95))
  expect_identical(sieve_efa_call_rmsea_level(call), 0.90)
  expect_false(exists("sieve_dmar_rmsea_level_argument", mode = "function"))
  expect_false(exists("sieve_efa_rmsea_level", mode = "function"))
  expect_false("rmsea" %in% names(formals(sieve_efa_dmar_wait)))
  expect_identical(sieve_efa_count_call(rt, 3L, data = X)$args$conf_level, 0.90)
  record <- sieve_efa_dmar_run(call)
  expect_true(record$ok)
  expect_null(record$rmsea)
  f <- sieve_efa_dmar_solution(record$value)
  level <- sieve_efa_fit_level(f)
  expect_identical(level, 0.90)
  expect_identical(f$conf_level, 0.95)
  # DMAR's fit measures of the unrotated model are lavaan's at that level
  # for the same model (ML on the complete cases).
  e <- lavaan::efa(data = X, nfactors = 3, rotation = "none", estimator = "ML")
  if (inherits(e, "efaList")) e <- e[[1]]
  fl <- lavaan::fitMeasures(e, c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper"), fm.args = list(rmsea.ci.level = level))
  got <- c(sieve_efa_fit_value(f$fit_measures, "rmsea_ci_lower"), sieve_efa_fit_value(f$fit_measures, "rmsea_ci_upper"))
  expect_lt(rl_gap(got, fl[c("rmsea.ci.lower", "rmsea.ci.upper")]), 1e-6)
  expect_lt(rl_gap(got, rl_dmar_limits(e, level, "standard")), 1e-6)
  tables <- sieve_efa_numeric_tables(f, rl_meta(X))
  expect_null(tables$fit_measures_rmsea)
})

test_that("the one-factor RMSEA on the Reliability page is at the RMSEA level whatever the page's level, and the recorded R gives it", {
  X <- rl_items(TRUE)[, 1:3 + 3L]
  X$x_extra <- rl_items(TRUE)[, 1L]
  items <- names(X)
  for (level in c(0.90, 0.95)) {
    sp <- sieve_reliability_spec(items, rep("continuous", 4L), "Textual", missing = "fiml",
      ci_method = "ml", denominator = "model_implied", conf_level = level)
    H <- sieve_reliability_H(X, sp)
    fs <- H$fit_summary
    expect_identical(fs$level, 0.90)
    expect_true(is.finite(fs$lower) && is.finite(fs$upper))
    env <- new.env(parent = globalenv())
    env$items <- X
    env$d <- X
    code <- c(sieve_reliability_H_helper_code(), sieve_reliability_H_code(sp))
    expect_true(any(grepl("rmsea.ci.level = 0.9)", code, fixed = TRUE)))
    suppressWarnings(eval(parse(text = code), env))
    sfx <- switch(fs$family, robust = ".robust", scaled = ".scaled", "")
    expect_lt(rl_gap(c(fs$lower, fs$upper), env$h_rmsea[paste0(c("rmsea.ci.lower", "rmsea.ci.upper"), sfx)]), 1e-10)
    expect_match(sieve_reliability_rmsea_text(fs), ", 90% CI [", fixed = TRUE)
  }
  # The option moves it, and the recorded R with it.
  withr::with_options(list(sieve.rmsea_conf_level = 0.95), {
    sp <- sieve_reliability_spec(items, rep("continuous", 4L), "Textual", missing = "fiml",
      ci_method = "ml", denominator = "model_implied", conf_level = 0.95)
    fs <- sieve_reliability_H(X, sp)$fit_summary
    expect_identical(fs$level, 0.95)
    expect_true(any(grepl("rmsea.ci.level = 0.95)", sieve_reliability_H_code(sp), fixed = TRUE)))
  })
})

test_that("reports, saved reports, the assistant, the optimizer and the cross-validation table name the RMSEA level", {
  X <- rl_items(FALSE)
  meta <- rl_meta(X)
  fit <- sieve_fit_cfa(X, meta, estimator = "mlr")
  m <- sieve_fit_measures(fit)
  expect_identical(sieve_fit_ci_level(m), 0.90)
  # The methods draft, whatever the Reliability tab's level (the level of
  # its omega intervals).
  draft <- sieve_methods_draft(X, meta, estimator = "mlr")
  expect_match(draft, sprintf("robust RMSEA = %.3f, 90%% CI [%.3f, %.3f]", m[["rmsea"]], m[["rmsea_lo"]], m[["rmsea_hi"]]), fixed = TRUE)
  draft90 <- sieve_methods_draft(X, meta, estimator = "mlr", reliability = list(conf_level = 0.90))
  expect_match(draft90, sprintf("robust RMSEA = %.3f, 90%% CI [%.3f, %.3f]", m[["rmsea"]], m[["rmsea_lo"]], m[["rmsea_hi"]]), fixed = TRUE)
  m95 <- sieve_fit_measures(fit, conf_level = 0.95)
  draft95 <- withr::with_options(list(sieve.rmsea_conf_level = 0.95), sieve_methods_draft(X, meta, estimator = "mlr"))
  expect_match(draft95, sprintf("robust RMSEA = %.3f, 95%% CI [%.3f, %.3f]", m95[["rmsea"]], m95[["rmsea_lo"]], m95[["rmsea_hi"]]), fixed = TRUE)
  # The assistant's fit table.
  tabs <- sieve_assistant_result_tables(list(task = "cfa", estimates = m, parameters = NULL))
  expect_true("RMSEA, lower 90% confidence limit" %in% tabs[["CFA fit"]]$measure)
  # The optimizer header and the cross-validation row.
  r <- list(fit_family = "robust", fit_conf_level = 0.90, table = data.frame())
  expect_match(rl_text(sieve_candidate_header(r)), "RMSEA 90% CI (robust)", fixed = TRUE)
  row <- sieve_cv_fit_row("Calibration (training)", fit)
  fm90 <- lavaan::fitMeasures(fit, c("rmsea.ci.lower.robust", "rmsea.ci.upper.robust"), fm.args = list(rmsea.ci.level = 0.90))
  expect_lt(rl_gap(c(row$RMSEA.lo, row$RMSEA.hi), fm90), 1e-10)
  ecvi <- suppressWarnings(DMAR::ecvi(fit = fit, conf_level = 0.95))
  expect_equal(row$ECVI.lo, ecvi$value[ecvi$term == "ci_lower"], tolerance = 1e-10)
  row95 <- sieve_cv_fit_row("Calibration (training)", fit, rmsea_conf_level = 0.95)
  expect_lt(row95$RMSEA.lo, row$RMSEA.lo)
  expect_identical(row95$ECVI.lo, row$ECVI.lo)
  # A stored record carries the level it was computed at (90% or 95%), and
  # each is labeled with its own level.
  m_old <- m
  attr(m_old, "families")$ci_level <- 0.95
  expect_identical(sieve_fit_label(m_old, "rmsea_hi"), "RMSEA 95% CI upper (robust)")
})

test_that("the Network page's interval: psychonetrics' 90% interval is reproduced by DMAR at the RMSEA level, shown and recorded", {
  skip_if_not_installed("psychonetrics")
  skip_if_not_installed("qgraph")
  skip_if_not_installed("igraph")
  cases <- list(
    ml = list(X = rl_items(FALSE), type = "continuous", estimator = "ml", family = "standard", ci = "standard"),
    mlr = list(X = rl_items(FALSE), type = "continuous", estimator = "mlr", family = "robust", ci = "robust"),
    fiml = list(X = rl_items(TRUE), type = "continuous", estimator = "mlr", family = "standard", ci = "standard"),
    dwls = list(X = rl_ordered(rl_items(FALSE)), type = "ordered", estimator = "wlsmv", family = "scaled", ci = "standard")
  )
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    r <- suppressWarnings(sieve_network_estimate(cs$X, rl_meta(cs$X, cs$type), kind = "network_ggm", estimator = cs$estimator))
    expect_null(r$error, info = nm)
    m <- sieve_network_selected(r$fit)
    expect_identical(attr(m, "family"), cs$family, info = nm)
    expect_identical(attr(m, "rmsea_ci_family"), cs$ci, info = nm)
    expect_identical(sieve_fit_ci_level(m), 0.90, info = nm)
    # At 0.90 the DMAR route gives psychonetrics' own limits, to the
    # tolerance of psychonetrics' root finder (uniroot at its default).
    sfx <- if (identical(cs$ci, "robust")) ".robust" else ""
    own <- r$fit$all[paste0(c("rmsea.ci.lower", "rmsea.ci.upper"), sfx)]
    expect_lt(rl_gap(m[c("rmsea_lo", "rmsea_hi")], own), 1e-7)
    # The same from DMAR's summary route on psychonetrics' statistic.
    stat <- if (identical(cs$ci, "robust")) "chisq.scaled" else "chisq"
    t90 <- DMAR::sem_fit_measures(chisq = r$fit$all[[stat]], df = r$fit$all[["df"]], N = sieve_network_nobs(r$model),
      conf_level = 0.90, sample_size_convention = "N")
    v90 <- stats::setNames(t90$value, t90$term)
    mult <- if (identical(cs$ci, "robust")) sqrt(r$fit$all[["chisq.scaling.factor.sb"]]) else 1
    expect_lt(rl_gap(m[c("rmsea_lo", "rmsea_hi")], mult * v90[c("rmsea_ci_lower", "rmsea_ci_upper")]), 1e-12)
    # At 0.95 the interval is wider around the same estimate.
    at95 <- sieve_network_rmsea_interval(r$fit$all, cs$ci, N = sieve_network_nobs(r$model),
      groups = sieve_network_groups(r$model), conf_level = 0.95)
    expect_true(at95$ok, info = nm)
    expect_true(at95$lower <= m[["rmsea_lo"]] + 1e-12 && at95$upper >= m[["rmsea_hi"]] - 1e-12, info = nm)
    # The labels and the method sentence name the level.
    expect_identical(sieve_fit_label(m, "rmsea_lo"), sprintf("RMSEA 90%% CI lower (%s)", cs$ci))
    expect_match(sieve_network_method_text(r), "90% CI", fixed = TRUE)
    expect_false(grepl("95% CI", sieve_network_method_text(r), fixed = TRUE))
    # The recorded R gives the interval on screen.
    code <- r$code
    expect_true(any(grepl("DMAR::sem_fit_measures(chisq = ", code, fixed = TRUE)))
    expect_true(any(grepl("conf_level = 0.9,", code, fixed = TRUE)))
    env <- new.env(parent = globalenv())
    env$items <- cs$X
    suppressWarnings(utils::capture.output(eval(parse(text = code), env)))
    limits <- if (identical(cs$ci, "robust")) {
      env$net_ggm_c * env$net_ggm_rmsea$value[env$net_ggm_rmsea$term %in% c("rmsea_ci_lower", "rmsea_ci_upper")]
    } else {
      env$net_ggm_rmsea$value[env$net_ggm_rmsea$term %in% c("rmsea_ci_lower", "rmsea_ci_upper")]
    }
    expect_lt(rl_gap(limits, m[c("rmsea_lo", "rmsea_hi")]), 1e-10)
  }
  # The option moves the page's interval and its record.
  r95 <- withr::with_options(list(sieve.rmsea_conf_level = 0.95),
    suppressWarnings(sieve_network_estimate(rl_items(FALSE), rl_meta(rl_items(FALSE)), kind = "network_ggm", estimator = "ml")))
  m95 <- sieve_network_selected(r95$fit)
  expect_identical(sieve_fit_ci_level(m95), 0.95)
  expect_identical(sieve_fit_label(m95, "rmsea_lo"), "RMSEA 95% CI lower (standard)")
  expect_true(any(grepl("conf_level = 0.95,", r95$code, fixed = TRUE)))
  # A stored result that records no level holds psychonetrics' own 90%
  # interval, and is labeled so.
  old <- list(selected = c(rmsea = 0.05, rmsea_lo = 0.03, rmsea_hi = 0.07))
  attr(old$selected, "family") <- "standard"
  expect_identical(sieve_fit_label(sieve_network_selected(old), "rmsea_lo"), "RMSEA 90% CI lower (standard)")
})

test_that("a page's confidence level input sets its other intervals and leaves the RMSEA interval at the RMSEA level", {
  # The Validity page: its Confidence level sets the HTMT and validity
  # intervals; the measurement model's RMSEA interval in the method line
  # stays at 90% when the input moves.
  st <- siever_test_study(sieve_hs9_fixture())
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(criteria = character(0), htmt_on = FALSE, marker = "", conf_level = 0.95,
      estimator = "mlr")
    session$setInputs(fit_go = 1)
    run <- demand$result()
    expect_true(sieve_has_solution(run))
    fm <- lavaan::fitMeasures(run$fit, c("rmsea.ci.lower.robust", "rmsea.ci.upper.robust"),
      fm.args = list(rmsea.ci.level = 0.90))
    for (level in c(0.90, 0.95)) {
      session$setInputs(conf_level = level)
      line <- rl_text(output$method)
      expect_match(line, sprintf("90%% CI [%.3f, %.3f]", fm[[1]], fm[[2]]), fixed = TRUE)
      expect_false(grepl("95% CI", line, fixed = TRUE))
    }
  })
  # The Bifactor page: its Confidence Level for Bifactor Indices sets the
  # indices' intervals; the headline fit's RMSEA interval stays at 90%.
  st <- siever_test_study(sieve_hs_fixture())
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    session$setInputs(conf_level = 0.90)
    session$setInputs(fit = 1)
    r <- res()
    expect_identical(r$status$code, "ok")
    expect_identical(sieve_fit_ci_level(r$measures), 0.90)
    m <- sieve_fit_measures(r$fit, conf_level = 0.90)
    for (level in c(0.90, 0.95)) {
      session$setInputs(conf_level = level)
      line <- rl_text(output$method)
      expect_match(line, sprintf("90%% CI [%.3f, %.3f]", m[["rmsea_lo"]], m[["rmsea_hi"]]), fixed = TRUE)
    }
  })
  # The method line every model page shows names the level of the measures
  # it is given.
  X <- rl_items(FALSE)
  fit <- sieve_fit_cfa(X, rl_meta(X), estimator = "mlr")
  for (level in c(0.90, 0.95)) {
    m <- sieve_fit_measures(fit, conf_level = level)
    line <- sieve_method_text(fit, "Correlated-factors CFA", measures = m)
    expect_match(line, sprintf("RMSEA (robust) %.3f, %s CI [%.3f, %.3f]", m[["rmsea"]],
      rl_pct(level), m[["rmsea_lo"]], m[["rmsea_hi"]]), fixed = TRUE)
  }
})

test_that("the methods draft: omega intervals at the reliability level, every RMSEA interval at the RMSEA level, as its help says", {
  # The help says the draft's RMSEA interval is at the RMSEA level, not at
  # the draft's (the Reliability tab's) confidence level. Constructs
  # of four and five items, so each construct's one-factor model has degrees
  # of freedom and its RMSEA interval is reported (three items saturate it,
  # df = 0, and the draft then says the RMSEA is not interpreted).
  X <- rl_items(FALSE)
  meta <- rl_meta(X)
  meta$construct <- rep(c("First", "Second"), c(4L, 5L))
  groups <- split(meta$item, factor(meta$construct, levels = c("First", "Second")))
  model <- paste(sprintf("%s =~ %s", names(groups), vapply(groups, paste, "", collapse = " + ")), collapse = "\n")
  rs <- list(treatment = "study", scope = "construct", conf_level = 0.80)
  cols <- c("rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust")
  # lavaan's own limits, at the level asked for: the correlated-factors CFA
  # (ML with FIML, the study default for continuous items) and each
  # construct's one-factor model (MLR with FIML, the Reliability tab's
  # robust ML fit behind model implied omega).
  lav <- function(syntax, data, level, estimator = "ML") {
    fit <- lavaan::cfa(syntax, data = data, std.lv = TRUE, missing = "fiml", estimator = estimator)
    unname(lavaan::fitMeasures(fit, cols, fm.args = list(rmsea.ci.level = level)))
  }
  one <- function(g, level) {
    lav(sprintf("F =~ %s", paste(groups[[g]], collapse = " + ")), X[groups[[g]]], level, estimator = "MLR")
  }
  cfa_line <- function(v, pct) sprintf("robust RMSEA = %.3f, %s CI [%.3f, %.3f]", v[1], pct, v[2], v[3])
  one_line <- function(v, pct) sprintf("one-factor RMSEA %.3f, %s CI [%.3f, %.3f] (robust)", v[1], pct, v[2], v[3])
  # The level of every RMSEA interval the draft prints.
  rmsea_levels <- function(d) {
    found <- regmatches(d, gregexpr("RMSEA (= )?[0-9.]+, [0-9.]+% CI", d))[[1]]
    sub("^.*, ([0-9.]+%) CI$", "\\1", found)
  }
  draft <- sieve_methods_draft(X, meta, reliability = rs)
  expect_match(draft, cfa_line(lav(model, X, 0.90), "90%"), fixed = TRUE)
  for (g in names(groups)) expect_match(draft, one_line(one(g, 0.90), "90%"), fixed = TRUE)
  # The omega intervals are at the reliability level, 80%, one per construct,
  # and no RMSEA interval is.
  expect_length(gregexpr("Omega \\(model-implied denominator\\) = [0-9.]+, 80% CI \\[", draft)[[1]], 2L)
  expect_identical(rmsea_levels(draft), rep("90%", 3L))
  expect_match(draft, "[Interpret each index with its interval and the residual correlations; no cutoff makes a model acceptable.]",
    fixed = TRUE)
  expect_false(grepl("thresholds alone", draft, fixed = TRUE))
  # The option moves every RMSEA interval, the CFA's and the one-factor
  # models', to lavaan's 95% limits and leaves the omega intervals at 80%.
  draft95 <- withr::with_options(list(sieve.rmsea_conf_level = 0.95), sieve_methods_draft(X, meta, reliability = rs))
  expect_match(draft95, cfa_line(lav(model, X, 0.95), "95%"), fixed = TRUE)
  for (g in names(groups)) expect_match(draft95, one_line(one(g, 0.95), "95%"), fixed = TRUE)
  expect_length(gregexpr("Omega \\(model-implied denominator\\) = [0-9.]+, 80% CI \\[", draft95)[[1]], 2L)
  expect_identical(rmsea_levels(draft95), rep("95%", 3L))
  # The help says the same.
  rd <- rl_rd_text("sieve_methods_draft")
  expect_false(grepl("also the level of the RMSEA", rd, fixed = TRUE))
  expect_false(grepl("draft's confidence level", rd, fixed = TRUE))
  expect_match(rd, "\\section{Interval Levels}", fixed = TRUE)
  expect_match(rd, "options(sieve.rmsea_conf_level = )", fixed = TRUE)
  expect_match(rd, "it does not reach any RMSEA interval", fixed = TRUE)
  expect_match(rd, "\\link[=sieve_default_rmsea_conf_level]", fixed = TRUE)
})
