# Reliability, scores and validity: the target and interval of each
# coefficient, what the interface says about them, the recorded steps, and
# the bootstraps' requests and counts.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

scores_one_factor <- function(n, loadings, seed, prefix = "x") {
  X <- withr::with_seed(seed, {
    f <- stats::rnorm(n)
    sapply(loadings, function(l) l * f + sqrt(1 - l^2) * stats::rnorm(n))
  })
  X <- as.data.frame(X)
  names(X) <- paste0(prefix, seq_along(loadings))
  X
}

# A DMAR-like engine that returns the given point estimate and records every
# call, so the gate on the estimate can be tested without a data set whose
# one-factor model fits badly enough to push a coefficient above 1.
scores_fake_engine <- function(estimate, calls) {
  function(data, ci_method = "none", ...) {
    calls$methods <- c(calls$methods, ci_method)
    value <- data.frame(
      term = c("estimate", "se", "ci_lower", "ci_upper", "conf_level", "N"),
      value = c(estimate, NA, NA, NA, 0.95, nrow(stats::na.omit(data))),
      stringsAsFactors = FALSE
    )
    attr(value, "ci_method") <- ci_method
    value
  }
}

test_that("a coefficient above 1 is inadmissible and no bootstrap describes it", {
  X <- scores_one_factor(200, c(.7, .6, .5, .6), seed = 11)
  calls <- new.env()
  testthat::local_mocked_bindings(
    sieve_reliability_engine = function(fun) scores_fake_engine(1.0093, calls)
  )
  for (coef in c("ordered", "continuous")) {
    types <- rep(coef, ncol(X))
    point <- sieve_reliability_omega(X, sieve_reliability_spec(names(X), types,
      construct = "Fairness", ci_method = "none", denominator = "observed"
    ))
    expect_identical(point$status$code, "inadmissible")
    expect_match(point$status$message, "above 1, so it is not a reliability and is inadmissible", fixed = TRUE)
    expect_match(point$status$message, "implies more common variance than the observed composite has", fixed = TRUE)
    # The estimate is kept at full precision; it is labeled, not altered.
    expect_identical(point$estimate, 1.0093)
    # The table's notes say it.
    notes <- sieve_reliability_notes(list(omega = point, H = list(
      estimate = NA_real_, status = list(message = "not computed"), error = "not computed",
      uncertainty = list(status = "failed"), missing = list(reason = NULL)
    )))
    expect_true(any(grepl("is above 1", notes, fixed = TRUE)))
  }
  # SIEVER's counted bootstrap refuses before drawing a resample.
  calls$methods <- NULL
  plan <- sieve_reliability_boot_plan(X, sieve_reliability_spec(names(X), rep("ordered", 4),
    construct = "Fairness", ci_method = "percentile", B = 50, seed = 1
  ))
  expect_identical(plan$status$code, "unavailable")
  expect_match(plan$status$message, "The bootstrap was not run", fixed = TRUE)
  expect_identical(calls$methods, "none")
  # The engine's own bootstraps are refused for the same reason: the engine
  # is called once, for the point estimate, and never with a bootstrap.
  for (method in c("dmar_percentile", "dmar_bca")) {
    calls$methods <- NULL
    res <- sieve_reliability_omega(X, sieve_reliability_spec(names(X), rep("ordered", 4),
      construct = "Fairness", ci_method = method, B = 50, seed = 1
    ))
    expect_identical(res$status$code, "inadmissible")
    expect_identical(res$uncertainty$status, "unavailable")
    expect_match(res$uncertainty$reason, "above 1", fixed = TRUE)
    expect_identical(calls$methods, "none")
    expect_true(is.na(res$lower) && is.na(res$upper))
  }
  # An estimate inside [0, 1] is untouched by the check.
  expect_identical(sieve_reliability_outside(0.93), character(0))
  expect_identical(sieve_reliability_outside(1), character(0))
  expect_identical(sieve_reliability_outside(NA_real_), character(0))
})

test_that("one robust ML request gives one standard error type for omega and H", {
  X <- scores_one_factor(300, c(.8, .7, .6, .5), seed = 20261021)
  # Complete cases: the engine's robust interval for omega uses robust.sem,
  # and H uses it too, not MLR's Huber-White sandwich.
  spec <- sieve_reliability_spec(names(X), rep("continuous", 4),
    construct = "A", missing = "listwise", ci_method = "mlr", denominator = "model_implied"
  )
  H <- sieve_reliability_H(X, spec)
  expect_identical(H$uncertainty$se_type, "robust.sem")
  expect_match(sieve_reliability_interval("listwise", "mlr")$se_type, "robust.sem", fixed = TRUE)
  # The interval is the delta method on an independently specified fit
  # with that standard error.
  ref <- lavaan::cfa("f =~ x1 + x2 + x3 + x4", data = X, std.lv = TRUE, estimator = "ML", se = "robust.sem")
  l <- as.numeric(lavaan::lavInspect(ref, "std")$lambda)
  V <- lavaan::lavInspect(ref, "vcov.std.all")[paste0("f=~x", 1:4), paste0("f=~x", 1:4)]
  delta <- sieve_H_delta(l, V)
  # H is DMAR::reliability_H(), from DMAR's own fit of the
  # same model: the same standard error type, equal to its optimizer's
  # convergence (2e-10 here).
  expect_equal(H$se, delta$se, tolerance = 1e-8)
  expect_equal(c(H$lower, H$upper), c(delta$lower, delta$upper), tolerance = 1e-8)
  # The recorded R fits the same model.
  expect_true(any(grepl("se = \"robust.sem\"", H$code, fixed = TRUE)))
  # Under FIML both use the Huber-White sandwich (lavaan's MLR).
  Xm <- X
  Xm[1:20, 1] <- NA
  Hf <- sieve_reliability_H(Xm, sieve_reliability_spec(names(X), rep("continuous", 4),
    construct = "A", missing = "fiml", ci_method = "mlr", denominator = "model_implied"
  ))
  expect_identical(Hf$uncertainty$se_type, "robust.huber.white")
  expect_match(sieve_reliability_interval("fiml", "mlr")$se_type, "Huber-White", fixed = TRUE)
})

# A study of two continuous constructs of five items each, so the one-factor
# model of each has degrees of freedom and an RMSEA.
scores_state <- function(n = 300, seed = 20261021) {
  A <- scores_one_factor(n, c(.8, .7, .65, .6, .55), seed = seed, prefix = "a")
  B <- scores_one_factor(n, c(.75, .7, .6, .6, .5), seed = seed + 1L, prefix = "b")
  d <- cbind(A, B)
  fixture <- list(data = d, item_cols = names(d), theory_map = list(Alpha = names(A), Beta = names(B)),
    name = "Score fixture", labels = NULL, load_code = character())
  s <- siever_test_study(fixture, labels = FALSE, theory = FALSE, name = NULL,
    configure = function(s) s$roles <- NULL)
  s
}

test_that("robust omega leads, its bootstrap waits, and model implied omega with the one-factor RMSEA stands beside it", {
  # Explicit reduced-count lifecycle/timing mechanics; valid production counts are tested separately.
  siever_local_dmar_mechanics()
  # The controls' defaults.
  ui <- as.character(mod_reliab_ui("reliab"))
  # DMAR's denominator wording.
  expect_match(ui, "<option value=\"observed\" selected>Robust omega (observed denominator)</option>", fixed = TRUE)
  expect_match(ui, "<option value=\"recommended\" selected>", fixed = TRUE)
  # The function's defaults are the same target.
  sp <- sieve_reliability_spec(paste0("a", 1:5), rep("continuous", 5), construct = "Alpha", run_bootstrap = FALSE)
  expect_identical(sp$coefficient, "omega_robust")
  expect_identical(sp$requested$ci_method, "recommended")
  expect_identical(sp$ci_target, "percentile")
  expect_identical(sp$uncertainty$status, "pending")
  # A closed form request that does not name the variant gets the variant
  # the closed forms are derived for, and is labeled as such.
  cf <- sieve_reliability_spec(paste0("a", 1:5), rep("continuous", 5), construct = "Alpha", ci_method = "mlr")
  expect_identical(cf$coefficient, "omega")
  expect_identical(cf$ci_method, "mlr")

  st <- scores_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      scope = "construct", treatment = "study", missing = "listwise",
      denominator = "observed", ci_method = "recommended", conf_level = 0.95,
      B = 40, boot_seed = 7, show_alpha = FALSE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    df <- reliab()
    items <- shiny::isolate(state$items)
    for (cn in c("Alpha", "Beta")) {
      its <- grep(if (cn == "Alpha") "^a" else "^b", names(items), value = TRUE)
      row <- df[df$construct == cn, ]
      robust <- DMAR::reliability_omega(items[, its], ci_method = "none", denominator = "observed")
      # Model-implied omega takes DMAR's default interval, robust ML with its
      # limits on the logit scale.
      implied <- DMAR::reliability_omega(items[, its], ci_method = "mlr_logistic", denominator = "model_implied")
      value <- function(r, term) r$value[r$term == term]
      # The point estimate of robust omega is shown at once; its interval waits.
      expect_identical(row$omega, value(robust, "estimate"))
      expect_identical(row$uncertainty, "pending")
      expect_match(row$interval, "^bootstrap not run yet \\(Bootstrap percentile, 40 replications\\)$")
      # Beside it: model implied omega with its robust ML interval.
      expect_identical(
        c(row$model_omega, row$model_lower, row$model_upper),
        c(value(implied, "estimate"), value(implied, "ci_lower"), value(implied, "ci_upper"))
      )
      expect_identical(row$model_interval, "MLR (sandwich) SE, logit-scale limits (delta method)")
      # And the one-factor model's RMSEA with its interval, family named.
      fit <- lavaan::cfa(sprintf("f =~ %s", paste(its, collapse = " + ")), data = items,
        std.lv = TRUE, estimator = "ML", se = "robust.sem", test = "satorra.bentler")
      fam <- row$rmsea_family
      expect_true(fam %in% c("standard", "scaled", "robust"))
      col <- sieve_fit_columns()[[fam]]
      # (The interval is at the RMSEA level, 90%, whatever the page's
      # confidence level; lavaan is called at that level.)
      expect_identical(row$rmsea_level, 0.90)
      fm <- lavaan::fitMeasures(fit, unname(col[c("rmsea", "rmsea_lo", "rmsea_hi")]),
        fm.args = list(rmsea.ci.level = row$rmsea_level))
      expect_equal(c(row$rmsea, row$rmsea_lower, row$rmsea_upper), unname(fm), tolerance = 1e-10)
      expect_match(row$rmsea_text, sprintf("(%s)", fam), fixed = TRUE)
    }
    # (The compact table: these are among the columns Show all columns
    # adds.)
    session$setInputs(show_all = TRUE)
    tbl <- paste(as.character(unlist(output$omega_tbl)), collapse = " ")
    expect_match(tbl, "Omega (model-implied denominator)", fixed = TRUE)
    # (At the RMSEA level, 90%.)
    expect_match(tbl, "One-factor RMSEA [90% CI]", fixed = TRUE)
    session$setInputs(show_all = FALSE)
    # The bootstrap is offered beside the table, with its wait, before it runs.
    main <- as.character(output$boot_main$html)
    expect_match(main, "Interval: bootstrap not run yet", fixed = TRUE)
    expect_match(main, "estimated wait is", fixed = TRUE)
    expect_match(main, "Run the bootstrap", fixed = TRUE)
    session$setInputs(run_boot_main = 1)
    expect_true(boot_running())
    # (DMAR's bootstrap runs in a background process: the page polls it
    # every 200 ms of session time; a run still going at the limit fails
    # here, saying so.)
    siever_wait_until(function() !isTRUE(boot_running()), what = "The page's background bootstrap",
      step = function() {
        session$elapse(200)
        Sys.sleep(0.05)
      })
    done <- reliab()
    expect_true(all(done$uncertainty == "completed"))
    expect_true(all(done$interval == "Bootstrap percentile"))
    expect_true(all(done$ci_lower <= done$omega & done$omega <= done$ci_upper))
    expect_identical(done$model_omega, df$model_omega)
    # The method note says what the model implied coefficient assumes.
    notes <- as.character(output$status_notes$html)
    expect_match(notes, "it is the reliability of the composite only if that model holds", fixed = TRUE)
  })
})

test_that("categorical omega takes the BCa bootstrap by default, with its jackknife cost stated", {
  # Explicit reduced-count lifecycle/timing mechanics; valid production counts are tested separately.
  siever_local_dmar_mechanics()
  types <- rep("ordered", 4)
  sp <- sieve_reliability_spec(paste0("x", 1:4), types, construct = "C", run_bootstrap = FALSE)
  expect_identical(sp$coefficient, "omega_categorical")
  expect_identical(sp$ci_target, "dmar_bca")
  run <- sieve_reliability_spec(paste0("x", 1:4), types, construct = "C", B = 200)
  expect_identical(run$ci_method, "dmar_bca")
  expect_identical(run$resampling, "dmar")
  # One request, resolved construct by construct.
  expect_identical(sieve_reliability_spec(paste0("x", 1:4), rep("continuous", 4))$ci_method, "percentile")
  m <- sieve_reliability_menu(c("categorical", "continuous"), selected = NULL)
  expect_identical(m$selected, "recommended")
  # DMAR's defaults.
  expect_match(m$note, "categorical omega takes DMAR's default, the BCa bootstrap", fixed = TRUE)
  expect_match(m$note, "jackknife of one refit per row", fixed = TRUE)
  # The wait counts the jackknife when the replications are fewer than the rows.
  expect_identical(sieve_bootstrap_wait(0.5, 200, 500, "dmar_bca"), 0.5 * 700)
  expect_identical(sieve_bootstrap_wait(0.5, 1000, 500, "dmar_bca"), 0.5 * 1000)
  # The tab states it before anything runs.
  st <- scores_state()
  shiny::isolate({
    items <- st$items
    for (it in names(items)) items[[it]] <- as.integer(cut(items[[it]], breaks = 5))
    meta <- st$meta
    meta$type <- "ordered"
    st$items <- items
    st$meta <- meta
  })
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      scope = "construct", treatment = "study", missing = "listwise",
      denominator = "observed", ci_method = "recommended", conf_level = 0.95,
      B = 200, boot_seed = NA, show_alpha = FALSE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    df <- reliab()
    expect_true(all(df$uncertainty == "pending"))
    expect_true(all(df$interval == "bootstrap not run yet (Bootstrap BCa (DMAR), 200 replications)"))
    expect_identical(boot_plan()$methods, c("dmar_bca", "dmar_bca"))
    expect_identical(boot_plan()$jackknife, c(300L, 300L))
    note <- as.character(output$boot_note$html)
    expect_match(note, "jackknife behind the BCa acceleration constant", fixed = TRUE)
    expect_match(note, "600 refits in all here", fixed = TRUE)
  })
})

test_that("the interface says what is robust about robust omega and what is not", {
  txt <- sieve_reliability_variant_text()
  expect_match(txt, "Its denominator is the variance of the composite actually scored", fixed = TRUE)
  expect_match(txt, "the denominator does not depend on the one-factor model", fixed = TRUE)
  expect_match(txt, "still comes from that model, so robust omega does not repair a misspecified model (Flora, 2020)", fixed = TRUE)
  expect_match(txt, "Kelley and Pornprasertmanit's (2016) hierarchical omega", fixed = TRUE)
  ui <- paste(as.character(mod_reliab_ui("reliab")), collapse = " ")
  expect_false(grepl("keeps its interpretation when the one-factor model is only approximate", ui, fixed = TRUE))
  expect_false(grepl("only robust omega keeps its interpretation", ui, fixed = TRUE))
  expect_match(ui, "What is robust is the denominator", fixed = TRUE)
  expect_match(ui, "robust omega does not repair a misspecified model, and neither variant does (Flora, 2020)", fixed = TRUE)
})

test_that("every row names the target of its coefficient and of H", {
  its <- paste0("x", 1:4)
  expect_identical(
    sieve_reliability_spec(its, rep("continuous", 4), denominator = "observed")$label,
    "Robust omega (observed denominator)"
  )
  expect_identical(
    sieve_reliability_spec(its, rep("continuous", 4), denominator = "model_implied")$label,
    "Omega (model-implied denominator)"
  )
  expect_identical(sieve_reliability_spec(its, rep("ordered", 4))$label, "Robust omega (observed denominator: unrestricted polychoric correlations)")
  expect_identical(
    sieve_reliability_spec(its, c("continuous", "continuous", "ordered", "ordered"))$label,
    "Robust omega (observed denominator; item scores as continuous)"
  )
  X <- scores_one_factor(250, c(.8, .7, .6, .5), seed = 3)
  O <- as.data.frame(lapply(X, function(v) as.integer(cut(v, 5))))
  cont <- sieve_reliability_construct(X, sieve_reliability_spec(its, rep("continuous", 4), construct = "C",
    ci_method = "none"), alpha = FALSE)
  ord <- sieve_reliability_construct(O, sieve_reliability_spec(its, rep("ordered", 4), construct = "O",
    ci_method = "none"), alpha = FALSE)
  tb <- sieve_reliability_table(list(C = cont, O = ord))
  expect_identical(tb$coefficient, c("Robust omega (observed denominator)", "Robust omega (observed denominator: unrestricted polychoric correlations)"))
  expect_identical(tb$H_label, c("H (observed item scores)", "H (latent responses)"))
  st <- scores_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      scope = "construct", treatment = "study", missing = "listwise",
      denominator = "observed", ci_method = "recommended", conf_level = 0.95,
      B = 40, boot_seed = 7, show_alpha = FALSE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    # (The compact table: H is among the columns Show all columns adds.)
    session$setInputs(show_all = TRUE)
    tbl <- paste(as.character(unlist(output$omega_tbl)), collapse = " ")
    expect_match(tbl, "H coefficient", fixed = TRUE)
    expect_identical(reliab()$H_label, rep("H (observed item scores)", 2))
    expect_identical(reliab()$coefficient, rep("Robust omega (observed denominator)", 2))
  })
})

test_that("the H statement promises what the one-factor model's own score attains, not more", {
  ui <- paste(as.character(mod_reliab_ui("reliab")), collapse = " ")
  expect_false(grepl("would recover reliability that the simple composite discards", ui, fixed = TRUE))
  expect_match(ui, "the squared determinacy of its regression score equals H", fixed = TRUE)
  expect_match(ui, "It is not a promise about the factor scores on this tab", fixed = TRUE)
  expect_match(ui, "it does not recover H, it removes the scoring step", fixed = TRUE)
  # The statement holds: for a one-factor model with uncorrelated errors the
  # squared determinacy of the model's regression score is H.
  X <- scores_one_factor(300, c(.8, .7, .6, .5), seed = 5)
  H <- sieve_reliability_H(X, sieve_reliability_spec(names(X), rep("continuous", 4), construct = "F",
    ci_method = "ml"))
  fit <- lavaan::cfa("f =~ x1 + x2 + x3 + x4", data = X, std.lv = TRUE)
  expect_equal(unname(lavaan::lavInspect(fit, "fs.det"))^2, H$estimate, tolerance = 1e-8)
})

test_that("reliability reference values are conventions with a source, not verdict words", {
  ui <- paste(as.character(mod_reliab_ui("reliab")), collapse = " ")
  expect_false(grepl("conventionally acceptable", ui, fixed = TRUE))
  expect_false(grepl(">= .80 good", ui, fixed = TRUE))
  expect_match(ui, "Conventional reference values, not verdicts:", fixed = TRUE)
  expect_match(ui, "Nunnally and Bernstein (1994)", fixed = TRUE)
  st <- scores_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      scope = "construct", treatment = "study", missing = "listwise",
      denominator = "observed", ci_method = "recommended", conf_level = 0.95,
      B = 40, boot_seed = 7, show_alpha = FALSE, score_method = "Bartlett"
    )
    # (The reference values are in the closed "More on how to read this
    # table" below the table, footnote_more; both parts are read.)
    foot <- paste(as.character(output$footnote$html), as.character(output$footnote_more$html))
    expect_false(grepl("acceptable", foot, fixed = TRUE))
    expect_false(grepl(">= .80 good", foot, fixed = TRUE))
    expect_match(foot, "Conventional reference values (Nunnally &amp; Bernstein, 1994)", fixed = TRUE)
  })
})

test_that("the recorded reliability step keeps its values under source()", {
  st <- scores_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      scope = "construct", treatment = "study", missing = "listwise",
      denominator = "observed", ci_method = "recommended", conf_level = 0.95,
      B = 40, boot_seed = 7, show_alpha = TRUE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    df <- reliab()
    log <- shiny::isolate(state$codelog)
    code <- log[!startsWith(log, "# [")]
    path <- tempfile(fileext = ".R")
    writeLines(code, path)
    env <- new.env(parent = globalenv())
    env$items <- shiny::isolate(state$items)
    source(path, local = env)
    term <- function(v, name) v$value[v$term == name]
    for (cn in c("Alpha", "Beta")) {
      row <- df[df$construct == cn, ]
      expect_true(all(paste0(c("omega_", "omega_model_implied_", "H_", "alpha_"), cn) %in% ls(env)))
      expect_identical(term(env[[paste0("omega_", cn)]], "estimate"), row$omega)
      expect_identical(
        c(term(env[[paste0("omega_model_implied_", cn)]], "estimate"),
          term(env[[paste0("omega_model_implied_", cn)]], "ci_lower")),
        c(row$model_omega, row$model_lower)
      )
      # H is DMAR::reliability_H()'s table.
      expect_equal(term(env[[paste0("H_", cn)]], "estimate"), row$H, tolerance = 1e-12)
      expect_identical(term(env[[paste0("alpha_", cn)]], "estimate"), row$alpha)
    }
  })
})

test_that("the AIPE planner records its full-precision call once, on request", {
  st <- scores_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      scope = "construct", treatment = "study", missing = "listwise",
      denominator = "observed", ci_method = "recommended", conf_level = 0.95,
      B = 40, boot_seed = 7, show_alpha = FALSE, score_method = "Bartlett",
      plan_omega = 0.8, plan_k = 6, plan_width = 0.075, plan_assurance = ""
    )
    shown <- as.character(output$plan_result$html)
    n_shown <- as.integer(sub(".*Required N: ([0-9]+)\\..*", "\\1", shown))
    # Showing the plan records nothing.
    expect_false(any(grepl("Plan the next study", shiny::isolate(state$codelog), fixed = TRUE)))
    session$setInputs(plan_log = 1)
    log <- shiny::isolate(state$codelog)
    expect_identical(sum(grepl("^# \\[[0-9]+\\] Plan the next study", log)), 1L)
    expect_true(any(grepl("width = 0.075,", log, fixed = TRUE)))
    expect_match(as.character(output$plan_logged$html), sprintf("Recorded: N = %d", n_shown), fixed = TRUE)
    # The recorded R gives the N the tab showed.
    code <- log[!startsWith(log, "# [")]
    start <- grep("^planning_omega <- ", code)
    plan <- eval(parse(text = paste(code[start:length(code)], collapse = "\n")), envir = new.env())
    expect_identical(as.integer(plan$value[plan$term == "necessary_N"]), n_shown)
    direct <- DMAR::ss_aipe_reliability(
      model = "Parallel", type = "Normal Theory", width = 0.075, n_items = 6,
      true_item_covariance = 0.8 / (6 - 0.8 * 5), true_residual_var = 1 - 0.8 / (6 - 0.8 * 5), conf_level = 0.95
    )
    expect_identical(n_shown, as.integer(direct$value[direct$term == "necessary_N"]))
  })
})

test_that("the tight and loose cross-validation discrepancies use one covariance divisor", {
  data <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  meta <- data.frame(item = names(data), label = names(data),
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
    facet = NA_character_, type = "continuous", status = "active",
    reverse = FALSE, stringsAsFactors = FALSE)
  mod <- "V =~ x1 + x2 + x3\nT =~ x4 + x5 + x6\nS =~ x7 + x8 + x9"
  fit <- lavaan::cfa(mod, data = data, std.lv = TRUE, estimator = "MLR", missing = "ml")
  # Evaluated on its own data, the frozen solution's discrepancy is the
  # re-estimated one: twice the minimized fmin.
  expect_equal(
    sieve_cross_validation_discrepancy(fit, data, NULL),
    2 * unname(lavaan::fitMeasures(fit, "fmin")),
    tolerance = 1e-8
  )
  # A split of HS9 (holdout .5, seed 113): .650652 with divisor n.
  cv <- sieve_cross_validate(data, meta, holdout = .5, seed = 113)
  dv <- data[cv$validation_rows, ]
  dc <- data[cv$calibration_rows, ]
  fc <- lavaan::cfa(mod, data = dc, std.lv = TRUE, estimator = "MLR", missing = "ml")
  Sig <- lavaan::lavInspect(fc, "implied")$cov
  Sv <- stats::cov(dv) * (nrow(dv) - 1) / nrow(dv)
  ref <- log(det(Sig)) + sum(diag(Sv %*% solve(Sig))) - log(det(Sv)) - 9
  expect_equal(cv$xval$Discrepancy[1], ref, tolerance = 1e-8)
  expect_equal(cv$xval$Discrepancy[3], cv$xval$Discrepancy[1] - cv$xval$Discrepancy[2], tolerance = 1e-12)
})

test_that("the split reliability takes the Reliability tab's target, carries intervals, and claims consistency, not replication", {
  data <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  meta <- data.frame(item = names(data), label = names(data),
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
    facet = NA_character_, type = "continuous", status = "active",
    reverse = FALSE, stringsAsFactors = FALSE)
  cv <- sieve_cross_validate(data, meta, holdout = .5, seed = 113)
  rel <- cv$rel
  expect_identical(unique(rel$Construct), c("Visual", "Textual", "Speed"))
  dc <- data[cv$calibration_rows, ]
  dv <- data[cv$validation_rows, ]
  value <- function(r, term) r$value[r$term == term]
  for (cn in c("Visual", "Textual", "Speed")) {
    its <- meta$item[meta$construct == cn]
    rows <- rel[rel$Construct == cn, ]
    target <- rows[rows$Coefficient == "Robust omega (observed denominator)", ]
    implied <- rows[rows$Coefficient == "Omega (model-implied denominator)", ]
    expect_identical(nrow(target), 1L)
    expect_identical(nrow(implied), 1L)
    # The target is robust omega, as on the Reliability tab, in each half.
    expect_equal(target$Calibration, value(DMAR::reliability_omega(dc[, its], ci_method = "none", denominator = "observed"), "estimate"), tolerance = 1e-12)
    expect_equal(target$Validation, value(DMAR::reliability_omega(dv[, its], ci_method = "none", denominator = "observed"), "estimate"), tolerance = 1e-12)
    # Model implied omega with DMAR's default interval (robust ML, logit
    # scale) in each half.
    a <- DMAR::reliability_omega(dc[, its], ci_method = "mlr_logistic", denominator = "model_implied")
    b <- DMAR::reliability_omega(dv[, its], ci_method = "mlr_logistic", denominator = "model_implied")
    expect_equal(c(implied$Calibration, implied$Calibration.lower, implied$Calibration.upper),
      c(value(a, "estimate"), value(a, "ci_lower"), value(a, "ci_upper")), tolerance = 1e-12)
    expect_equal(c(implied$Validation, implied$Validation.lower, implied$Validation.upper),
      c(value(b, "estimate"), value(b, "ci_lower"), value(b, "ci_upper")), tolerance = 1e-12)
    # The difference of independent halves, by the delta method.
    half <- stats::qnorm(.975) * sqrt(value(a, "se")^2 + value(b, "se")^2)
    d <- value(b, "estimate") - value(a, "estimate")
    expect_equal(c(implied$Difference, implied$Difference.lower, implied$Difference.upper),
      c(d, d - half, d + half), tolerance = 1e-12)
  }
  # The Reliability tab set to model implied omega: the target carries the interval.
  mi <- sieve_cross_validate(data, meta, holdout = .5, seed = 113, denominator = "model_implied")
  expect_true(all(mi$rel$Coefficient == "Omega (model-implied denominator)"))
  expect_true(all(is.finite(mi$rel$Calibration.lower) & is.finite(mi$rel$Difference.upper)))
  # The claim is consistency, not evidence against an artifact.
  ui <- paste(as.character(mod_finalize_ui("finalize")), collapse = " ")
  expect_false(grepl("are evidence the reliability is not an artifact", ui, fixed = TRUE))
  expect_match(ui, "are consistent with a reliability that does not depend on which rows calibrated the model", fixed = TRUE)
  expect_match(ui, "not independent replication", fixed = TRUE)
})

test_that("the sample table downloads as a sample reference table with its provenance", {
  st <- scores_state()
  st$dataset_name <- "two constructs"
  ui <- paste(as.character(mod_finalize_scoring_ui("finalize")), collapse = " ")
  expect_match(ui, "Download sample reference table (.csv)", fixed = TRUE)
  expect_false(grepl("Download norm table", ui, fixed = TRUE))
  expect_match(ui, "Scoring rules, sample reference table, and frozen norm references", fixed = TRUE)
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    path <- output$dl_norms
    expect_true(file.exists(path))
    expect_match(basename(path), "siever_sample_reference_table.csv", fixed = TRUE)
    got <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    expect_identical(got$construct, c("Alpha", "Beta"))
    expect_true(all(c("reference", "sample_rows", "dataset", "computed", "siever_version") %in% names(got)))
    expect_true(all(got$sample_rows == 300L))
    expect_true(all(got$dataset == "two constructs"))
    expect_true(all(grepl("not population norms", got$reference, fixed = TRUE)))
    expect_true(all(got$siever_version == sieve_version()))
    expect_true(all(grepl("^20[0-9]{2}-[0-9]{2}-[0-9]{2} ", got$computed)))
    # The numbers are the table's, unrounded.
    tab <- sieve_construct_norms(shiny::isolate(state$items), shiny::isolate(state$meta))$table
    expect_equal(got$mean, tab$mean, tolerance = 1e-12)
  })
})

test_that("criterion correlations are descriptive, with no interval, under a declared dependent design", {
  d <- withr::with_seed(20261012, {
    n <- 400
    f1 <- stats::rnorm(n)
    f2 <- stats::rnorm(n)
    item <- function(f, l) l * f + sqrt(1 - l^2) * stats::rnorm(n)
    data.frame(
      a1 = item(f1, .8), a2 = item(f1, .75), a3 = item(f1, .7),
      b1 = item(f2, .8), b2 = item(f2, .75), b3 = item(f2, .7),
      crit = 0.4 * f1 + stats::rnorm(n), cl = rep(1:20, each = 20)
    )
  })
  seen <- list()
  for (unit in c("independent", "clustered", "repeated")) {
    s <- sieve_new_state()
    s$raw <- d
    s$items <- d[, 1:6]
    s$id_vars <- c("crit", "cl")
    s$meta <- sieve_build_meta(d, names(d)[1:6], list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3")))
    s$confirmed <- TRUE
    s$palette <- "nd"
    s$thresholds <- sieve_default_thresholds()
    s$sampling_design <- if (unit == "clustered") list(unit = "clustered", cluster = "cl") else list(unit = unit)
    shiny::testServer(mod_validity_server, args = list(state = s), {
      session$setInputs(score_method = "mean", criteria = "crit", htmt_on = FALSE, marker = "")
      seen[[unit]] <<- crit_df()
      if (unit != "independent") {
        invisible(output$crit_tbl)
        log <- paste(shiny::isolate(state$codelog), collapse = "\n")
        expect_match(log, sprintf("descriptive, no interval under the declared %s sampling unit", unit), fixed = TRUE)
        expect_false(grepl("stats::cor.test(construct_scores", log, fixed = TRUE))
      }
    })
  }
  ind <- seen$independent
  expect_true(all(is.finite(ind$lower) & is.finite(ind$upper)))
  for (unit in c("clustered", "repeated")) {
    dep <- seen[[unit]]
    # The same correlation, described; no interval and no p-value.
    expect_equal(dep$r, ind$r, tolerance = 1e-12)
    expect_true(all(is.na(dep$lower) & is.na(dep$upper) & is.na(dep$p)))
    expect_true(all(grepl(sprintf("The declared sampling unit is %s", unit), dep$reason, fixed = TRUE)))
  }
  # A stated prediction is not scored against an interval the design does not support.
  ev <- sieve_validity_evidence(
    data.frame(construct = "A", criterion = "crit", r = seen$clustered$r[1], lower = NA_real_,
      upper = NA_real_, n = 400L, p = NA_real_, reason = seen$clustered$reason[1], stringsAsFactors = FALSE),
    data.frame(construct = "A", criterion = "crit", kind = "directional", sign = 1, min_abs = NA_real_,
      bound = NA_real_, stringsAsFactors = FALSE)
  )
  expect_false(ev$scored)
  expect_match(ev$statement, "declared sampling unit is clustered", fixed = TRUE)
})

test_that("the content exclusion lists the selected items for confirmation and goes through sieve_set_item_status() with a reason", {
  fx <- sieve_hs9_fixture()
  s <- sieve_new_state()
  s$dataset_name <- fx$name
  s$raw <- fx$data
  s$items <- fx$data[, fx$item_cols, drop = FALSE]
  s$id_vars <- fx$id_vars
  s$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  s$confirmed <- TRUE
  s$palette <- "nd"
  s$thresholds <- sieve_default_thresholds()
  shiny::testServer(mod_content_server, args = list(state = s), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(demo = 1)
    session$setInputs(keep_all_matches = 1)
    # The researcher selects the items (no I-CVI cutoff selects them).
    failing <- matched()$ledger_item[c(7L, 9L)]
    session$setInputs(ledger_tbl_rows_selected = c(7L, 9L))
    # The button lists the items; nothing is excluded until confirmed.
    session$setInputs(park_selected = 1)
    expect_true(all(state$meta$status == "active"))
    expect_identical(sort(park_pending()$ledger_item), sort(failing))
    session$setInputs(park_selected_confirm = 1)
    meta <- state$meta
    expect_true(all(meta$status[meta$item %in% failing] == "parked"))
    expect_true(all(meta$status[!meta$item %in% failing] == "active"))
    # The ledger keeps a reason written from the item's own evidence.
    reasons <- meta$exclude_reason[match(failing, meta$item)]
    expect_true(all(grepl("^Content validity: I-CVI = [0-9.]+ with [0-9]+ experts \\(95% CI \\[", reasons)))
    expect_true(all(grepl("excluded after review on the Content Validity tab.", reasons, fixed = TRUE)))
    expect_false(any(grepl("Lynn|criterion", reasons)))
    # The item actions and the replayed code carry the change as a mutation.
    actions <- shiny::isolate(state$item_actions)
    expect_identical(sort(unlist(lapply(actions, `[[`, "items"))), sort(failing))
    expect_true(all(vapply(actions, function(a) identical(a$status, "parked"), logical(1))))
    log <- shiny::isolate(state$codelog)
    for (it in failing) {
      expect_true(any(grepl(sprintf("meta$status[meta$item %%in%% c(\"%s\")] <- \"parked\"", it), log, fixed = TRUE)))
    }
    expect_true(any(grepl("^# Reason: +Content validity: I-CVI", log)))
    expect_true(any(grepl("(reason: Content validity: I-CVI", log, fixed = TRUE)))
  })
})

test_that("the HTMT bootstrap uses the table's rows, shows its used and dropped resamples, and states the range of B", {
  d <- withr::with_seed(20261021, {
    n <- 240
    f1 <- stats::rnorm(n)
    f2 <- .5 * f1 + sqrt(.75) * stats::rnorm(n)
    item <- function(f, l) l * f + sqrt(1 - l^2) * stats::rnorm(n)
    data.frame(
      a1 = item(f1, .8), a2 = item(f1, .75), a3 = item(f1, .7),
      b1 = item(f2, .8), b2 = item(f2, .75), b3 = item(f2, .7)
    )
  })
  # Scattered missing values, so complete cases and pairwise correlations differ.
  withr::with_seed(5, for (j in names(d)) d[sample.int(nrow(d), 25), j] <- NA)
  s <- sieve_new_state()
  s$raw <- d
  s$items <- d
  s$meta <- sieve_build_meta(d, names(d), list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3")))
  s$confirmed <- TRUE
  s$palette <- "nd"
  s$thresholds <- sieve_default_thresholds()
  shiny::testServer(mod_validity_server, args = list(state = s), {
    # A request is never lowered to shorten a wait (no cap is placed on it;
    # the estimate is stated instead). A sub-minimum request is refused
    # unchanged before dispatch. A valid request
    # then uses all declared rows. That 20,000 runs as 20,000 is tested with a mocked
    # engine in test-score-specification.R.
    session$setInputs(
      score_method = "mean", criteria = character(0), htmt_on = TRUE, marker = "",
      htmt_B = 50, htmt_thresh = 0.85, htmt_seed = 11
    )
    session$setInputs(htmt_boot = 1)
    refused <- htmt_ci()
    expect_match(refused$error, "at least 200", fixed = TRUE)
    expect_null(refused$table)
    session$setInputs(htmt_B = 200, htmt_boot = 2)
    boot <- htmt_ci()
    expect_null(boot$error)
    # The bootstrap's HTMT is the table's (pairwise correlations on every row).
    expect_identical(boot$table$htmt, htmt_pairs()$htmt)
    expect_identical(boot$rows, nrow(d))
    expect_identical(boot$B, 200)
    note <- paste(as.character(unlist(output$htmt_ci_note)), collapse = " ")
    expect_false(grepl("were requested", note, fixed = TRUE))
    expect_match(note, sprintf("%d of the 200 resamples were used", boot$used), fixed = TRUE)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "htmt_rows <- active_items[rowSums(!is.na(active_items)) > 0, , drop = FALSE]", fixed = TRUE)
  })
  # Resamples the engine drops are counted and shown.
  real <- DMAR::htmt
  testthat::local_mocked_bindings(
    htmt = function(data, blocks, B = 0, conf_level = 0.95, seed = NULL) {
      out <- real(data, blocks)
      if (B > 0) {
        out$ci_upper <- out$htmt + 0.1
        # Counts come from the structured result even when no prose warning
        # is emitted (seven drops are below the one-sided 5% caution share).
        attr(out, "B") <- as.integer(B)
        attr(out, "B_used") <- as.integer(B - 7L)
        attr(out, "n_dropped") <- 7L
        attr(out, "B_required") <- 200L
        attr(out, "bootstrap_status") <- "ok"
        attr(out, "conf_level") <- conf_level
      }
      out
    },
    .package = "DMAR"
  )
  shiny::testServer(mod_validity_server, args = list(state = s), {
    session$setInputs(
      score_method = "mean", criteria = character(0), htmt_on = TRUE, marker = "",
      htmt_B = 200, htmt_thresh = 0.85, htmt_seed = 11
    )
    session$setInputs(htmt_boot = 1)
    boot <- htmt_ci()
    expect_identical(boot$dropped, 7L)
    expect_identical(boot$used, 193L)
    note <- paste(as.character(unlist(output$htmt_ci_note)), collapse = " ")
    expect_match(note, "193 of the 200 resamples were used; 7 left HTMT undefined for some pair", fixed = TRUE)
  })
})
