# Tests for the Network page (R/mod_network.R, R/fct_network.R), which fits
# psychometric network models. The engine is psychonetrics,
# a Suggests package, so everything here skips without it. The page follows
# the run policy of fct_ondemand.R: nothing is estimated until a button is
# pressed, one engine call per run, the result reused while nothing
# analytic changed. The scientific identities tested are the ones the page
# teaches: the saturated latent network is the correlated-factors CFA (its
# implied construct correlations are Phi), the residual network of the nine
# Holzinger and Swineford tests carries the known local dependence of the
# two speeded tests, and the walktrap communities of the item network
# reproduce the three constructs.

skip_if_not_installed("psychonetrics")
skip_if_not_installed("qgraph")
skip_if_not_installed("igraph")
skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

# The bundled fixture: nine continuous Holzinger and Swineford tests, three
# constructs (Visual, Textual, Speed), confirmed.
network_state <- function(confirmed = TRUE) {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = confirmed, labels = TRUE,
    theory = TRUE, name = fx$name, configure = function(s) {
  m <- shiny::isolate(s$meta)
  m$type <- "continuous"
  s$meta <- m
  s$case_index <- seq_len(nrow(fx$data))
  s$estimator <- "MLR"

  })
  s
}

# The page's estimates run in a separate R process (sieve_network_worker_start());
# the tests of its logic run them in this one (sieve_network_inline_start()),
# where the engine's calls can be counted and replaced. The separate process,
# its Cancel and the wait are tested in section 8.
network_start <- sieve_network_inline_start

network_inputs <- function(session, ...) {
  defaults <- list(alpha = 0.01, estimator = "study")
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}

# ---- 1. The page is registered where every page is ------------------------

test_that("the Network page sits after Bifactor in the tab bar, the catalog and the Structure workspace", {
  expect_true("network" %in% names(sieve_page_catalog()))
  expect_identical(sieve_page_catalog()$network$label, "Network")
  expect_true("network" %in% sieve_pages_with_update())
  structure <- sieve_workspaces()$structure$pages
  expect_identical(structure[match("bifactor", structure) + 1L], "network")
  expect_identical(sieve_page_workspace("network"), "structure")
  paths <- sieve_tab_paths()
  expect_identical(paths$optional[match("Bifactor", paths$optional) + 1L], "Network")
  html <- paste(as.character(app_ui(list())), collapse = "")
  values <- regmatches(html, gregexpr('data-value="[a-z]+"', html))[[1]]
  values <- unique(sub('data-value="([a-z]+)"', "\\1", values))
  expect_identical(values[match("bifactor", values) + 1L], "network")
  page <- interface_html <- paste(as.character(mod_network_ui("network")), collapse = "")
  expect_match(page, "This tab is optional.", fixed = TRUE)
  expect_match(page, "What a Network Model Shows", fixed = FALSE)
  # No em dashes anywhere on the page.
  expect_false(grepl("\u2014", page, fixed = TRUE))
})

# The source files are read only where the source tree is beside the tests:
# an installed package (R CMD check, the installed-test profile) has no R/
# folder there, and its functional tests above must still run.
test_that("the Network source files have no em dash and round nothing on the way to a value", {
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  for (f in c("R/fct_network.R", "R/mod_network.R")) {
    src <- readLines(file.path(root, f), warn = FALSE)
    expect_false(any(grepl("\u2014", src, fixed = TRUE)), info = f)
    # Full precision is stored: nothing is rounded on the way to a value.
    code <- src[!grepl("^[[:space:]]*#", src)]
    expect_false(any(grepl("round\\(|signif\\(", code)), info = f)
  }
})

# ---- 2. The gate, and nothing estimated until the button ------------------

test_that("the gate speaks before data and before confirmation, and no engine call is made", {
  test_frame <- environment()
  calls <- 0L
  real_ggm <- psychonetrics::ggm
  testthat::local_mocked_bindings(
    ggm = function(...) {
      calls <<- calls + 1L
      real_ggm(...)
    },
    .package = "psychonetrics", .env = test_frame
  )
  st <- sieve_new_state()
  shiny::testServer(mod_network_server, args = list(state = st, start = network_start), {
    network_inputs(session)
    expect_match(as.character(output$gate$html), "No data loaded yet", fixed = TRUE)
    session$setInputs(ggm_go = 1)
    expect_identical(calls, 0L)
    expect_null(shown("ggm"))
  })
  st <- network_state(confirmed = FALSE)
  shiny::testServer(mod_network_server, args = list(state = st, start = network_start), {
    network_inputs(session)
    expect_match(as.character(output$gate$html), "await confirmation", fixed = TRUE)
    session$setInputs(ggm_go = 1)
    expect_identical(calls, 0L)
  })
})

test_that("the item network waits for its button, calls the engine once per run, and reuses an unchanged result", {
  test_frame <- environment()
  calls <- 0L
  real_ggm <- psychonetrics::ggm
  testthat::local_mocked_bindings(
    ggm = function(...) {
      calls <<- calls + 1L
      real_ggm(...)
    },
    .package = "psychonetrics", .env = test_frame
  )
  st <- network_state()
  shiny::testServer(mod_network_server, args = list(state = st, start = network_start), {
    network_inputs(session)
    expect_null(output$gate$html)
    # The line states the estimate and the button; nothing has run.
    line <- as.character(output$ggm_line$html)
    expect_match(line, "is not estimated yet", fixed = TRUE)
    expect_match(line, "Estimate the item network", fixed = TRUE)
    expect_identical(demands$ggm$status()$state, "not_run")
    expect_identical(calls, 0L)
    expect_length(shiny::isolate(state$analyses), 0L)

    session$setInputs(ggm_go = 1)
    expect_identical(calls, 1L)
    st_now <- demands$ggm$status()
    expect_identical(st_now$state, "current")
    r <- st_now$value
    expect_null(r$error)
    expect_identical(r$status$code, "ok")
    # The study's default for continuous items, ML with FIML, is
    # psychonetrics' FIML (on these complete data its likelihood is ML's),
    # timed as ML.
    expect_identical(r$spec$engine$estimator, "FIML")
    expect_identical(sieve_network_wait_class(r$spec$engine), "ML")
    # The sparse network: symmetric, zero diagonal, the pruned pairs at zero.
    W <- r$omega
    expect_identical(dim(W), c(9L, 9L))
    expect_true(isSymmetric(unname(W)))
    expect_true(all(diag(W) == 0))
    expect_identical(rownames(W), r$spec$observed)
    # Every pair in the table, estimated edges with se and a Wald interval.
    e <- r$edges
    expect_identical(nrow(e), 36L)
    est <- e[e$estimated, ]
    expect_true(nrow(est) > 0L && nrow(est) < 36L)
    expect_true(all(is.finite(est$se)) && all(est$se > 0))
    z <- stats::qnorm(0.975)
    expect_equal(est$lower, est$estimate - z * est$se)
    expect_equal(est$upper, est$estimate + z * est$se)
    expect_true(all(is.na(e$se[!e$estimated])) && all(e$estimate[!e$estimated] == 0))
    # The fit of the sparse model, in SIEVER's names with its family.
    m <- r$fit$selected
    expect_true(all(c("chisq", "df", "cfi", "tli", "rmsea", "rmsea_lo", "rmsea_hi", "srmr", "bic") %in% names(m)))
    # Normal theory: the standard indices (MLR, chosen, gives the robust ones).
    expect_identical(attr(m, "family"), "standard")
    expect_true(is.finite(m[["rmsea"]]) && m[["rmsea_lo"]] <= m[["rmsea"]] && m[["rmsea"]] <= m[["rmsea_hi"]])
    expect_identical(unname(m[["df"]]), 36 - nrow(est))
    # Strength and expected influence from the same matrix.
    c_ <- r$centrality
    expect_setequal(c_$node, r$spec$observed)
    expect_equal(c_$strength[match(rownames(W), c_$node)], unname(rowSums(abs(W))))
    expect_equal(c_$expected_influence[match(rownames(W), c_$node)], unname(rowSums(W)))
    # The communities reproduce the three constructs.
    cm <- r$communities
    expect_identical(cm$n_communities, 3L)
    expect_identical(cm$agreement, 9L)
    expect_identical(cm$steps, 4L)

    # Recorded: the analysis registry (kind, methods, stored results), the
    # reproducible code with the actual calls, and the Show code record.
    an <- shiny::isolate(state$analyses)
    expect_length(an, 1L)
    rec <- an[[1L]]
    expect_identical(rec$kind, "network_ggm")
    expect_true(all(c("network_ggm", "network_partial_correlation", "network_model_search", "network_accuracy",
      "network_communities", "network_strength", "fiml", "fit_indices") %in% rec$methods))
    expect_false("mlr" %in% rec$methods)
    expect_true(is.numeric(rec$estimates$fit) && "rmsea_lo" %in% names(rec$estimates$fit))
    expect_true(is.matrix(rec$estimates$network))
    # The record is readable where every record is read: the Reproducible
    # Code page's catalog and the report from recorded analyses, without
    # stopping the session.
    catalog <- sieve_analysis_catalog(state)
    expect_true("network_ggm" %in% catalog$kind)
    report <- sieve_readable_saved_report(state, rec$id)
    expect_match(paste(report, collapse = "\n"), "Item network", fixed = TRUE)
    expect_match(paste(report, collapse = "\n"), "RMSEA", fixed = TRUE)
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("psychonetrics::ggm(", log, fixed = TRUE)))
    expect_true(any(grepl("psychonetrics::prune(", log, fixed = TRUE)))
    expect_true(any(grepl("psychonetrics::stepup(", log, fixed = TRUE)))
    expect_true(any(grepl("igraph::cluster_walktrap(", log, fixed = TRUE)))
    expect_true(is.list(r$code_view) && length(r$code_view$code) > 5L)
    html <- as.character(output$ggm_code$html)
    expect_match(html, "psychonetrics::ggm", fixed = TRUE)
    expect_match(html, "belong to the result on screen", fixed = TRUE)
    # The page guide was told, without a fitted object.
    published <- shiny::isolate(state$page_results)
    expect_true(any(vapply(unlist(published, recursive = FALSE), function(x) identical(x$title, "Item network"), logical(1))))

    # Pressing again with nothing changed reuses the result.
    session$setInputs(ggm_go = 2)
    expect_identical(calls, 1L)
    expect_identical(demands$ggm$status()$state, "current")
    # The method line names the engine, the estimator, the search and the fit.
    method <- as.character(output$ggm_method$html)
    expect_match(method, "Gaussian graphical model of the items in psychonetrics", fixed = TRUE)
    expect_match(method, "prune at alpha = 0.01, then step up by BIC", fixed = TRUE)
    expect_match(method, "RMSEA (standard)", fixed = TRUE)

    # A change of alpha is another set of choices: the wire keeps one result
    # per set (as the other pages do for their estimator control), so the new
    # alpha has no result yet and its run calls the engine once more, while
    # the result for the first alpha is kept and comes back without a call.
    session$setInputs(alpha = 0.05)
    expect_identical(demands$ggm$status()$state, "not_run")
    expect_identical(calls, 1L)
    session$setInputs(ggm_go = 3)
    expect_identical(calls, 2L)
    expect_identical(demands$ggm$status()$state, "current")
    expect_identical(demands$ggm$status()$value$spec$alpha, 0.05)
    session$setInputs(alpha = 0.01)
    expect_identical(demands$ggm$status()$state, "current")
    expect_identical(demands$ggm$status()$value$spec$alpha, 0.01)
    expect_identical(calls, 2L)
    # An excluded item is a change of the inputs: the result is from before.
    m <- shiny::isolate(state$meta)
    m$status[m$item == "t2_cubes"] <- "parked"
    state$meta <- m
    expect_identical(demands$ggm$status()$state, "stale")
    expect_match(demands$ggm$status()$sentence, "t2_cubes excluded", fixed = TRUE)
    expect_identical(calls, 2L)
  })
})

# ---- 3. The residual and latent networks against the CFA -------------------

test_that("the residual network finds the local dependence of the two speeded tests and reports the measurement model", {
  st <- network_state()
  items <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  r <- sieve_network_estimate(items, meta, kind = "network_rnm")
  expect_null(r$error)
  expect_identical(r$status$code, "ok")
  expect_identical(r$spec$latents, unname(sieve_latent_name(sieve_name_plan(meta, reserved = names(items)), c("Visual", "Textual", "Speed"))))
  est <- r$edges[r$edges$estimated, ]
  pair <- est[(est$from == "t12_counting_groups_of_dots" & est$to == "t10_addition") |
    (est$to == "t12_counting_groups_of_dots" & est$from == "t10_addition"), ]
  expect_identical(nrow(pair), 1L)
  expect_gt(pair$estimate, 0.3)
  expect_lt(pair$lower, pair$estimate)
  # The loadings are on the CFA page's scale (factor variances 1) and come
  # with a completely standardized version; Phi with its standard errors.
  expect_identical(nrow(r$loadings), 9L)
  expect_true(all(is.finite(r$loadings$std)) && all(abs(r$loadings$std) < 1))
  expect_identical(nrow(r$phi), 3L)
  expect_true(all(is.finite(r$phi$se)))
  expect_true(all(abs(r$phi$estimate) < 1))
})

test_that("the saturated latent network is the correlated-factors CFA: its implied construct correlations are lavaan's Phi", {
  st <- network_state()
  items <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  plan <- sieve_network_lambda(meta, names(items))
  X <- sieve_network_data(items, plan$items)
  # standardize = "z" as the page passes it: on the raw score scale of these
  # tests (standard deviations from 3.5 to 36) psychonetrics' optimizer
  # stops short of the maximum (chi square 86.5 here, 87.7 for its plain
  # CFA); standardized, it reaches lavaan's 85.3055.
  lnm <- psychonetrics::runmodel(psychonetrics::lnm(X,
    lambda = plan$lambda, latents = plan$latents,
    identification = "variance", estimator = "ML", standardize = "z"
  ))
  implied <- sieve_network_implied_cor(
    psychonetrics::getmatrix(lnm, "omega_zeta"), psychonetrics::getmatrix(lnm, "delta_zeta")
  )
  cfa <- lavaan::cfa(sieve_cfa_plan(meta, reserved = names(items))$model, X, std.lv = TRUE, estimator = "ML")
  phi <- lavaan::lavInspect(cfa, "cor.lv")
  phi <- phi[plan$latents, plan$latents]
  expect_equal(unname(implied), unname(phi), tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(unname(diag(implied)), rep(1, 3), ignore_attr = TRUE)
  # The raw scale, left alone, is the optimizer's problem the page avoids.
  raw <- psychonetrics::runmodel(psychonetrics::lnm(X,
    lambda = plan$lambda, latents = plan$latents,
    identification = "variance", estimator = "ML"
  ))
  expect_gt(sieve_network_fit(raw, "standard")$selected[["chisq"]], unname(lavaan::fitMeasures(cfa, "chisq")) + 0.5)
  expect_true(any(grepl("standardize = \"z\"", sieve_network_estimate(items, meta, kind = "network_lnm")$code, fixed = TRUE)))
  # The same fit, to the chi square.
  expect_equal(
    sieve_network_fit(lnm, "standard")$selected[["chisq"]],
    unname(lavaan::fitMeasures(cfa, "chisq")), tolerance = 1e-3
  )
  # Through the page's builder, the sparse latent network of these data drops
  # the Textual to Speed edge and implies a smaller correlation for that pair.
  l <- sieve_network_estimate(items, meta, kind = "network_lnm")
  expect_identical(l$status$code, "ok")
  W <- l$omega
  tex <- plan$latents[plan$labels == "Textual"]
  spd <- plan$latents[plan$labels == "Speed"]
  expect_identical(unname(W[tex, spd]), 0)
  expect_lt(l$implied_cor[tex, spd], phi[tex, spd])
  expect_equal(unname(diag(l$implied_cor)), rep(1, 3))
  expect_identical(sum(l$edges$estimated), 2L)
})

# ---- 4. Ordered items: polychoric input, DWLS, the scaled family -----------

test_that("ordered items are estimated by DWLS on polychoric correlations with the scaled fit family", {
  st <- network_state()
  items <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  ord <- as.data.frame(lapply(items, function(v) as.integer(cut(as.numeric(v), breaks = 5))))
  names(ord) <- names(items)
  meta$type <- "ordered"
  r <- sieve_network_estimate(ord, meta, kind = "network_ggm")
  expect_identical(r$status$code, "ok")
  expect_identical(r$spec$engine$estimator, "DWLS")
  expect_identical(r$spec$engine$criterion, "none")
  expect_setequal(r$spec$engine$ordered, names(ord))
  m <- r$fit$selected
  expect_identical(attr(m, "family"), "scaled")
  expect_true(is.finite(m[["rmsea"]]) && is.finite(m[["cfi"]]))
  expect_true(is.na(m[["bic"]]))
  expect_true("network_ising_irt" %in% r$spec$methods)
  expect_true(any(grepl("ordered = c(", r$code, fixed = TRUE)))
  expect_true(any(grepl("criterion = \"none\"", r$code, fixed = TRUE)))
})

# ---- 5. Refusals before anything is estimated -----------------------------

test_that("a latent network of one construct is refused before anything is estimated, and a missing engine is named", {
  st <- network_state()
  items <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  meta$construct <- "All"
  l <- sieve_network_estimate(items, meta, kind = "network_lnm")
  expect_null(l$model)
  expect_match(l$error, "at least two constructs", fixed = TRUE)
  expect_false(sieve_has_solution(l$status))
  # The plan's refusal reaches the button's line before any run.
  st$meta <- meta
  sieve_confirm_types(st)
  shiny::testServer(mod_network_server, args = list(state = st, start = network_start), {
    network_inputs(session)
    expect_match(as.character(output$lnm_line$html), "at least two constructs", fixed = TRUE)
  })
  # The engine's own refusal without psychonetrics.
  test_frame <- environment()
  testthat::local_mocked_bindings(sieve_have = function(pkg) !identical(pkg, "psychonetrics"), .env = test_frame)
  g <- sieve_network_estimate(items, shiny::isolate(network_state()$meta), kind = "network_ggm")
  expect_match(g$error, "psychonetrics is not installed", fixed = TRUE)
})

# ---- 6. The references registry ------------------------------------------

test_that("the network methods are in the registry, verified, with their DOIs, and the bibliography cites them", {
  refs <- sieve_method_references()
  keys <- c(
    "network_ggm", "network_partial_correlation", "network_accuracy", "network_model_search",
    "network_factor_equivalence", "network_ising_irt", "network_strength", "network_expected_influence",
    "network_communities", "network_ega"
  )
  expect_true(all(keys %in% refs$key))
  rows <- refs[refs$key %in% keys, ]
  expect_true(all(rows$verified))
  expect_true(all(grepl("^10[.]", rows$doi)))
  expect_true(all(c("psychonetrics", "qgraph", "igraph") %in% refs$key[refs$kind == "software"]))
  bib <- sieve_method_bibliography(c("network_ggm", "network_model_search", "psychonetrics", "qgraph", "igraph"))
  expect_true(any(grepl("Generalized network psychometrics", bib, fixed = TRUE)))
  expect_true(any(grepl("psychonetrics", bib, fixed = TRUE)))
  expect_true(any(grepl("qgraph", bib, fixed = TRUE)))
  expect_identical(attr(bib, "omitted"), character(0))
  # The specification's methods, as the registry record reads them.
  spec <- list(kind = "network_rnm", estimator = "MLR")
  m <- sieve_spec_methods(spec, estimates = list(fit = c(df = 20)), fitted = TRUE)
  expect_true(all(c("network_ggm", "cfa", "mlr", "fit_indices", "network_communities") %in% m))
})

# ---- 7. The method statements ---------------------------------------------
# Under FIML the n stated is every row with an observed item, the rows the
# engine uses, not the complete cases, and the registry's fitted count is
# set for a fit that is not lavaan's. The record and the code disclosure
# carry the psychonetrics, qgraph and igraph versions. The items the
# residual and latent networks leave out are named. The preflight that
# names FIML says the robust correction is unavailable. The edge intervals
# say what they cover. Under DWLS the standard RMSEA interval is not
# labeled "(scaled)".

network_incomplete <- function(items) {
  x <- items
  x[c(2L, 5L, 11L, 17L), 1L] <- NA
  x[c(3L, 5L, 23L), 4L] <- NA
  x[7L, ] <- NA
  x
}

test_that("under FIML the n is the engine's, the record keeps it and the engine versions, and the method line says both counts", {
  st <- network_state()
  st$items <- network_incomplete(shiny::isolate(st$items))
  X <- shiny::isolate(st$items)
  informative <- sum(rowSums(!is.na(X)) > 0)
  complete <- sum(stats::complete.cases(X))
  sieve_confirm_types(st)
  expect_gt(informative, complete)
  shiny::testServer(mod_network_server, args = list(state = st, start = network_start), {
    # MLR asked for on incomplete data (the subject of the note below; the
    # default, ML with FIML, asks for no robust correction).
    network_inputs(session, estimator = "mlr")
    # Before the press: the FIML treatment and its normal theory standard
    # errors are said, and nothing has run.
    line <- gsub("\\s+", " ", as.character(output$ggm_line$html))
    expect_match(line, "Incomplete continuous data are estimated by full information maximum likelihood with normal theory standard errors; psychonetrics applies its robust correction to complete data only.", fixed = TRUE)
    expect_identical(demands$ggm$status()$state, "not_run")
    session$setInputs(ggm_go = 1)
    r <- demands$ggm$status()$value
    expect_identical(r$spec$engine$estimator, "FIML")
    expect_identical(r$n, as.integer(informative))
    expect_identical(r$n_complete, as.integer(complete))
    expect_identical(r$n_input, nrow(X))
    text <- sieve_network_method_text(r)
    expect_match(text, sprintf("n = %d (FIML: every row with an observed item; %d complete cases)", informative, complete), fixed = TRUE)
    expect_match(text, sprintf("in psychonetrics %s:", as.character(utils::packageVersion("psychonetrics"))), fixed = TRUE)
    # After the press the line does not repeat the preflight note.
    expect_false(grepl("Incomplete continuous data are estimated", as.character(output$ggm_line$html), fixed = TRUE))
    rec <- shiny::isolate(state$analyses)[[r$analysis_id]]
    expect_identical(as.integer(rec$sample_size$fitted), as.integer(informative))
    for (pkg in c("psychonetrics", "qgraph", "igraph")) {
      expect_identical(rec$versions[[pkg]], as.character(utils::packageVersion(pkg)), info = pkg)
    }
    expect_true("psychonetrics" %in% names(r$code_view$versions))
    # The catalog and the report read the recorded count and versions.
    cat_x <- sieve_analysis_catalog(state)
    expect_identical(cat_x$fitted[match(r$analysis_id, cat_x$id)], as.integer(informative))
    expect_match(cat_x$versions[match(r$analysis_id, cat_x$id)], "psychonetrics", fixed = TRUE)
  })
  # On complete data nothing is substituted and nothing is said.
  st2 <- network_state()
  shiny::testServer(mod_network_server, args = list(state = st2, start = network_start), {
    network_inputs(session)
    expect_false(grepl("Incomplete continuous data", as.character(output$ggm_line$html), fixed = TRUE))
  })
})

test_that("the residual and latent networks name the items they leave out before the press and beside the result", {
  st <- network_state()
  m <- shiny::isolate(st$meta)
  solo <- m$item[[9L]]
  m$construct[m$item == solo] <- "Solo"
  st$meta <- m
  sieve_confirm_types(st)
  shiny::testServer(mod_network_server, args = list(state = st, start = network_start), {
    network_inputs(session)
    expected <- sprintf("An item of construct 'Solo' is not in the model: its construct has fewer than 2 included items (%s).", solo)
    for (stem in c("rnm", "lnm")) {
      expect_match(gsub("\\s+", " ", as.character(output[[paste0(stem, "_line")]]$html)), expected, fixed = TRUE, info = stem)
    }
    # The item network models every included item and names nothing.
    expect_false(grepl("not in the model", as.character(output$ggm_line$html), fixed = TRUE))
    session$setInputs(rnm_go = 1)
    r <- demands$rnm$status()$value
    expect_false(solo %in% r$spec$observed)
    expect_match(gsub("\\s+", " ", as.character(output$rnm_line$html)), expected, fixed = TRUE)
  })
  expect_identical(sieve_network_not_modeled_text(NULL), character(0))
  two <- data.frame(item = c("a", "b", "c"), construct = c(NA, "X", "X"),
    reason = c("no construct is assigned", rep("its construct has fewer than 3 included items", 2)),
    stringsAsFactors = FALSE)
  expect_identical(sieve_network_not_modeled_text(two), c(
    "An item is not in the model: no construct is assigned (a).",
    "Items of construct 'X' are not in the model: its construct has fewer than 3 included items (b, c)."
  ))
})

test_that("each edge table says what its intervals cover, and the DWLS interval is labeled by its own family", {
  html <- paste(as.character(mod_network_ui("network")), collapse = "")
  caption <- "The intervals are Wald intervals for the model the search selected; they do not include the uncertainty of the selection itself."
  expect_length(gregexpr(caption, html, fixed = TRUE)[[1]], 3L)
  st <- network_state()
  items <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  ord <- as.data.frame(lapply(items, function(v) as.integer(cut(as.numeric(v), breaks = 5))))
  names(ord) <- names(items)
  meta$type <- "ordered"
  r <- sieve_network_estimate(ord, meta, kind = "network_ggm")
  m <- r$fit$selected
  expect_identical(attr(m, "family"), "scaled")
  expect_identical(attr(m, "rmsea_ci_family"), "standard")
  # (The interval is at the RMSEA level, 90%, computed by DMAR from
  # psychonetrics' statistic; its family is the standard one under DWLS.)
  expect_identical(sieve_fit_label(m, c("rmsea", "rmsea_lo", "rmsea_hi")),
    c("RMSEA (scaled)", "RMSEA 90% CI lower (standard)", "RMSEA 90% CI upper (standard)"))
  expect_match(sieve_network_method_text(r), "RMSEA (scaled) ", fixed = TRUE)
  expect_match(sieve_network_method_text(r), ", 90% CI of the standard RMSEA [", fixed = TRUE)
  # Measures that carry no interval family (every lavaan fit) keep the
  # index's family on the interval.
  v <- c(rmsea = .05, rmsea_lo = .03, rmsea_hi = .07)
  attr(v, "family") <- "robust"
  attr(v, "ci_level") <- 0.95
  expect_identical(sieve_fit_label(v, c("rmsea", "rmsea_lo")), c("RMSEA (robust)", "RMSEA 95% CI lower (robust)"))
})

# On a real pool of 500 respondents by 68 ordered items with seven
# categories (data not in the package), every network model stops under
# DWLS with "system is computationally singular". The page says what
# happened and what can be done, not the engine's words alone.
test_that("a singular DWLS estimate says what it means and what to do, and other failures pass through", {
  dwls <- sieve_network_engine("WLSMV", "listwise", c("a", "b"), NULL)
  msg <- sieve_network_error_text("system is computationally singular: reciprocal condition number = 6.55139e-24", dwls, 68L)
  expect_match(msg, "psychonetrics could not estimate this network under DWLS from the polychoric correlations of these 68 ordered items", fixed = TRUE)
  expect_match(msg, "(system is computationally singular: reciprocal condition number = 6.55139e-24)", fixed = TRUE)
  expect_match(msg, "treat the items as continuous with this page's Estimator control", fixed = TRUE)
  expect_match(msg, "Rhemtulla, Brosseau-Liard & Savalei, 2012", fixed = TRUE)
  # On that pool the search under ML did not finish within an hour, so the
  # note does not leave the continuous treatment as the only way.
  expect_match(msg, "its time grows quickly with the number of items; a network of fewer items is much faster to estimate.", fixed = TRUE)
  ml <- sieve_network_engine("ML", "listwise", NULL, NULL)
  expect_identical(sieve_network_error_text("system is computationally singular", ml, 9L), "system is computationally singular")
  expect_identical(sieve_network_error_text("some other failure", dwls, 9L), "some other failure")
})

# ---- 8. The search in a separate R process, its Cancel, and its wait -------

# A worker that stays alive until it is stopped, for the running state.
network_slow_start <- function(data, meta, kind, estimator = "auto", alpha = 0.01) {
  sieve_network_handle(callr::r_bg(function() {
    Sys.sleep(300)
    NULL
  }, supervise = TRUE, stdout = NULL, stderr = NULL))
}
# The residual network in a slow worker, the other models here.
network_mixed_start <- function(data, meta, kind, estimator = "auto", alpha = 0.01) {
  if (identical(kind, "network_rnm")) {
    return(network_slow_start(data, meta, kind, estimator, alpha))
  }
  sieve_network_inline_start(data, meta, kind, estimator, alpha)
}
# Waits for a search's process to end. One still running at the limit (by
# default 600 seconds, which allows for a busy machine) fails the test and
# is stopped, so it does not load the machine for later tests. Returns TRUE
# when the process ended.
network_wait_dead <- function(handle, seconds = 600) {
  ended <- siever_wait_until(function() !isTRUE(handle$is_alive()), "The network search's R process",
    seconds = seconds, step = function() Sys.sleep(0.2))
  if (!ended) handle$kill()
  ended
}

test_that("the search's wait follows its measured times, and 68 items are estimated at hours, not seconds", {
  local_measured_rates()
  sieve_rate_reset()
  on.exit(sieve_rate_reset(), add = TRUE)
  tab <- sieve_network_timings()
  expect_true(all(c("network_ggm", "network_rnm", "network_lnm") %in% tab$kind))
  # At each measured size the reference is the time measured.
  for (i in seq_len(nrow(tab))) {
    expect_equal(sieve_network_reference_seconds(tab$kind[i], tab$class[i], tab$p[i]), tab$seconds[i],
      tolerance = 1e-12, info = paste(tab$kind[i], tab$class[i], tab$p[i])
    )
  }
  ml <- sieve_network_engine("ML", "listwise", NULL, NULL)
  sizes <- c(9, 12, 18, 24, 30, 36, 42, 48, 60, 68)
  for (kind in c("network_ggm", "network_rnm", "network_lnm")) {
    s <- vapply(sizes, function(p) sieve_network_reference_seconds(kind, "ML", p), numeric(1))
    expect_true(all(diff(s) > 0), info = kind)
  }
  # The residual network of a private study's 68 items under ML runs
  # longer than an hour, where the CFA's estimate scaled by 10 would say
  # about 65 seconds.
  wait <- sieve_network_wait("network_rnm", 68L, ml, worker = TRUE)
  expect_gt(wait, 3600)
  expect_false(grepl("seconds", sieve_duration_label(wait), fixed = TRUE))
  # The start of the separate R process is added once, and only for it.
  expect_equal(wait - sieve_network_wait("network_rnm", 68L, ml, worker = FALSE),
    sieve_network_worker_seconds(), tolerance = 1e-12
  )
  # This machine's speed: an estimate measured at twice its reference time
  # doubles the next estimate of its model and class, and of no other.
  ref24 <- sieve_network_reference_seconds("network_rnm", "ML", 24L)
  sieve_network_wait_note("network_rnm", 24L, ml, engine_seconds = 2 * ref24, start_seconds = 3)
  expect_equal(sieve_network_wait("network_rnm", 36L, ml, worker = FALSE),
    2 * sieve_network_reference_seconds("network_rnm", "ML", 36L), tolerance = 1e-12
  )
  expect_equal(sieve_network_wait("network_ggm", 36L, ml, worker = FALSE),
    sieve_network_reference_seconds("network_ggm", "ML", 36L), tolerance = 1e-12
  )
  expect_equal(sieve_network_worker_seconds(), 3, tolerance = 1e-12)
  # The class is the engine's path, not the lavaan estimator it reports.
  expect_identical(sieve_network_wait_class(sieve_network_engine("WLSMV", "listwise", c("a", "b"), NULL)), "DWLS")
  expect_identical(sieve_network_wait_class(sieve_network_engine("MLR", "fiml", NULL, data.frame(a = 1:3))), "MLR")
  expect_identical(sieve_network_wait_class(sieve_network_engine("MLR", "fiml", NULL, data.frame(a = c(1, NA, 3)))), "FIML")
  expect_identical(sieve_network_wait_class(ml), "ML")
})

test_that("the page states the measured wait, and that a separate R process runs the search", {
  local_measured_rates()
  sieve_rate_reset()
  on.exit(sieve_rate_reset(), add = TRUE)
  st <- network_state()
  shiny::testServer(mod_network_server, args = list(state = st, start = network_start), {
    network_inputs(session)
    pl <- plans$rnm()
    expect_equal(pl$seconds, sieve_network_wait("network_rnm", pl$p, pl$engine, worker = FALSE), tolerance = 1e-12)
    expect_false(grepl("separate R process", pl$sentence, fixed = TRUE))
  })
  skip_if_not_installed("callr")
  shiny::testServer(mod_network_server, args = list(state = st, start = sieve_network_worker_start), {
    network_inputs(session)
    pl <- plans$rnm()
    expect_equal(pl$seconds, sieve_network_wait("network_rnm", pl$p, pl$engine, worker = TRUE), tolerance = 1e-12)
    expect_match(pl$sentence, "in a separate R process that can be canceled", fixed = TRUE)
  })
})

test_that("a search runs in a separate R process: the page stays usable, says what runs, and Cancel stops it", {
  skip_if_not_installed("callr")
  # The clock of the time so far is the test's, and the page's line output
  # is counted by its draws (each draw names the items left out once per
  # model): a tick redraws the time so far alone, never the line with its
  # Cancel button (the line is not redrawn every second).
  test_frame <- environment()
  clock <- new.env(parent = emptyenv())
  clock$seconds <- 0
  clock$draws <- 0L
  real_left_out <- sieve_network_not_modeled_text
  testthat::local_mocked_bindings(
    sieve_demand_elapsed = function(started) clock$seconds,
    sieve_network_not_modeled_text = function(...) {
      clock$draws <- clock$draws + 1L
      real_left_out(...)
    },
    .env = test_frame
  )
  box <- new.env()
  st <- network_state()
  shiny::testServer(mod_network_server, args = list(state = st, start = network_mixed_start), {
    network_inputs(session)
    session$setInputs(rnm_go = 1)
    s <- demands$rnm$status()
    expect_identical(s$state, "running")
    expect_identical(s$before, "not_run")
    box$first <- demands$rnm$job()$handle
    expect_true(box$first$is_alive())
    line <- as.character(output$rnm_line$html)
    expect_match(line, "The residual network is being estimated in a separate R process.", fixed = TRUE)
    expect_match(line, sprintf('id="%s"', session$ns("rnm_go_progress")), fixed = TRUE)
    expect_match(line, sprintf('id="%s"', session$ns("rnm_go_cancel")), fixed = TRUE)
    expect_identical(output$rnm_go_progress, "0.0 seconds so far.")
    drawn <- clock$draws
    for (i in 1:2) {
      clock$seconds <- i
      session$elapse(1000)
      expect_identical(output$rnm_go_progress, sprintf("%.1f seconds so far.", i))
      expect_identical(as.character(output$rnm_line$html), line)
      expect_identical(clock$draws, drawn)
    }
    # Pressing again while it runs starts nothing new.
    session$setInputs(rnm_go = 2)
    expect_identical(demands$rnm$job()$handle, box$first)
    # The other models do not wait for it.
    session$setInputs(ggm_go = 1)
    expect_identical(demands$ggm$status()$state, "current")
    expect_identical(demands$rnm$status()$state, "running")
    # Cancel stops the process, and nothing is kept or recorded.
    session$setInputs(rnm_go_cancel = 1)
    expect_true(network_wait_dead(box$first, 10))
    expect_null(demands$rnm$job())
    expect_identical(demands$rnm$status()$state, "not_run")
    kinds_recorded <- vapply(shiny::isolate(state$analyses), function(a) a$kind %||% "", character(1))
    expect_false("network_rnm" %in% kinds_recorded)
    expect_true("network_ggm" %in% kinds_recorded)
    # A request with other inputs replaces the one running.
    session$setInputs(rnm_go = 3)
    box$second <- demands$rnm$job()$handle
    session$setInputs(alpha = 0.05)
    session$setInputs(rnm_go = 4)
    expect_true(network_wait_dead(box$second, 10))
    box$third <- demands$rnm$job()$handle
    expect_true(box$third$is_alive())
    expect_equal(demands$rnm$job()$key$choices$alpha, 0.05)
  })
  # The session's end stops the process still running.
  expect_true(network_wait_dead(box$third, 10))
})

test_that("a search made in the separate R process is the one made here, with its records", {
  local_measured_rates()
  skip_if_not_installed("callr")
  sieve_rate_reset()
  on.exit(sieve_rate_reset(), add = TRUE)
  st <- network_state()
  shiny::testServer(mod_network_server, args = list(state = st, start = sieve_network_worker_start), {
    network_inputs(session)
    session$setInputs(ggm_go = 1)
    expect_identical(demands$ggm$status()$state, "running")
    handle <- demands$ggm$job()$handle
    # The result is read only from a process that ended.
    if (network_wait_dead(handle)) {
      # The page collects the result at its next look (twice a second).
      session$elapse(600)
      s <- demands$ggm$status()
      expect_identical(s$state, "current")
      direct <- sieve_network_estimate(shiny::isolate(state$items), shiny::isolate(state$meta),
        kind = "network_ggm", estimator = estimator_choice(), alpha = 0.01
      )
      r <- s$value
      expect_equal(r$omega, direct$omega, tolerance = 1e-10)
      expect_identical(r$spec$key, direct$spec$key)
      expect_false(is.null(r$analysis_id))
      expect_true(any(grepl("Item network:", shiny::isolate(state$codelog), fixed = TRUE)))
      expect_true(is.list(r$code_view))
      # This machine's speed and the process's start are measured.
      expect_true(is.finite(sieve_rate(sprintf("network_ggm_%s", sieve_network_wait_class(r$spec$engine)))))
      expect_true(is.finite(sieve_rate("network_worker")))
    }
  })
})

test_that("a search whose data change while it runs is kept as from before, recorded, and not written to the script", {
  skip_if_not_installed("callr")
  st <- network_state()
  shiny::testServer(mod_network_server, args = list(state = st, start = sieve_network_worker_start), {
    network_inputs(session)
    session$setInputs(ggm_go = 1)
    handle <- demands$ggm$job()$handle
    m <- shiny::isolate(state$meta)
    m$status[m$item == m$item[9]] <- "excluded"
    state$meta <- m
    session$flushReact()
    # The result is read only from a process that ended.
    if (network_wait_dead(handle)) {
      session$elapse(600)
      s <- demands$ggm$status()
      expect_identical(s$state, "running")
      expect_false(is.null(s$value$analysis_id))
      expect_null(s$value$code_view)
      expect_false(any(grepl("Item network:", shiny::isolate(state$codelog), fixed = TRUE)))
      # The record names the nine items it was estimated from.
      rec <- Filter(function(a) identical(a$kind, "network_ggm"), shiny::isolate(state$analyses))[[1L]]
      expect_length(rec$spec$observed, 9L)
    }
  })
})

test_that("items read from a .sav file reach the separate R process as numbers", {
  skip_if_not_installed("callr")
  skip_if_not_installed("haven")
  # Sent as they are, haven_labelled columns stop the estimate in a process
  # where haven is not loaded ("Can't convert <haven_labelled> to
  # <double>"); the page sends the included items as numbers.
  st <- network_state()
  x <- shiny::isolate(st$items)
  first <- names(x)[1L]
  x[[first]] <- haven::labelled(x[[first]], labels = c(lowest = min(x[[first]], na.rm = TRUE)))
  st$items <- x
  shiny::testServer(mod_network_server, args = list(state = st, start = sieve_network_worker_start), {
    network_inputs(session)
    session$setInputs(ggm_go = 1)
    # The result is read only from a process that ended.
    if (network_wait_dead(demands$ggm$job()$handle)) {
      session$elapse(600)
      s <- demands$ggm$status()
      expect_identical(s$state, "current")
      expect_null(s$value$error)
    }
  })
})

test_that("the page's Update starts a search in the separate R process, says so, and keeps the result from before on the page", {
  skip_if_not_installed("callr")
  test_frame <- environment()
  notes <- character(0)
  testthat::local_mocked_bindings(
    sieve_notify = function(message, type = "message", duration = 8) {
      notes <<- c(notes, message)
      invisible(NULL)
    },
    .env = test_frame
  )
  # The first estimate here, every later one in a worker that stays alive.
  calls <- 0L
  first_here <- function(data, meta, kind, estimator = "auto", alpha = 0.01) {
    calls <<- calls + 1L
    if (calls == 1L) {
      return(sieve_network_inline_start(data, meta, kind, estimator, alpha))
    }
    network_slow_start(data, meta, kind, estimator, alpha)
  }
  box <- new.env()
  st <- network_state()
  shiny::testServer(mod_network_server, args = list(state = st, start = first_here), {
    network_inputs(session)
    session$setInputs(rnm_go = 1)
    expect_identical(demands$rnm$status()$state, "current")
    sieve_set_case_status(state, 1L, "excluded", "Review test")
    session$flushReact()
    expect_identical(demands$rnm$status()$state, "stale")
    notes <<- character(0)
    session$setInputs(update_go = 1)
    s <- demands$rnm$status()
    expect_identical(s$state, "running")
    expect_identical(s$before, "stale")
    box$handle <- demands$rnm$job()$handle
    expect_true(any(grepl("Started the residual network in a separate R process", notes, fixed = TRUE)))
    expect_false(any(grepl("Not brought up to date", notes, fixed = TRUE)))
    # The result from before stays, marked as such, while the update runs.
    line <- as.character(output$rnm_line$html)
    expect_match(line, "The result below is from before", fixed = TRUE)
    expect_false(is.null(solved("rnm")))
    expect_false(is.null(shown("rnm")$stale))
    # Canceled, it is the result from before again.
    session$setInputs(rnm_go_cancel = 1)
    expect_true(network_wait_dead(box$handle, 10))
    expect_identical(demands$rnm$status()$state, "stale")
  })
})

test_that("a search still running for the inputs before a change is named on the line, with Cancel", {
  skip_if_not_installed("callr")
  box <- new.env()
  st <- network_state()
  shiny::testServer(mod_network_server, args = list(state = st, start = network_mixed_start), {
    network_inputs(session)
    session$setInputs(rnm_go = 1)
    box$handle <- demands$rnm$job()$handle
    m <- shiny::isolate(state$meta)
    m$status[m$item == m$item[9]] <- "excluded"
    state$meta <- m
    session$flushReact()
    # The line is for the inputs now, which have no result, and says that
    # the search for the inputs before is still running.
    s <- demands$rnm$status()
    expect_identical(s$state, "not_run")
    expect_false(is.null(s$elsewhere))
    line <- as.character(output$rnm_line$html)
    expect_match(line, "for the inputs before the change is still running in a separate R process", fixed = TRUE)
    expect_match(line, sprintf('id="%s"', session$ns("rnm_go_cancel")), fixed = TRUE)
    session$setInputs(rnm_go_cancel = 1)
    expect_true(network_wait_dead(box$handle, 10))
    expect_null(demands$rnm$status()$elsewhere)
  })
})

# ---- 9. The optimizer's verdict -------------------------------------------

# A nonconverged fit: the nine tests' item network from starting values
# halved, with the optimizer allowed one iteration.
network_unfinished <- function() {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  net <- psychonetrics::ggm(d, vars = names(d), omega = "full")
  free <- !net@parameters$fixed
  net@parameters$est[free] <- net@parameters$est[free] * .5
  net <- psychonetrics::setoptimizer(net, "nlminb", optim.args = list(control = list(iter.max = 1, eval.max = 2)))
  suppressWarnings(psychonetrics::runmodel(net))
}

network_hs9_meta <- function(d) {
  m <- sieve_build_meta(d, names(d), list(Visual = names(d)[1:3], Textual = names(d)[4:6], Speed = names(d)[7:9]))
  m$type <- "continuous"
  m
}

test_that("a fit whose optimizer stopped is not a solution, whatever its estimates", {
  net <- network_unfinished()
  expect_identical(as.integer(net@optim$convergence), 1L)
  s <- sieve_network_status(net, warnings = "Model might not have converged properly: mean(abs(gradient)) > 1.")
  expect_identical(s$code, "not_converged")
  expect_false(sieve_has_solution(s))
  expect_false(s$converged)
  expect_match(s$message, "The optimizer did not converge", fixed = TRUE)
  expect_match(s$message, net@optim$message, fixed = TRUE)
  expect_identical(s$optimizer$code, 1L)
})

test_that("a fit with finite estimates and standard errors is still refused when its optimizer reports no convergence", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:3)]
  net <- suppressWarnings(psychonetrics::runmodel(psychonetrics::ggm(d, vars = names(d))))
  expect_identical(sieve_network_status(net)$code, "ok")
  pars <- sieve_network_quiet(psychonetrics::parameters(net))
  expect_true(all(is.finite(pars$est)) && all(is.finite(pars$se[!pars$fixed])))
  net@optim$convergence <- 1L
  net@optim$message <- "Controlled nonconvergence"
  s <- sieve_network_status(net)
  expect_identical(s$code, "not_converged")
  expect_match(s$message, "code 1: Controlled nonconvergence", fixed = TRUE)
})

test_that("the service reports no network when the search's model did not converge", {
  test_frame <- environment()
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  m <- network_hs9_meta(d)
  bad <- network_unfinished()
  testthat::local_mocked_bindings(sieve_network_search = function(...) bad, .env = test_frame)
  r <- sieve_network_estimate(d, m, kind = "network_ggm", estimator = "ml")
  expect_identical(r$status$code, "not_converged")
  expect_null(r$edges)
  expect_null(r$omega)
})

test_that("a stage of the search that did not converge refuses the network, and a warning inside the search qualifies it", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:3)]
  net <- suppressWarnings(psychonetrics::runmodel(psychonetrics::ggm(d, vars = names(d))))
  stage_table <- function(code = c(0L, 0L, 0L), gradient = c(FALSE, FALSE, FALSE),
                          stage = c("saturated", "pruned", "stepped_up")) {
    data.frame(stage = stage, code = code, message = "", gradient_warning = gradient, stringsAsFactors = FALSE)
  }
  # The saturated model stopped: the edges pruned from it rest on it.
  attr(net, "sieve_search_optimizer") <- stage_table(code = c(1L, 0L, 0L))
  s <- sieve_network_status(net)
  expect_identical(s$code, "not_converged")
  expect_match(s$message, "the saturated model of the search", fixed = TRUE)
  expect_match(s$message, "rest on an unfinished estimate", fixed = TRUE)
  # A large-gradient warning inside the step-up qualifies the result.
  attr(net, "sieve_search_optimizer") <- stage_table(gradient = c(FALSE, FALSE, TRUE))
  s <- sieve_network_status(net)
  expect_identical(s$code, "ok")
  expect_match(s$message, "carries that caution", fixed = TRUE)
  # After the MLR refit, the one estimation it is, the warning refuses it.
  attr(net, "sieve_search_optimizer") <- stage_table(
    code = c(0L, 0L, 0L, 0L), gradient = c(FALSE, FALSE, FALSE, TRUE),
    stage = c("saturated", "pruned", "stepped_up", "refit")
  )
  s <- sieve_network_status(net)
  expect_identical(s$code, "not_converged")
  expect_match(s$message, "the MLR refit", fixed = TRUE)
})

test_that("a default search records every stage's verdict, and each converged", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  m <- network_hs9_meta(d)
  r <- sieve_network_estimate(d, m, kind = "network_ggm", estimator = "ml")
  expect_identical(r$status$code, "ok")
  expect_identical(r$status$optimizer$stage, c("saturated", "pruned", "stepped_up"))
  expect_true(all(r$status$optimizer$code == 0L))
  expect_match(r$status$message, "The optimizer converged for every model of the search", fixed = TRUE)
  r <- sieve_network_estimate(d, m, kind = "network_ggm", estimator = "mlr")
  expect_identical(r$status$optimizer$stage, c("saturated", "pruned", "stepped_up", "refit"))
  expect_true(all(r$status$optimizer$code == 0L))
})

test_that("network planning respects SPSS user missing values before estimation", {
  st <- network_state()
  shiny::isolate({
    d <- st$items
    nm <- names(d)[1L]
    d[[nm]][1L] <- 999
    d[[nm]] <- haven::labelled_spss(d[[nm]], na_values = 999)
    st$items <- d
  })
  shiny::testServer(mod_network_server, args = list(state = st, start = network_start), {
    network_inputs(session)
    planned <- model_plan("ggm")()
    # The default, ML with FIML: the user missing value is
    # missing, so the data are incomplete and FIML reads them as such.
    actual <- sieve_network_engine("ML", "fiml", character(),
      sieve_network_data(shiny::isolate(st$items), active_items()))
    expect_identical(planned$engine, actual)
    expect_identical(planned$engine$estimator, "FIML")
    expect_false(isTRUE(planned$engine$complete))
    expect_identical(sieve_network_wait_class(planned$engine), "FIML")
    expect_null(planned$fiml_note)
    # MLR asked for on the same data: the note says the robust correction
    # is not applied to incomplete data.
    network_inputs(session, estimator = "mlr")
    planned <- model_plan("ggm")()
    actual <- sieve_network_engine("MLR", "fiml", character(),
      sieve_network_data(shiny::isolate(st$items), active_items()))
    expect_identical(planned$engine, actual)
    expect_identical(planned$engine$estimator, "FIML")
    expect_match(planned$fiml_note, "normal theory standard errors", fixed = TRUE)
  })
})


test_that("Cancel honors an automatic inclusion refit and the next inclusion change can refit again", {
  skip_if_not_installed("callr")
  calls <- 0L
  first_here <- function(data, meta, kind, estimator = "auto", alpha = 0.01) {
    calls <<- calls + 1L
    if (calls == 1L) return(sieve_network_inline_start(data, meta, kind, estimator, alpha))
    network_slow_start(data, meta, kind, estimator, alpha)
  }
  state <- network_state()
  shiny::testServer(mod_network_server, args = list(state = state, start = first_here), {
    network_inputs(session)
    session$setInputs(rnm_go = 1L)
    item <- state$meta$item[9L]
    sieve_set_item_status(state, item, "parked", "Review test")
    session$flushReact()
    expect_identical(calls, 2L)
    expect_identical(demands$rnm$status()$state, "running")
    session$setInputs(rnm_go_cancel = 1L)
    session$elapse(1000L)
    expect_identical(calls, 2L)
    expect_identical(demands$rnm$status()$state, "stale")
    sieve_set_item_status(state, item, "active", "Review test")
    session$flushReact()
    sieve_set_item_status(state, item, "parked", "Review test")
    session$flushReact()
    expect_identical(calls, 3L)
    expect_identical(demands$rnm$status()$state, "running")
    session$setInputs(rnm_go_cancel = 2L)
    session$elapse(1000L)
    expect_identical(calls, 3L)
  })
})
