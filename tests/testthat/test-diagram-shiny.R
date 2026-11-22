# The path diagram in the app: the CFA and SEM tabs, their
# display controls, and their downloads, all reading one specification.
#
# The module servers are driven with shiny::testServer. What the screen
# shows is the reactive specification the modules expose (`diagram$spec`);
# what a download writes is what sieve_export_diagram() returns for it,
# which is recorded here by wrapping that function. Both are compared with
# a specification built in plain R from the same fitted model, and that
# model is compared with lavaan.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

# A loaded, confirmed state on the primary nine Holzinger and Swineford
# tests: what the analysis tabs see after a normal ingest.
ds_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = TRUE, name = fx$name)
  s
}

ds_text <- function(x) paste(as.character(unlist(x)), collapse = " ")

# Count the calls of the two functions that must run once per fitted model
# and never because a display control moved, and keep what each export
# returned.
ds_watch <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$fits <- 0L
  seen$sems <- 0L
  seen$specs <- 0L
  seen$written <- list()
  run_cfa <- sieve_run_cfa
  run_structural <- sieve_run_structural
  build <- sieve_diagram_spec
  export <- sieve_export_diagram
  testthat::local_mocked_bindings(
    sieve_run_cfa = function(...) {
      seen$fits <- seen$fits + 1L
      run_cfa(...)
    },
    # The SEM tab fits through the structural run, which returns the result
    # record (fit, status, conditions) the tab reads. That is the one call
    # counted; the wrappers that only return the fit go through it.
    sieve_run_structural = function(...) {
      # The tab also fits the labeled model that defines the indirect and
      # total effects (effects = TRUE), which is a separate analysis and not
      # the model the diagram shows; only the diagram's model is counted.
      if (!isTRUE(list(...)$effects)) {
        seen$sems <- seen$sems + 1L
      }
      run_structural(...)
    },
    sieve_diagram_spec = function(...) {
      seen$specs <- seen$specs + 1L
      build(...)
    },
    sieve_export_diagram = function(...) {
      out <- export(...)
      seen$written[[length(seen$written) + 1L]] <- out
      invisible(out)
    },
    .env = env
  )
  seen
}

ds_visible_edges <- function(spec) {
  e <- spec$edges[spec$edges$visible, c("id", "type", "from", "to")]
  rownames(e) <- NULL
  e
}

test_that("CFA: the covariance and residual controls change what is drawn and never refit", {
  seen <- ds_watch()
  st <- ds_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone")
    session$setInputs(fit_go = 1)
    full <- diagram$spec()
    expect_s3_class(full, "sieve_diagram")
    expect_identical(full$status$code, "ok")
    expect_null(diagram$problem())
    f <- fit()
    coef_before <- lavaan::coef(f)
    expect_identical(seen$fits, 1L)
    # Two specifications per fit: one for the model not fit yet (it says what
    # to press), one for the fit.
    expect_identical(seen$specs, 2L)
    # The default view is the complete model, fully standardized, top-down,
    # with the item labels and the analysis identifier of the fit.
    expect_identical(full$meta$standardized, "std.all")
    expect_identical(full$meta$layout, "top_down")
    expect_true(all(full$edges$visible[full$edges$type %in%
      c("loading", "covariance", "variance", "residual_path")]))
    expect_identical(
      full$nodes$label[full$nodes$id == "t1_visual_perception"],
      "Visual perception"
    )
    expect_identical(full$meta$analysis_id, cfa_run()$analysis_id)
    expect_match(full$meta$caption, sprintf("Analysis %s.", cfa_run()$analysis_id), fixed = TRUE)

    # The factor covariance control: exactly the exogenous covariances
    # leave the drawing.
    session$setInputs(covs = FALSE)
    no_cov <- diagram$spec()
    gone <- setdiff(ds_visible_edges(full)$id, ds_visible_edges(no_cov)$id)
    expect_setequal(gone, c("Visual~~Textual", "Visual~~Speed", "Textual~~Speed"))
    expect_identical(gone, full$edges$id[full$edges$type == "covariance" &
      full$edges$cov_class %in% "exogenous"])
    expect_identical(
      setdiff(ds_visible_edges(no_cov)$id, ds_visible_edges(full)$id),
      character(0)
    )
    expect_true(all(no_cov$edges$hidden_reason[match(gone, no_cov$edges$id)] == "covariances"))
    expect_match(no_cov$meta$caption, "3 covariances between exogenous variables", fixed = TRUE)
    # Hidden, not removed: the parameters, their values, and the nodes stay.
    expect_identical(no_cov$parameters, full$parameters)
    expect_identical(
      no_cov$edges[, c("id", "est", "se", "ci_lower", "ci_upper")],
      full$edges[, c("id", "est", "se", "ci_lower", "ci_upper")]
    )
    expect_identical(
      no_cov$nodes[, c("id", "x", "y", "w", "h")],
      full$nodes[, c("id", "x", "y", "w", "h")]
    )
    session$setInputs(covs = TRUE)
    expect_identical(diagram$spec(), full)

    # The unique term control: the residual nodes leave, the residual
    # variances stay in the model.
    session$setInputs(resid = FALSE)
    bare <- diagram$spec()
    expect_false(any(bare$nodes$visible[bare$nodes$kind == "residual"]))
    expect_true(all(bare$nodes$visible[bare$nodes$kind != "residual"]))
    expect_identical(sum(bare$nodes$kind == "residual"), 9L)
    hidden <- bare$edges[!bare$edges$visible & full$edges$visible, ]
    expect_setequal(unique(hidden$type), c("residual_path", "variance"))
    expect_identical(nrow(hidden), 18L)
    expect_true(all(hidden$hidden_reason == "residuals"))
    expect_identical(bare$parameters, full$parameters)
    pe <- lavaan::parameterEstimates(f)
    theta <- pe[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% names(st$items), ]
    kept <- bare$parameters[bare$parameters$type == "variance" &
      bare$parameters$lhs %in% names(st$items), ]
    expect_identical(kept$est, theta$est)
    expect_identical(nrow(kept), 9L)
    session$setInputs(resid = TRUE)

    # Every other display control reaches the specification.
    session$setInputs(what = "none")
    expect_identical(diagram$spec()$meta$standardized, "none")
    expect_identical(diagram$spec()$edges$value, diagram$spec()$edges$est)
    session$setInputs(what = "std.lv")
    expect_identical(diagram$spec()$meta$standardized, "std.lv")
    session$setInputs(what = "std.all", vars = FALSE)
    expect_false(any(diagram$spec()$edges$visible[diagram$spec()$edges$type == "variance"]))
    session$setInputs(vars = TRUE, rescovs = FALSE)
    expect_false(diagram$spec()$meta$options$residual_covariances)
    session$setInputs(rescovs = TRUE, intercepts = TRUE)
    expect_true(any(diagram$spec()$edges$visible[diagram$spec()$edges$type == "intercept"]))
    session$setInputs(intercepts = FALSE, fixed_zero = TRUE)
    expect_true(diagram$spec()$meta$options$fixed_zero)
    session$setInputs(fixed_zero = FALSE, min_path = 0.5)
    cut <- diagram$spec()
    low <- abs(full$edges$value) < 0.5 & full$edges$type == "loading"
    expect_identical(
      cut$edges$id[!cut$edges$visible & cut$edges$type == "loading"],
      full$edges$id[low]
    )
    expect_gt(sum(low), 0L)
    # The loading threshold applies to loadings only.
    expect_true(all(cut$edges$visible[cut$edges$type %in% c(
      "covariance", "variance",
      "residual_path"
    )]))
    session$setInputs(min_path = 0, labels = "short")
    expect_identical(
      diagram$spec()$nodes$label[diagram$spec()$nodes$kind == "observed"],
      paste0("V", 1:9)
    )
    expect_match(ds_text(output$diagram_key), "Visual perception", fixed = TRUE)
    session$setInputs(labels = "names", layout = "left_right")
    expect_identical(diagram$spec()$meta$layout, "left_right")
    session$setInputs(layout = "top_down", mono = TRUE)
    expect_true(diagram$draw_args()$monochrome)
    session$setInputs(mono = FALSE)
    expect_identical(diagram$spec(), full)
    # A value the controls do not offer (a restored session can hold one)
    # is read as the default, not passed on as an option.
    session$setInputs(what = "std", layout = "tree2", labels = "abbreviated")
    expect_identical(diagram$spec(), full)
    session$setInputs(what = "std.all", layout = "top_down", labels = "names")

    # Not one of those controls fit a model or rebuilt the specification
    # from the fitted model, and the fitted model is the one it was.
    expect_identical(seen$fits, 1L)
    expect_identical(seen$specs, 2L)
    expect_identical(lavaan::coef(fit()), coef_before)

    # The controls that define the model make it a model not fit yet, and
    # the button refits it, once.
    session$setInputs(ident = "marker")
    session$setInputs(fit_go = 2)
    marker <- diagram$spec()
    expect_identical(seen$fits, 2L)
    expect_identical(seen$specs, 4L)
    expect_identical(marker$meta$identification, "marker")
    first <- marker$edges[marker$edges$id == "Visual=~t1_visual_perception", ]
    expect_true(first$fixed)
    expect_lt(first$value, 1)
    session$setInputs(what = "none")
    expect_identical(diagram$spec()$edges$label_text[
      diagram$spec()$edges$id == "Visual=~t1_visual_perception"
    ], "1")
    expect_identical(seen$fits, 2L)
  })
})

test_that("CFA: the header states the estimator and what the identification does", {
  st <- ds_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone")
    note <- ds_text(output$model_note)
    expect_match(note, "maximum likelihood (ML) with full information maximum likelihood (FIML)", fixed = TRUE)
    expect_match(note, "full information maximum likelihood", fixed = TRUE)
    expect_match(note, "does not standardize the observed items", fixed = TRUE)
    expect_no_match(note, "free and standardized", fixed = TRUE)
    expect_identical(sieve_lavaan_spec(st$meta)$estimator, "ML")
    session$setInputs(ident = "marker")
    expect_match(ds_text(output$model_note), "fixing the loading of the first item to one",
      fixed = TRUE
    )
    # Ordered items change the estimator, and the sentence follows.
    m <- shiny::isolate(st$meta)
    m$type[1:3] <- "ordered"
    st$meta <- m
    session$flushReact()
    note <- ds_text(output$model_note)
    expect_identical(sieve_lavaan_spec(shiny::isolate(st$meta))$estimator, "WLSMV")
    expect_match(note, "WLSMV", fixed = TRUE)
    expect_match(note, "on the complete cases", fixed = TRUE)
    expect_match(note, "lavaan has no FIML for this estimator", fixed = TRUE)
  })
})

test_that("CFA: screen, plain R, and the three downloads show one specification", {
  seen <- ds_watch()
  st <- ds_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", covs = FALSE, what = "std.all",
      labels = "short"
    )
    session$setInputs(fit_go = 1)
    shown <- diagram$spec()
    f <- fit()

    # Plain R: the same options applied to the same fitted model.
    in_r <- sieve_diagram_spec(
      f,
      standardized = "std.all", covariances = FALSE, label_mode = "short",
      layout = "top_down", labels = sieve_hs9_fixture()$labels,
      name_map = cfa_run()$spec$name_map,
      analysis_id = cfa_run()$analysis_id
    )
    expect_identical(in_r$nodes, shown$nodes)
    expect_identical(in_r$edges, shown$edges)
    expect_identical(in_r$parameters, shown$parameters)
    expect_identical(in_r$meta$options, shown$meta$options)
    expect_identical(in_r$meta$caption, shown$meta$caption)
    # And the model itself is lavaan's: estimates and standardized values.
    pe <- lavaan::parameterEstimates(f)
    ss <- lavaan::standardizedSolution(f, type = "std.all")
    rows <- shown$edges[!is.na(shown$edges$par), ]
    key <- function(d) paste(d$lhs, d$op, d$rhs)
    expect_identical(rows$est, pe$est[match(key(rows), key(pe))])
    expect_equal(rows$value, ss$est.std[match(key(rows), key(ss))], tolerance = 1e-12)

    files <- c(pdf = output$dl_diagram, svg = output$dl_diagram_svg, png = output$dl_diagram_png)
    expect_true(all(file.exists(files)))
    expect_true(all(file.size(files) > 1000))
    expect_identical(rawToChar(readBin(files[["pdf"]], "raw", 4L)), "%PDF")
    expect_identical(as.integer(readBin(files[["png"]], "raw", 4L)), c(137L, 80L, 78L, 71L))
    expect_true(any(grepl("<svg", readLines(files[["svg"]], warn = FALSE), fixed = TRUE)))
    expect_identical(basename(files[["pdf"]]), "siever_path_diagram.pdf")

    # Each file was written from the specification on the screen: the same
    # nodes, the same edges with their ends and types, the same labels, and
    # the same options.
    expect_length(seen$written, 3L)
    expect_identical(vapply(seen$written, function(w) w$format, ""), c("pdf", "svg", "png"))
    for (w in seen$written) {
      expect_identical(w$drawn$nodes$id, shown$nodes$id[shown$nodes$visible], info = w$format)
      expect_identical(w$drawn$edges[, c("id", "type", "from", "to")], ds_visible_edges(shown),
        info = w$format
      )
      expect_identical(w$drawn$edges$label_text, shown$edges$label_text[shown$edges$visible],
        info = w$format
      )
      expect_false(any(w$drawn$edges$type == "covariance"), info = w$format)
      expect_identical(w$options, shown$meta$options, info = w$format)
      expect_identical(w$analysis_id, shown$meta$analysis_id, info = w$format)
      expect_identical(w$code, sieve_diagram_code(shown), info = w$format)
      # Sized to the content, not to a fixed page.
      expect_equal(w$width, max(shown$size$width, sieve_diagram_look()$caption_min),
        info = w$format
      )
      expect_gt(w$height, shown$size$height)
    }
    if (requireNamespace("pdftools", quietly = TRUE)) {
      text <- gsub("[[:space:]]+", " ", paste(pdftools::pdf_text(files[["pdf"]]), collapse = " "))
      for (label in c(
        "Visual", "Textual", "Speed", "V1", "V9", "Key: V1 = Visual perception",
        "Analysis"
      )) {
        expect_match(text, label, fixed = TRUE)
      }
      values <- unique(shown$edges$label_text[shown$edges$visible & shown$edges$type == "loading"])
      for (value in values) {
        expect_match(text, value, fixed = TRUE)
      }
    }

    # The download recorded the R that regenerates the figure, and that R
    # gives back the specification on the screen.
    log <- shiny::isolate(st$codelog)
    expect_true(any(grepl("Path diagram of the confirmatory factor model", log, fixed = TRUE)))
    expect_true(any(grepl("written as PDF", log, fixed = TRUE)))
    expect_true(any(grepl("SIEVER::sieve_export_diagram(diagram, \"siever_path_diagram.svg\")",
      log,
      fixed = TRUE
    )))
    code <- sieve_diagram_export_code(shown, "cfa_fit", "siever_path_diagram.pdf")
    expect_true(all(code %in% log))
    env <- new.env(parent = globalenv())
    assign("cfa_fit", f, envir = env)
    build <- sub("SIEVER::sieve_diagram_spec", "sieve_diagram_spec",
      sieve_diagram_code(shown, fit_name = "cfa_fit"),
      fixed = TRUE
    )
    assign("sieve_diagram_spec", sieve_diagram_spec, envir = env)
    eval(parse(text = paste(build, collapse = "\n")), envir = env)
    expect_identical(env$diagram$nodes, shown$nodes)
    expect_identical(env$diagram$edges, shown$edges)
    expect_identical(env$diagram$meta$caption, shown$meta$caption)

    # A size that is asked for is the size that is written.
    session$setInputs(fig_auto = FALSE, fig_width = 7, dl_caption = FALSE, mono = TRUE)
    invisible(output$dl_diagram)
    last <- seen$written[[length(seen$written)]]
    expect_identical(last$width, 7)
    expect_equal(last$height, 7 * shown$size$height / shown$size$width)
    expect_null(last$caption)
    expect_true(any(grepl("monochrome = TRUE, show_caption = FALSE, width = 7",
      shiny::isolate(st$codelog),
      fixed = TRUE
    )))
    expect_identical(seen$fits, 1L)
  })
  expect_null(grDevices::dev.list())
})

test_that("CFA: a model that cannot be fit gives a status alert and no download", {
  seen <- ds_watch()
  st <- ds_state()
  # One included item per construct: no construct can be measured.
  m <- shiny::isolate(st$meta)
  m$status <- ifelse(duplicated(m$construct), "parked", "active")
  st$meta <- m
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone")
    session$setInputs(fit_go = 1)
    expect_null(fit())
    s <- diagram$spec()
    expect_identical(s$status$code, "failed")
    expect_identical(nrow(s$nodes), 0L)
    expect_identical(nrow(s$edges), 0L)
    expect_s3_class(diagram$problem(), "sieve_diagram_error")
    alert <- ds_text(output$diagram_status)
    expect_match(alert, "No path diagram", fixed = TRUE)
    expect_match(alert, "alert-danger", fixed = TRUE)
    # The retained reason is shown, not a generic message.
    expect_match(alert, "The model is empty", fixed = TRUE)
    # No download control, no caption, no parameter table, and no plot.
    expect_error(output$diagram_downloads)
    expect_error(output$diagram_caption)
    expect_error(output$diagram_params)
    expect_error(output$diagram)
    # A request for the file fails and writes nothing that could pass for
    # a figure.
    for (format in c("pdf", "svg", "png")) {
      id <- c(pdf = "dl_diagram", svg = "dl_diagram_svg", png = "dl_diagram_png")[[format]]
      expect_error(output[[id]], class = "sieve_diagram_error")
    }
    expect_length(seen$written, 0L)
    expect_false(any(grepl("Path diagram", shiny::isolate(st$codelog), fixed = TRUE)))
  })
  expect_null(grDevices::dev.list())
})

test_that("CFA: a fit that did not converge is reported, not drawn", {
  st <- ds_state()
  real <- sieve_run_cfa
  # sieve_run_cfa() takes the session's fit store and the caller's page and
  # rows; the mock accepts them and fits without a store.
  testthat::local_mocked_bindings(sieve_run_cfa = function(spec, data, ...) {
    out <- real(spec, data)
    run <- sieve_capture(lavaan::cfa(spec$model, data[, spec$observed],
      std.lv = TRUE,
      control = list(iter.max = 2L)
    ))
    out$fit <- run$value
    attr(out$fit, "sieve_spec") <- spec
    out$warnings <- run$warnings
    out$status <- sieve_fit_status(out$fit, warnings = run$warnings)
    out
  })
  # The modification index panel of the same tab reacts to the fit as well,
  # and lavaan warns there that the model did not converge. That warning is
  # about that panel, not about the diagram, and is the only one let pass.
  other_panel <- function(w) {
    if (grepl("modindices", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  }
  withCallingHandlers(shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone")
    session$setInputs(fit_go = 1)
    expect_false(lavaan::lavInspect(fit(), "converged"))
    s <- diagram$spec()
    expect_identical(s$status$code, "not_converged")
    expect_identical(nrow(s$edges), 0L)
    alert <- ds_text(output$diagram_status)
    expect_match(alert, "No path diagram", fixed = TRUE)
    expect_match(alert, s$status$message, fixed = TRUE)
    expect_error(output$diagram_downloads)
    expect_error(output$dl_diagram, class = "sieve_diagram_error")
    # No display control turns the failure into a figure.
    session$setInputs(covs = FALSE, resid = FALSE, what = "none")
    expect_identical(diagram$spec()$status$code, "not_converged")
    expect_s3_class(diagram$problem(), "sieve_diagram_error")
  }), warning = other_panel)
})

test_that("SEM: loadings, regressions, and each kind of covariance are typed and controlled", {
  seen <- ds_watch()
  st <- ds_state()
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(outcomes = c("Speed", "age_years"), predictors = c("Visual", "Textual"))
    session$setInputs(fit_go = 1)
    s <- diagram$spec()
    f <- fit()
    expect_identical(s$status$code, "ok")
    expect_identical(s$meta$model_kind, "sem")
    expect_identical(s$meta$layout, "left_right")
    expect_identical(seen$sems, 1L)
    expect_identical(seen$specs, 1L)

    # Loadings and regressions are different edges.
    expect_identical(sum(s$edges$type == "loading"), 9L)
    reg <- s$edges[s$edges$type == "regression", ]
    expect_setequal(reg$id, c(
      "Speed~Visual", "Speed~Textual", "age_years~Visual",
      "age_years~Textual"
    ))
    expect_true(all(reg$from %in% c("Visual", "Textual")))
    expect_true(all(reg$to %in% c("Speed", "age_years")))

    # The covariance of the two predictors is an exogenous covariance,
    # drawn between the predictors.
    cov <- s$edges[s$edges$type == "covariance", ]
    exo <- cov[cov$cov_class == "exogenous", ]
    expect_identical(exo$id, "Visual~~Textual")
    expect_identical(c(exo$from, exo$to), c("Visual", "Textual"))
    # The covariance of the two outcomes is a covariance of disturbances,
    # drawn between the two disturbance terms.
    dist <- cov[cov$cov_class == "disturbance", ]
    expect_identical(dist$id, "Speed~~age_years")
    expect_identical(c(dist$from, dist$to), c("e.Speed", "e.age_years"))
    expect_identical(
      s$nodes$role[match(c("e.Speed", "e.age_years"), s$nodes$id)],
      c("disturbance", "disturbance")
    )
    expect_identical(nrow(cov), 2L)
    # Its value is the residual correlation lavaan reports for that
    # parameter, which is not the correlation of the two outcomes.
    ss <- lavaan::standardizedSolution(f, type = "std.all")
    residual_r <- ss$est.std[ss$lhs == "Speed" & ss$op == "~~" & ss$rhs == "age_years"]
    expect_equal(dist$value, residual_r, tolerance = 1e-12)
    implied <- lavaan::lavInspect(f, "cor.all")["Speed", "age_years"]
    expect_gt(abs(implied - residual_r), 0.01)
    expect_match(s$meta$caption, "residual covariances, not correlations of the variables",
      fixed = TRUE
    )
    # The outcomes have disturbances, not variances of their own.
    loops <- s$edges$from[s$edges$type == "variance"]
    expect_true(all(c("e.Speed", "e.age_years") %in% loops))
    expect_false(any(c("Speed", "age_years") %in% loops))
    # Readable names reach the figure; identifiers stay the model's.
    expect_identical(s$nodes$label[s$nodes$id == "t10_addition"], "Speeded addition")

    # Each covariance control hides its own class and only that.
    session$setInputs(covs = FALSE)
    off <- diagram$spec()
    expect_identical(off$edges$id[!off$edges$visible & s$edges$visible], "Visual~~Textual")
    session$setInputs(covs = TRUE, rescovs = FALSE)
    off <- diagram$spec()
    expect_identical(off$edges$id[!off$edges$visible & s$edges$visible], "Speed~~age_years")
    session$setInputs(rescovs = TRUE, resid = FALSE)
    bare <- diagram$spec()
    expect_false(any(bare$nodes$visible[bare$nodes$kind == "residual"]))
    # With the terms hidden the disturbance covariance leaves the figure
    # with them, keeps its row and its class, and is counted in the caption;
    # the covariance of the predictors stays.
    gone <- bare$edges[bare$edges$id == "Speed~~age_years", ]
    expect_false(gone$visible)
    expect_identical(c(gone$hidden_reason, gone$cov_class), c("residuals", "disturbance"))
    expect_true(bare$edges$visible[bare$edges$id == "Visual~~Textual"])
    expect_match(bare$meta$caption, "1 residual covariance between them", fixed = TRUE)
    expect_false(grepl("drawn between the variables", bare$meta$caption, fixed = TRUE))
    session$setInputs(resid = TRUE, min_path = 0.99)
    cut <- diagram$spec()
    # The loading threshold never hides a regression.
    expect_true(all(cut$edges$visible[cut$edges$type == "regression"]))
    expect_false(any(cut$edges$visible[cut$edges$type == "loading"]))
    session$setInputs(min_path = 0)
    expect_identical(seen$sems, 1L)
    expect_identical(seen$specs, 1L)

    # The downloads come from the specification on the screen.
    shown <- diagram$spec()
    files <- c(output$dl_diagram, output$dl_diagram_svg, output$dl_diagram_png)
    expect_true(all(file.exists(files)) && all(file.size(files) > 1000))
    expect_identical(basename(files[1]), "siever_structural_diagram.pdf")
    for (w in seen$written) {
      expect_identical(w$drawn$nodes$id, shown$nodes$id[shown$nodes$visible])
      expect_identical(w$drawn$edges[, c("id", "type", "from", "to")], ds_visible_edges(shown))
      expect_identical(w$options, shown$meta$options)
    }
    # The logged R names the model, the fit, and the diagram call.
    log <- shiny::isolate(st$codelog)
    expect_true(any(grepl("Speed ~ Visual + Textual", log, fixed = TRUE)))
    # Auto's estimator for continuous items is ML with FIML.
    expect_true(any(grepl("sem_fit <- lavaan::sem(sem_model, data = sem_data, estimator = \"ML\",",
      log,
      fixed = TRUE
    )))
    expect_true(all(sieve_diagram_code(shown, fit_name = "sem_fit") %in% log))
    expect_identical(seen$sems, 1L)
  })
  expect_null(grDevices::dev.list())
})

test_that("SEM: a structural model that cannot be fit gives a status alert and no download", {
  st <- ds_state()
  # The engine returns no fit: the result record says failed, with a reason.
  failed_run <- structure(
    list(
      spec = NULL, fit = NULL,
      status = SIEVER:::sieve_fit_status(NULL, error = "lavaan could not fit this model."),
      warnings = character(0), messages = character(0),
      error = "lavaan could not fit this model."
    ),
    class = "sieve_result"
  )
  testthat::local_mocked_bindings(sieve_run_structural = function(...) failed_run)
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(outcomes = "Speed", predictors = c("Visual", "Textual"))
    session$setInputs(fit_go = 1)
    expect_null(fit())
    expect_identical(diagram$spec()$status$code, "failed")
    expect_match(ds_text(output$diagram_status), "No path diagram", fixed = TRUE)
    expect_error(output$diagram_downloads)
    expect_error(output$dl_diagram, class = "sieve_diagram_error")
    expect_error(output$dl_diagram_png, class = "sieve_diagram_error")
  })
})

test_that("the module text states what each arrow means", {
  page_text <- function(ui) {
    gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", as.character(ui)))
  }
  cfa <- page_text(mod_cfa_ui("cfa"))
  sem <- page_text(mod_sem_ui("sem"))
  expect_match(sem, "measurement loading", fixed = TRUE)
  expect_match(sem, "structural regression", fixed = TRUE)
  expect_match(sem, "between the disturbance terms of two outcomes is a residual (disturbance) covariance",
    fixed = TRUE
  )
  expect_match(sem, "not the correlation of the outcomes themselves", fixed = TRUE)
  expect_match(sem, "between two predictors that no other variable in the model predicts is their covariance",
    fixed = TRUE
  )
  expect_match(sem, "does not by itself establish a causal direction", fixed = TRUE)
  expect_no_match(sem, "residual covariances among predictors", fixed = TRUE)
  expect_match(cfa, "Show factor covariances", fixed = TRUE)
  # Only the layouts of the engine that draws this notation are offered.
  for (ui in c(cfa, sem)) {
    expect_no_match(ui, "Spring", fixed = TRUE)
    expect_no_match(ui, "Circle", fixed = TRUE)
    expect_match(ui, "Top-down", fixed = TRUE)
    expect_match(ui, "Left-right", fixed = TRUE)
  }
  code <- unlist(lapply(
    c("mod_cfa_server", "mod_sem_server", "sieve_diagram_wire"),
    function(f) deparse(get(f))
  ))
  expect_false(any(grepl("semPaths", code, fixed = TRUE)))
})

test_that("a clean R session draws and exports with the exported functions only", {
  skip_on_cran()
  # The fresh process loads what this process is testing: the installed
  # package under R CMD check, the source tree under load_all().
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)
  from_source <- requireNamespace("pkgload", quietly = TRUE) && pkgload::is_dev_package("SIEVER")
  installed <- !from_source
  skip_if(
    from_source && !file.exists(file.path(root, "DESCRIPTION")),
    "the SIEVER source tree is not where the tests expect it"
  )
  dir <- withr::local_tempdir()
  script <- file.path(dir, "export.R")
  writeLines(c(
    if (installed) {
      "library(SIEVER)"
    } else {
      sprintf(
        "pkgload::load_all(%s, quiet = TRUE, export_all = FALSE, helpers = FALSE)",
        deparse(root)
      )
    },
    "hs <- DMAR::holzinger_swineford",
    "model <- 'visual =~ t1_visual_perception + t2_cubes + t4_lozenges",
    "          textual =~ t6_paragraph_comprehension + t7_sentence + t9_word_meaning'",
    "fit <- lavaan::cfa(model, data = hs, std.lv = TRUE)",
    "diagram <- sieve_diagram_spec(fit, analysis_id = 'clean-session')",
    "plain <- sieve_diagram_update(diagram, covariances = FALSE)",
    "for (ext in c('pdf', 'svg', 'png')) {",
    "  out <- tryCatch(sieve_export_diagram(plain, paste0('figure.', ext), dpi = 96),",
    "                  error = function(e) NULL)",
    "  cat('WROTE', ext, if (is.null(out)) 0 else file.size(out$file), '\\n')",
    "}",
    "cat('NODES', paste(diagram$nodes$id[diagram$nodes$kind != 'residual'], collapse = ','), '\\n')",
    "cat('DRAWN', sum(plain$edges$visible), 'OF', nrow(plain$edges), '\\n')",
    "cat('CODE', sieve_diagram_code(plain), '\\n')",
    "cat('DEVICES', length(grDevices::dev.list()), '\\n')",
    "cat('SEED', exists('.Random.seed', envir = globalenv()), '\\n')",
    "cat('FILES', paste(sort(list.files()), collapse = ','), '\\n')"
  ), script)
  out <- withr::with_dir(dir, withr::with_envvar(
    c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
    suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(script)),
      stdout = TRUE, stderr = TRUE
    ))
  ))
  status <- attr(out, "status")
  expect_true(is.null(status) || identical(status, 0L), info = paste(out, collapse = "\n"))
  line <- function(tag) sub(paste0("^", tag, " "), "", grep(paste0("^", tag, " "), out, value = TRUE))
  wrote <- line("WROTE")
  expect_length(wrote, 3L)
  expect_gt(as.numeric(sub("^pdf ", "", wrote[1])), 1000)
  expect_gt(as.numeric(sub("^png ", "", wrote[3])), 1000)
  expect_match(line("NODES"), "visual,textual,t1_visual_perception", fixed = TRUE)
  # What the fresh session reports is what this session computes for the
  # same model: the counts and the reproduction call.
  fit <- lavaan::cfa("visual =~ t1_visual_perception + t2_cubes + t4_lozenges
                      textual =~ t6_paragraph_comprehension + t7_sentence + t9_word_meaning",
    data = DMAR::holzinger_swineford, std.lv = TRUE
  )
  here <- sieve_diagram_update(sieve_diagram_spec(fit, analysis_id = "clean-session"),
    covariances = FALSE
  )
  expect_identical(
    trimws(line("DRAWN")),
    sprintf("%d OF %d", sum(here$edges$visible), nrow(here$edges))
  )
  expect_identical(trimws(line("CODE")), paste(sieve_diagram_code(here), collapse = " "))
  # The session ends with no open device, no random number stream, and no
  # file it was not asked to write.
  expect_identical(trimws(line("DEVICES")), "0")
  expect_identical(trimws(line("SEED")), "FALSE")
  made <- strsplit(trimws(line("FILES")), ",", fixed = TRUE)[[1]]
  expect_true(all(made %in% c("export.R", "figure.pdf", "figure.png", "figure.svg")))
  expect_true(all(c("figure.pdf", "figure.png") %in% made))
  expect_false("Rplots.pdf" %in% made)
  expect_identical(rawToChar(readBin(file.path(dir, "figure.pdf"), "raw", 4L)), "%PDF")
})


test_that("diagram labels remain visible across panel sizes without refitting", {
  skip_if_no_browser()
  siever_with_app(load_example = TRUE, width = 1280, height = 900, function(app) {
    siever_open_tab(app, "cfa")
    if (isTRUE(app$get_js("!!document.getElementById('cfa-fit_go')"))) app$click("cfa-fit_go")
    app$wait_for_js("document.querySelector('#cfa-method_line').textContent.includes(', current.')",
      timeout = 120 * 1000)
    app$wait_for_js("document.querySelector('#cfa-diagram img')?.complete")
    before <- app$get_js("document.querySelector('#cfa-method_line').textContent")
    # The center of the first latent ellipse in this nine-item fixture.
    # Its border and paths are outside this patch, so ink comes from its label.
    label_ink <- function() app$get_js(paste0(
      "(function(){const i=document.querySelector('#cfa-diagram img'),c=document.createElement('canvas');",
      "c.width=i.naturalWidth;c.height=i.naturalHeight;const g=c.getContext('2d');g.drawImage(i,0,0);",
      "const p=g.getImageData(Math.round(c.width*.165)-14,Math.round(c.height*.356)-5,28,10).data;",
      "let n=0;for(let k=0;k<p.length;k+=4)if(p[k+3]>0&&Math.max(p[k],p[k+1],p[k+2])<150)n++;return n;})()"))
    readable <- function() {
      txt <- app$get_js("document.querySelector('#cfa-diagram_readability').textContent")
      as.numeric(sub(".*Labels about ([0-9.]+) pt on screen.*", "\\1", txt))
    }
    expect_gte(readable(), 8)
    expect_gte(label_ink(), 5)
    app$set_inputs(`cfa-diagram_zoom` = 50)
    siever_wait_render(app)
    expect_gte(readable(), 8)
    expect_gte(label_ink(), 5)
    app$set_inputs(`cfa-diagram_zoom` = 100)
    siever_wait_render(app)
    width <- app$get_js("document.querySelector('#cfa-diagram img').naturalWidth")
    app$get_chromote_session()$Emulation$setDeviceMetricsOverride(
      width = 2000, height = 1100, deviceScaleFactor = 1, mobile = FALSE)
    app$wait_for_js(sprintf("document.querySelector('#cfa-diagram img')?.complete && document.querySelector('#cfa-diagram img').naturalWidth > %d", width),
      timeout = 120 * 1000)
    siever_wait_render(app)
    expect_gte(readable(), 8)
    expect_gte(label_ink(), 5)
    expect_identical(app$get_js("document.querySelector('#cfa-method_line').textContent"), before)
    expect_identical(siever_n_output_errors(app), 0L)
  })
})
