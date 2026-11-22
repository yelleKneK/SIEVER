# Wiring checks for stale researcher controls. Statistical adapters have their
# own direct numerical oracles; these use a real small parent lavaan model and
# counted adapter stubs to verify that editing a control never relabels an old
# result or runs another analysis automatically.
reviewed_control_fixture <- function() {
  withr::with_seed(201, {
    eta <- stats::rnorm(180)
    d <- data.frame(
      a = .8 * eta + stats::rnorm(
        180,
        0,
        .6
      ),
      b = .7 * eta + stats::rnorm(
        180,
        0,
        .7
      ),
      c = .9 * eta + stats::rnorm(
        180,
        0,
        .5
      ),
      g = rep(
        c(
          "A",
          "B"
        ),
        each = 90
      )
    )
  })
  m <- sieve_build_meta(d, c("a", "b", "c"), list(F = c("a", "b", "c")))
  m$type <- "continuous"
  f <- lavaan::cfa("F =~ a+b+c", data = d, group = "g", std.lv = TRUE, estimator = "MLR", missing = "fiml")
  fixture <- list(data = d, item_cols = c("a", "b", "c"), theory_map = list(F = c("a", "b", "c")),
    name = "Controls fixture", labels = NULL, load_code = character())
  s <- siever_test_study(fixture, labels = FALSE, theory = FALSE, name = NULL, configure = function(s) {
    s$meta <- m
    roles <- sieve_propose_roles(d)
    roles$role <- ifelse(roles$variable == "g", "group", "item")
    s$roles <- roles
  })
  list(state = s, fit = f, data = d)
}

test_that("ordinal impact group, rung, source and population edits cannot relabel saved impact", {
  x <- reviewed_control_fixture()
  seen <- new.env(parent = emptyenv())
  seen$n <- 0L
  testthat::local_mocked_bindings(sieve_ordinal_fit_impact = function(fit, scores, reference, focal, mean = 0, sd = 1, anchors) {
    seen$n <- seen$n + 1L
    list(
      table = data.frame(
        item = "a",
        signed = .25,
        absolute = .25,
        rms = .3
      ),
      model_key = "fixture-ordinal-result",
      reference = reference,
      focal = focal,
      limitation = "Counted adapter fixture"
    )
  }, .package = "SIEVER")
  shiny::testServer(mod_finalize_server, args = list(state = x$state), {
    session$setInputs(
      dmacs_source = "full",
      dmacs_ref = "A",
      dmacs_foc = "B",
      inv_level = "scalar",
      ordinal_impact_mean = 0,
      ordinal_impact_sd = 1,
      ordinal_impact_anchors = "Prespecified anchor rationale"
    )
    keep_inv(list(
      data = x$data,
      group = "g",
      fits = list(
        scalar = x$fit,
        strict = x$fit
      ),
      levels = c(
        "scalar",
        "strict"
      ),
      group_labels = c(
        "A",
        "B"
      ),
      missing = "fiml"
    ))
    session$setInputs(ordinal_impact_go = 1)
    expect_null(ordinal_impact_shown()$error)
    expect_identical(ordinal_impact_shown()$table$signed, .25)
    stored <- state$ordinal_impact_records
    expect_identical(stored[[1L]]$scientific_dataset_id, state$scientific_dataset_id)
    changes <- list(
      dmacs_ref = "B",
      dmacs_foc = "A",
      inv_level = "strict",
      dmacs_source = "partial",
      ordinal_impact_mean = 1,
      ordinal_impact_sd = 2,
      ordinal_impact_anchors = "Changed rationale"
    )
    originals <- list(
      dmacs_ref = "A",
      dmacs_foc = "B",
      inv_level = "scalar",
      dmacs_source = "full",
      ordinal_impact_mean = 0,
      ordinal_impact_sd = 1,
      ordinal_impact_anchors = "Prespecified anchor rationale"
    )
    for (nm in names(changes)) {
      do.call(session$setInputs, stats::setNames(list(changes[[nm]]), nm))
      expect_true(isTRUE(ordinal_impact_shown()$stale), info = nm)
      expect_match(ordinal_impact_shown()$error, "changed", info = nm)
      expect_identical(state$ordinal_impact_records, stored, info = nm)
      expect_identical(seen$n, 1L, info = nm)
      do.call(session$setInputs, stats::setNames(list(originals[[nm]]), nm))
      expect_null(ordinal_impact_shown()$error, info = nm)
    }
  })
})

test_that("anchor sets, rationale, disclosure and estimator edits mark earlier models historical", {
  x <- reviewed_control_fixture()
  seen <- new.env(parent = emptyenv())
  seen$n <- 0L
  testthat::local_mocked_bindings(sieve_anchor_sensitivity = function(
    data,
    group,
    meta,
    anchor_sets,
    estimator,
    missing,
    rationale,
    discovery
  ) {
    seen$n <- seen$n + 1L
    contrast <- data.frame(estimate = .4, lower = .1, upper = .7)
    models <- lapply(
      names(anchor_sets),
      function(nm) list(
        name = nm,
        model_id = paste0(
          "fixture-",
          nm
        ),
        syntax = "F =~ a+b+c",
        anchors = anchor_sets[[nm]],
        released = character(0),
        fit = x$fit,
        parameters = lavaan::parameterEstimates(x$fit),
        interpretation = list(
          contrasts = contrast,
          anchors = data.frame(
            item = c(
              "a",
              "b"
            ),
            anchor = TRUE
          )
        ),
        warnings = character(0)
      )
    )
    list(
      models = models,
      anchor_sets = anchor_sets,
      rationale = rationale,
      discovery = discovery,
      exposure = "Fixture disclosure",
      contrasts = contrast
    )
  }, .package = "SIEVER")
  shiny::testServer(mod_finalize_server, args = list(state = x$state), {
    session$setInputs(
      inv_anchor_sets = "First: a, b\nSecond: b, c",
      inv_anchor_rationale = "Prespecified",
      inv_anchor_discovery = TRUE,
      estimator = "auto"
    )
    keep_inv(list(data = x$data, group = "g", fits = list(scalar = x$fit), levels = "scalar", group_labels = c("A", "B"), missing = "fiml"))
    session$setInputs(inv_anchor_run = 1)
    expect_null(anchor_shown()$error)
    expect_length(state$analyses, 2L)
    stored <- state$anchor_decisions
    expect_identical(stored[[1L]]$scientific_dataset_id, state$scientific_dataset_id)
    analyses <- state$analyses
    changes <- list(
      inv_anchor_sets = "First: a, c\nSecond: b, c",
      inv_anchor_rationale = "New rationale",
      inv_anchor_discovery = FALSE,
      estimator = "ml"
    )
    originals <- list(
      inv_anchor_sets = "First: a, b\nSecond: b, c",
      inv_anchor_rationale = "Prespecified",
      inv_anchor_discovery = TRUE,
      estimator = "auto"
    )
    for (nm in names(changes)) {
      do.call(session$setInputs, stats::setNames(list(changes[[nm]]), nm))
      expect_true(isTRUE(anchor_shown()$stale), info = nm)
      expect_identical(
        anchor_shown()$error,
        paste(
          "The settings for this anchor sensitivity changed.",
          "Press Compare and freeze anchor decisions to compute it for the current choices.",
          "No analysis runs when these settings change."
        ),
        info = nm
      )
      expect_identical(state$anchor_decisions, stored, info = nm)
      expect_identical(state$analyses, analyses, info = nm)
      expect_identical(seen$n, 1L, info = nm)
      do.call(session$setInputs, stats::setNames(list(originals[[nm]]), nm))
      expect_null(anchor_shown()$error, info = nm)
    }
  })
})
