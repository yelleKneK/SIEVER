# Reopening and saving projects, replaying recorded analyses, scoring, and
# split omega, with the project controls. Inputs are synthetic or the
# package's public bundled example; no private fixture is required.
replay_hs9_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = FALSE, name = NULL, configure = function(s) {
  s$roles <- sieve_declared_roles(fx$data, fx$item_cols)
  s$estimator <- "MLR"

  })
  s
}

test_that("reopening keeps original decisions intact, applicable, and supersedable", {
  s <- replay_hs9_state()
  original <- sieve_record_decision(s, "factors", "How many?", 3L)
  key <- sieve_decision_key(original)
  first <- sieve_project_snapshot(s)
  dest <- tempfile(fileext = ".siever.rds")
  sieve_project_write(first, dest)
  on.exit(unlink(dest), add = TRUE)
  t <- sieve_new_state()
  sieve_project_restore(t, sieve_project_read(dest))
  shiny::isolate({
    expect_false(identical(t$dataset_id, s$dataset_id))
    expect_identical(t$scientific_dataset_id, s$scientific_dataset_id)
    expect_identical(sieve_current_decision(t, "factors", t$dataset_id)$id, original$id)
    expect_identical(sieve_decision_key(t$decisions[[1L]]), key)
    expect_true(all(sieve_decisions_check(t)$intact))
  })
  next_record <- sieve_record_decision(t, "factors", "How many?", 2L)
  expect_identical(next_record$supersedes, original$id)
  # Reopen a second time, including the already remapped applicability.
  u <- sieve_new_state()
  sieve_project_restore(u, sieve_project_snapshot(t))
  shiny::isolate(expect_identical(sieve_current_decision(u, "factors", u$dataset_id)$id, next_record$id))
})

test_that("Save As preserves two distinct versions including settings and basis", {
  s <- replay_hs9_state()
  s$project_name <- "First version"
  s$efa_settings <- list(n_factors = 3L)
  before <- tempfile(fileext = ".siever.rds")
  after <- tempfile(fileext = ".siever.rds")
  on.exit(unlink(c(before, after)), add = TRUE)
  sieve_project_write(sieve_project_snapshot(s), before)
  m <- shiny::isolate(s$meta)
  m$status[2] <- "parked"
  m$facet[1] <- "Planning"
  s$meta <- m
  s$construct_basis <- "empirical"
  s$project_name <- "Second version"
  s$efa_settings <- list(n_factors = 2L)
  sieve_project_write(sieve_project_snapshot(s), after)
  a <- sieve_new_state()
  b <- sieve_new_state()
  sieve_project_restore(a, sieve_project_read(before))
  sieve_project_restore(b, sieve_project_read(after))
  shiny::isolate({
    expect_identical(a$meta$status[2], "active")
    expect_identical(b$meta$status[2], "parked")
    expect_identical(b$meta$facet[1], "Planning")
    expect_identical(a$construct_basis, "theoretical")
    expect_identical(b$construct_basis, "empirical")
    expect_identical(a$efa_settings$n_factors, 3L)
    expect_identical(b$efa_settings$n_factors, 2L)
    expect_identical(a$raw, b$raw)
  })
})

test_that("recorded analysis inputs replay the nine-item and eight-item revisions", {
  s <- replay_hs9_state()
  sieve_log(
    s,
    "Before exclusion",
    "before_names <- meta$item[meta$status == 'active']; before_r <- stats::cor(items[, before_names])[1,2]"
  )
  m <- shiny::isolate(s$meta)
  m$status[9] <- "parked"
  s$meta <- m
  sieve_log(
    s,
    "After exclusion",
    "after_names <- meta$item[meta$status == 'active']; after_r <- stats::cor(items[, after_names])[1,2]"
  )
  script <- sieve_replay_script(s)
  env <- new.env(parent = baseenv())
  eval(parse(text = script), env)
  expect_length(env$before_names, 9L)
  expect_length(env$after_names, 8L)
  expect_equal(env$before_r, stats::cor(shiny::isolate(s$items))[1, 2], tolerance = 1e-14)
  expect_equal(env$after_r, env$before_r, tolerance = 1e-14)
  t <- sieve_new_state()
  sieve_project_restore(t, sieve_project_snapshot(s))
  expect_identical(sieve_replay_script(t), script)
})

test_that("scoring preserves distinct construct labels, item bounds, and missing scores", {
  dat <- data.frame(a = c(0, 20, NA, 100), b = c(0, 40, NA, 50), c = c(2, 3, NA, 4))
  meta <- data.frame(
    item = names(dat), status = "active", construct = c("A-B", "A_B", "A B"),
    reverse = c(TRUE, TRUE, FALSE), rev_lo = c(0, 0, NA), rev_hi = c(100, 50, NA), rev_bounds = "declared"
  )
  code <- sieve_scoring_code(meta)
  expect_identical(length(unique(code$name_map)), 3L)
  e <- new.env(parent = baseenv())
  e$dat <- dat
  eval(parse(text = code$code), e)
  expect_equal(e$dat[[code$name_map[["A-B"]]]], c(100, 80, NA, 0))
  expect_equal(e$dat[[code$name_map[["A_B"]]]], c(50, 10, NA, 0))
  expect_equal(e$dat[[code$name_map[["A B"]]]], dat$c)
  expect_false(any(is.nan(as.matrix(e$dat))))
  processed <- e$dat[, names(dat)]
  p <- new.env(parent = baseenv())
  p$dat <- processed
  eval(parse(text = sieve_scoring_code(meta, "processed")$code), p)
  expect_identical(p$dat, e$dat)
  meta$rev_hi[1] <- NA_real_
  expect_error(sieve_scoring_code(meta), "bounds.*unknown")
})

test_that("split omega uses the resolved reliability target", {
  s <- replay_hs9_state()
  meta <- shiny::isolate(s$meta)
  items <- shiny::isolate(s$items)
  got <- sieve_split_omega(items, meta)
  group <- sieve_reliability_items(meta, names(items))[[1]]
  spec <- sieve_reliability_spec(group$items, group$types, construct = group$construct, ci_method = "none")
  ref <- sieve_reliability_omega(items, spec)
  expect_equal(unname(got[1]), ref$estimate, tolerance = 1e-12)
  expect_identical(unname(attr(got, "targets")[1]), spec$label)
})

test_that("browser project saving never clears changed state merely by preparing a download", {
  s <- replay_hs9_state()
  shiny::testServer(mod_project_server, args = list(state = s), {
    session$flushReact()
    expect_match(paste(as.character(unlist(output$status)), collapse = " "), "Changed")
    path <- tempfile()
    on.exit(unlink(path), add = TRUE)
    write_download(path, "Named copy")
    session$flushReact()
    expect_match(paste(as.character(unlist(output$status)), collapse = " "), "Changed")
    session$setInputs(acknowledge = 1L)
    expect_match(paste(as.character(unlist(output$status)), collapse = " "), "Saved copy acknowledged")
    m <- shiny::isolate(s$meta)
    m$facet[1] <- "New facet"
    s$meta <- m
    session$flushReact()
    expect_match(paste(as.character(unlist(output$status)), collapse = " "), "Changed")
    expect_warning(
      expect_error(write_download(
        file.path(
          tempdir(),
          "absent-folder",
          "file"
        ),
        "Failed copy"
      )),
      "cannot open file"
    )
    session$flushReact()
    expect_match(paste(as.character(unlist(output$status)), collapse = " "), "Changed")
  })
})


test_that("the live replay preserves recorded analysis expressions", {
  st <- replay_hs9_state()
  its <- shiny::isolate(st$meta$item)
  sieve_set_item_status(st, its[2L], "parked", reason = "Review")
  block <- c("names_seen <- meta$item[meta$status == 'active']", "n_seen <- length(names_seen)")
  sieve_log(st, "Read the included names", block)
  expect_true(all(block %in% shiny::isolate(st$codelog)))
  env <- new.env(parent = globalenv())
  expect_no_error(eval(parse(text = sieve_replay_script(st)), env))
  expect_identical(env$names_seen, setdiff(its, its[2L]))
  expect_identical(env$n_seen, length(its) - 1L)
})


test_that("identical code on a different input revision is logged again", {
  s <- replay_hs9_state()
  code <- "items_seen <- names(items); result_seen <- stats::cor(items)[1, 2]"
  sieve_log(s, "Same analysis", code)
  n <- length(shiny::isolate(s$codelog))
  sieve_log(s, "Same analysis", code)
  expect_length(shiny::isolate(s$codelog), n)
  d <- shiny::isolate(s$items)
  d[[1L]][1L] <- d[[1L]][1L] + 1
  s$items <- d
  sieve_log(s, "Same analysis", code)
  expect_gt(length(shiny::isolate(s$codelog)), n)
  expect_length(shiny::isolate(s$replay_steps), 2L)
  states <- shiny::isolate(s$replay_states)
  expect_length(unique(shiny::isolate(unlist(s$replay_steps))), 2L)
})

test_that("complete exported CV and validity scripts reproduce the fitted models", {
  s <- replay_hs9_state()
  # Incomplete and reversed values distinguish the analyzed items from raw.
  d <- shiny::isolate(s$items)
  d[[1]][1:3] <- NA_real_
  d[[2]] <- -d[[2]]
  s$items <- d
  shiny::isolate(sieve_confirm_types(s))
  shiny::testServer(mod_finalize_server, args = list(state = s), {
    session$setInputs(holdout = 40, seed = 113, estimator = "mlr", split = 1)
    app <- cv_shown()
    expect_null(app$error)
    script <- sieve_replay_script(state)
    env <- new.env(parent = baseenv())
    expect_no_error(suppressWarnings(capture.output(eval(parse(text = script), env))))
    expect_identical(env$cross_validation$n_val, app$n_val)
    expect_identical(env$cross_validation$n_cal, app$n_cal)
    expect_equal(env$cross_validation$table, app$table, tolerance = 1e-12)
    expect_equal(env$cross_validation$xval, app$xval, tolerance = 1e-12)
    expect_equal(env$cross_validation$rel, app$rel, tolerance = 1e-12)
    expect_identical(env$cross_validation$validation_rows, app$validation_rows)
    expect_equal(env$norms_result$table, norms(), tolerance = 1e-12)
    expect_length(env$norms_result$T, 3L)
    expect_match(paste(as.character(unlist(output$cv_msg)), collapse = " "), "internal assessment")
  })
  # Start a distinct session log so the complete validity script is independent.
  v <- replay_hs9_state()
  v$items <- d
  shiny::isolate(sieve_confirm_types(v))
  shiny::testServer(mod_validity_server, args = list(state = v), {
    session$setInputs(estimator = "mlr", fit_go = 1)
    expect_false(is.null(fit()))
    expect_no_error(output$phi_tbl)
    expect_no_error(output$ave_tbl)
    app_ave <- ave_df()
    app_ss <- std_solution()$value
    script <- sieve_replay_script(state)
    env <- new.env(parent = baseenv())
    expect_no_error(suppressWarnings(capture.output(eval(parse(text = script), env))))
    expect_equal(env$validity_ss, app_ss, tolerance = 1e-10)
    expect_equal(env$ave$ave, app_ave$AVE, tolerance = 1e-10)
    expect_identical(lavaan::lavInspect(env$validity_fit, "options")$estimator, "ML")
    expect_identical(lavaan::lavInspect(env$validity_fit, "options")$missing, "ml")
  })
})

test_that("split omega on ordered items uses the categorical observed-score target", {
  s <- replay_hs9_state()
  d <- shiny::isolate(s$items)
  d[] <- lapply(
    d,
    function(x) as.integer(cut(x,
      breaks = stats::quantile(
        x,
        seq(0,
          1,
          length.out = 6
        )
      ),
      include.lowest = TRUE
    ))
  )
  meta <- shiny::isolate(s$meta)
  meta$type <- "ordered"
  got <- sieve_split_omega(d, meta)
  its <- meta$item[meta$construct == meta$construct[1]]
  direct <- DMAR::reliability_omega_categorical(d[, its, drop = FALSE], ci_method = "none", conf_level = .95)
  expect_equal(unname(got[1]), direct$value[direct$term == "estimate"], tolerance = 1e-10)
  # The label names its target in DMAR's denominator wording.
  expect_identical(unname(attr(got, "targets")[1]), "Robust omega (observed denominator: unrestricted polychoric correlations)")
})


test_that("split omega follows explicitly selected item treatment", {
  fx <- replay_hs9_state()
  data <- shiny::isolate(fx$items)
  meta <- shiny::isolate(fx$meta)
  # Ordered metadata treated as continuous by an explicit MLR selection.
  meta$type <- "ordered"
  got <- sieve_split_omega(data, meta, estimator = "mlr")
  g <- sieve_reliability_items(meta, names(data))[[1L]]
  sp <- sieve_reliability_spec(g$items, rep("continuous", length(g$items)), construct = g$construct, ci_method = "none")
  ref <- sieve_reliability_omega(data, sp)
  expect_equal(unname(got[1L]), ref$estimate, tolerance = 1e-12)
  expect_identical(unname(attr(got, "targets")[1L]), sp$label)
})
