# The run policy at the standard rates, the page guide's one Update, reading
# a column as numbers, the fixed overhead of small fits, and the not-run
# line of an on-request page.

test_that("the run policy decides at the standard rates, so a first result does not flip between visits", {
  local_measured_rates()
  sieve_rate_reset()
  on.exit(sieve_rate_reset(), add = TRUE)
  standard <- sieve_fit_wait(24, "MLR", "cfa")
  # A measured rate three times the default changes the stated wait, not
  # the decision computed at the standard rates.
  sieve_rate_note("cfa_MLR", 3 * sieve_rate("cfa_MLR") * 276, 276)
  measured <- sieve_fit_wait(24, "MLR", "cfa")
  expect_gt(measured, standard)
  expect_identical(sieve_at_default_rates(sieve_fit_wait(24, "MLR", "cfa")), standard)
  # The measured rates are back afterwards.
  expect_identical(sieve_fit_wait(24, "MLR", "cfa"), measured)
  expect_true(sieve_runs_by_itself(5, standard = 1))
  expect_false(sieve_runs_by_itself(7, standard = 1))
  expect_false(sieve_runs_by_itself(1, standard = 3))
  expect_false(sieve_runs_by_itself(1, standard = NA_real_))
  expect_true(sieve_runs_by_itself(NA_real_, standard = 1))
  expect_false(sieve_runs_by_itself(1, standard = 1, visible = FALSE))
  # Without a standard estimate the stated one decides.
  expect_true(sieve_runs_by_itself(2))
  expect_false(sieve_runs_by_itself(2.5))
  expect_match(sieve_run_policy_text(), "depends only on the size of the problem", fixed = TRUE)
})

test_that("parallel analysis keeps its first-run decision when a measured rate moves", {
  local_measured_rates()
  sieve_rate_reset()
  on.exit(sieve_rate_reset(), add = TRUE)
  x <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  b <- sieve_retention_cor(x, requested = "pearson")
  # (The plan is DMAR's, its measured rate is per unit of a reference
  # dataset, and there are no workers.)
  first <- sieve_pa_plan(b, 200L)
  expect_null(first$refused)
  # Slower measured runs (but within six seconds) do not change it.
  units <- sieve_pa_cost(b, 200L)$work
  sieve_rate_note("pa_dmar_pearson", min(5, max(first$compute, 0.01) * 2), units)
  later <- sieve_pa_plan(b, 200L)
  expect_gt(later$compute, first$compute)
  expect_identical(later$automatic, first$automatic)
})

test_that("the page guide names the page's Update when a result on it is stale, and offers no second button", {
  fx <- sieve_hs9_fixture()
  st <- sieve_new_state()
  shiny::isolate({
    st$raw <- fx$data
    st$items <- fx$data[, fx$item_cols, drop = FALSE]
    st$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
    st$confirmed <- TRUE
    sieve_page_publish(st, "reliab", "reliab-run_go", "Reliability",
      list(state = "stale", sentence = "The item pool changed."), "Update", own = TRUE)
    status <- sieve_page_status(st, "reliab")
  })
  expect_true(status$update)
  html <- as.character(sieve_page_status_ui(status, st))
  expect_match(html, "Update recomputes them in turn", fixed = TRUE)
  # One Update per page: the page's status line holds it, with its
  # estimate; the guide has no second, unestimated one.
  expect_false(grepl("update_go", html, fixed = TRUE))
  expect_false(grepl("<button", html, fixed = TRUE))
  shiny::isolate({
    sieve_page_publish(st, "reliab", "reliab-run_go", "Reliability", list(state = "current"), "Update", own = TRUE)
    current <- sieve_page_status(st, "reliab")
  })
  expect_false(current$update)
  expect_false(grepl("update_go", as.character(sieve_page_status_ui(current, st)), fixed = TRUE))
})

test_that("a column is read as numbers the one way, labelled SPSS columns included", {
  skip_if_not_installed("haven")
  v <- haven::labelled(c(1, 2, NA, 7), labels = c(low = 1, high = 7))
  expect_identical(sieve_as_number(v), c(1, 2, NA, 7))
  expect_identical(sieve_as_number(factor(c("b", "a"))), c(2, 1))
  expect_identical(sieve_as_number(c("1", "x")), c(1, NA))
  # The whole suite runs with haven loaded (helper-haven.R).
  expect_true("haven" %in% loadedNamespaces())
})

test_that("several small fits of one computation pay the fixed overhead once", {
  local_measured_rates()
  sieve_rate_reset()
  on.exit(sieve_rate_reset(), add = TRUE)
  # Five constructs of four to six items (the full Holzinger and Swineford
  # example) were estimated at 3.25 seconds and took 0.8.
  ks <- c(4, 5, 4, 6, 5)
  expect_lte(sieve_demand_wait("reliability", ks, "MLR"), sieve_auto_seconds())
  # One fit keeps its estimate.
  expect_equal(sieve_demand_wait("cfa", 24, "MLR"), sieve_fit_wait(24, "MLR", "cfa"))
  expect_identical(sieve_fit_wait(4, "MLR", "cfa"), 0.5)
  expect_lt(sieve_fit_wait(4, "MLR", "cfa", floor = FALSE), 0.5)
})

test_that("an on-request page's not-run line does not say a quick result waits because of its estimate", {
  # Only the primary CFA and Reliability results may run by themselves;
  # the Validity page's measurement model, however quick, is
  # computed on request, and its line says so rather than "so it waits".
  fx <- sieve_hs9_fixture()
  st <- sieve_new_state()
  shiny::isolate({
    st$raw <- fx$data
    st$items <- fx$data[, fx$item_cols, drop = FALSE]
    st$id_vars <- fx$id_vars
    st$roles <- sieve_declared_roles(fx$data, fx$item_cols)
    st$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
    st$theory <- list(k = fx$theory_k, label = fx$theory_lbl, map = fx$theory_map)
    st$confirmed <- TRUE
    st$thresholds <- sieve_default_thresholds()
    sieve_new_dataset(st)
  })
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(score_method = "mean", htmt_on = FALSE, marker = "")
    line <- gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(output$gate)), collapse = " ")))
    expect_match(line, "it is computed when you ask for it", fixed = TRUE)
    expect_false(grepl("so it waits for you", line, fixed = TRUE))
  })
})
