skip_if_not_installed("DMAR")

api_reliability_state <- function() {
  fx <- sieve_hs9_fixture()
  st <- sieve_new_state()
  st$raw <- fx$data
  st$items <- fx$data[, fx$item_cols, drop = FALSE]
  st$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  st$confirmed <- TRUE
  st
}

test_that("the congeneric planning boundary and sensitivity use the population API", {
  lambda <- c(.8, .7, .6, .7)
  psi <- c(.36, .51, .64, .51)
  planned <- sieve_plan_congeneric(lambda, psi, width = .1, seed = 31)
  direct <- DMAR::ss_aipe_reliability(model = "Congeneric", type = "Factor Analytic", width = .1,
    n_items = 4, true_factor_loading = lambda, true_residual_var = psi, conf_level = .95)
  expect_null(planned$error)
  expect_equal(planned$result, direct)
  expect_true(all(is.finite(planned$sensitivity$N)))
  expect_true(all(planned$sensitivity$reason == ""))
  expect_named(planned$call, c("model", "type", "width", "n_items", "true_factor_loading",
    "true_residual_var", "conf_level", "assurance", "fmi"))
})

test_that("network RMSEA intervals and replay use n_groups and the intended divisor", {
  all <- c(chisq = 45, df = 24, chisq.scaled = 40, df.scaled = 24, chisq.scaling.factor = 1.25)
  for (family in c("standard", "robust")) {
    got <- sieve_network_rmsea_interval(all, ci_family = family, N = 301, groups = 2, conf_level = .9)
    reference <- DMAR::sem_fit_measures(chisq = if (family == "robust") 40 else 45,
      df = 24, N = 301, n_groups = 2, conf_level = .9, sample_size_convention = "N")
    multiplier <- if (family == "robust") sqrt(1.25) else 1
    expect_true(got$ok)
    expect_equal(c(got$lower, got$upper), multiplier * reference$value[
      match(c("rmsea_ci_lower", "rmsea_ci_upper"), reference$term)])
  }
})

test_that("assurance disclosure distinguishes the request and selected final-pass estimate", {
  st <- api_reliability_state()
  testthat::local_mocked_bindings(ss_aipe_reliability = function(model, type, width,
      n_items, true_item_covariance, true_residual_var, conf_level, assurance, fmi = 0) {
    expect_identical(n_items, 6L)
    expect_equal(true_item_covariance + true_residual_var, 1)
    x <- data.frame(term = c("necessary_N", "width", "assurance", "assurance_attained", "G"),
      value = c(125, width, assurance, .87, 400))
    attr(x, "no_interval") <- 3L
    attr(x, "fmi") <- fmi
    attr(x, "necessary_N_complete") <- 100L
    x
  }, .package = "DMAR")
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(scope = "construct", conf_level = .95, ci_method = "recommended", B = 10000,
      plan_omega = .8, plan_k = 6L, plan_width = .1, plan_assurance = ".8", plan_seed = 31L,
      plan_fmi = .2)
    session$setInputs(plan_go = 1)
    planned <- plan_calc()
    expect_true(planned$planned$ok)
    html <- as.character(output$plan_result$html)
    expect_match(html, "to target 80% assurance", fixed = TRUE)
    expect_match(html, "Final-pass estimated assurance: 87% from 400 simulations", fixed = TRUE)
    expect_match(html, "3 final-pass samples returned no interval", fixed = TRUE)
    expect_match(html, "Monte Carlo SE", fixed = TRUE)
    expect_match(html, "complete-data N = 100", fixed = TRUE)
    expect_false(grepl("gives 80% assurance", html, fixed = TRUE))
    expect_false(grepl("half of realized", html, fixed = TRUE))
    replay <- eval(parse(text = paste(plan_code(planned), collapse = "\n")), new.env())
    expect_equal(replay, planned$planned$value)
    request <- boot_requirement(list(B = 1000L, ci_method = "dmar_bca", conf_level = .95,
      cluster = NULL, resampling = "dmar"))
    expect_false(request$ok)
    expect_identical(request$B, 1000L)
    expect_identical(request$required, 2000L)
    expect_true(boot_requirement(list(B = 10000L, ci_method = "dmar_bca", conf_level = .95,
      cluster = NULL, resampling = "dmar"))$ok)
    expect_true(boot_requirement(list(B = 100L, ci_method = "bootstrap_se_logistic", conf_level = .99,
      cluster = NULL, resampling = "dmar"))$ok)
  })
})
