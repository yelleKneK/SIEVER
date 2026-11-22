# Ordered-item MI sensitivity analysis: each parameter is pooled by Rubin's
# (1987) rules, T = W + (1 + 1/M)B, and not by lavaan.mi's default, which
# scales W by the average relative increase in variance over all free
# parameters. With missingness concentrated on one item the default gives
# that item's loading a standard error of about .81 of Rubin's.

test_that("ordered MI pools every parameter by Rubin's rules when missingness is concentrated on one item", {
  skip_on_cran()
  skip_if_not_installed("mice")
  # Not skip_if_not_installed(), which loads lavaan.mi's namespace while
  # lavaan is not attached (see test-lavaan-mi-namespace.R).
  skip_if_not(sieve_installed("lavaan.mi"), "lavaan.mi is not installed")
  raw <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  d <- as.data.frame(lapply(raw, function(v) as.integer(cut(v,
    breaks = unique(stats::quantile(v, probs = seq(0, 1, .2))), include.lowest = TRUE))))
  # MAR on x4 only, driven by x5: about 42% of x4 is missing, the other
  # items are complete, so the fractions of missing information differ
  # widely across the parameters.
  withr::with_seed(1L, {
    d$x4[stats::runif(nrow(d)) < stats::plogis(-0.6 + 0.8 * as.numeric(scale(raw$x5)))] <- NA
  })
  expect_gt(mean(is.na(d$x4)), .35)
  expect_false(anyNA(d[c("x5", "x6")]))
  a <- sieve_mi_model("Textual =~ x4 + x5 + x6", d, ordered = names(d), seed = 1L)
  expect_identical(a$pooled_variance, "rubin_parameterwise")
  expect_match(a$method, "Rubin's (1987) rules: T = W + (1 + 1/M)B", fixed = TRUE)
  expect_match(a$method, "df = (M - 1)(1 + W/((1 + 1/M)B))^2", fixed = TRUE)
  expect_match(a$method, "FMI = (1 + 1/M)B/T", fixed = TRUE)

  # Rubin's rules computed independently from the per-imputation fits that
  # were pooled (converged, with standard errors).
  fit <- a$fit
  pooled <- vapply(fit@convergence, function(x) isTRUE(x[[1L]]) && isTRUE(x[[2L]]), logical(1))
  expect_identical(sum(pooled), a$used)
  pt <- fit@ParTable
  free <- which(pt$free > 0L)
  Q <- sapply(fit@ParTableList[pooled], `[[`, "est")[free, , drop = FALSE]
  U <- sapply(fit@ParTableList[pooled], `[[`, "se")[free, , drop = FALSE]^2
  M <- ncol(Q)
  W <- rowMeans(U)
  B <- apply(Q, 1L, stats::var)
  T_total <- W + (1 + 1 / M) * B
  df <- (M - 1) * (1 + W / ((1 + 1 / M) * B))^2
  key <- paste(pt$lhs, pt$op, pt$rhs)[free]
  e <- a$estimates
  k <- match(key, paste(e$lhs, e$op, e$rhs))
  expect_false(anyNA(k))
  # Every pooled standard error is sqrt(W + (1 + 1/M)B).
  expect_equal(e$se[k], sqrt(T_total), tolerance = 1e-10)
  expect_lt(max(abs(e$se[k] - sqrt(T_total))), 1e-10)
  # The fraction of missing information and the t reference, parameter by
  # parameter (B = 0, and so infinite df, for the thresholds of the
  # complete items).
  expect_lt(max(abs(e$fmi[k] - (1 + 1 / M) * B / T_total)), 1e-10)
  finite <- is.finite(df)
  expect_true(all(is.infinite(e$df[k][!finite])))
  expect_lt(max(abs(e$df[k][finite] - df[finite]) / df[finite]), 1e-10)
  half <- stats::qt(1 - (1 - .95) / 2, df) * sqrt(T_total)
  expect_lt(max(abs(e$ci.lower[k] - (rowMeans(Q) - half))), 1e-10)
  expect_lt(max(abs(e$ci.upper[k] - (rowMeans(Q) + half))), 1e-10)
  # These data discriminate: lavaan.mi's default convention gives other
  # standard errors on the same fits, too small for x4's loading.
  ariv <- lavaan.mi::parameterEstimates.mi(fit, fmi = TRUE, ci = TRUE, level = .95,
    omit.imps = c("no.conv", "no.se"))
  expect_gt(max(abs(ariv$se[k] - sqrt(T_total))), 1e-3)
  x4 <- which(key == "Textual =~ x4")
  expect_lt(ariv$se[k][x4], e$se[k][x4])
})

test_that("sieve_mi_model refuses a pooled variance other than Rubin's parameterwise rules, saying why", {
  expect_error(
    sieve_mi_model("F =~ x1 + x2 + x3", data.frame(), ordered = "x1", pooled_variance = "ariv"),
    "pooled_variance must be \"rubin_parameterwise\"", fixed = TRUE
  )
  expect_error(
    sieve_mi_model("F =~ x1 + x2 + x3", data.frame(), ordered = "x1", pooled_variance = "ariv"),
    "assumes the same fraction of missing information for every parameter", fixed = TRUE
  )
})

test_that("the MI panel names Rubin's parameterwise pooling in its request, its key, the recorded call and its text", {
  ui <- paste(as.character(sieve_mi_ui(shiny::NS("cfa"))), collapse = " ")
  expect_match(ui, "Each parameter is pooled on its own by Rubin's (1987) rules", fixed = TRUE)
  expect_match(ui, "T = W + (1 + 1/M)B", fixed = TRUE)
  expect_match(ui, "df = (M - 1)(1 + W/((1 + 1/M)B))^2", fixed = TRUE)
  expect_match(ui, "FMI = (1 + 1/M)B/T", fixed = TRUE)
  expect_false(grepl("Pooled intervals use Rubin's rules with t reference distributions", ui, fixed = TRUE))

  # The analysis runs in a background process (sieve_mi_start(), replaced
  # here by a finished job); the request it is given is what is checked.
  st <- mi_ordered_study()
  test_frame <- environment()
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_mi_start = mi_fake_start(log = seen),
    sieve_installed = mi_suggests_installed(), .env = test_frame)
  parent <- shiny::reactiveVal(list(spec = shiny::isolate(sieve_cfa_spec(st$meta)),
    fit = list(input = 1L)))
  server <- function(input, output, session) {
    mi <- sieve_mi_server(input, output, session, st, parent, function() TRUE)
  }
  shiny::testServer(server, {
    session$setInputs(mi_sensitivity_go = 1L, mi_sensitivity_seed = 1L)
    session$elapse(600)
    expect_true(is.list(mi$shown()$value))
  })
  expect_identical(seen$args$pooled_variance, "rubin_parameterwise")
  records <- shiny::isolate(st$analyses)
  spec <- records[[length(records)]]$spec
  expect_identical(spec$kind, "ordered_mi")
  expect_identical(spec$pooled_variance, "rubin_parameterwise")
  # The key covers the pooling: the same request without it has another key.
  bare <- spec[setdiff(names(spec), c("key", "pooled_variance"))]
  expect_false(identical(sieve_hash(bare), spec$key))
  expect_identical(sieve_hash(spec[setdiff(names(spec), "key")]), spec$key)
  code <- paste(shiny::isolate(st$codelog), collapse = "\n")
  expect_match(code, "SIEVER::sieve_mi_model(", fixed = TRUE)
  expect_match(code, "pooled_variance = \"rubin_parameterwise\")", fixed = TRUE)
})

test_that("the categorical MI note shows the messages DMAR gave with the estimate", {
  skip_if_not_installed("mice")
  skip_if_not("imputations" %in% names(formals(DMAR::reliability_omega_categorical)))
  test_frame <- environment()
  withr::local_options(sieve.auto_run = "never")
  items <- sieve_hs9_fixture()$theory_map$Textual
  state <- siever_test_study(configure = function(s) {
    d <- s$items
    for (nm in items) d[[nm]] <- as.integer(cut(d[[nm]],
      breaks = stats::quantile(d[[nm]], 0:4 / 4), include.lowest = TRUE))
    d[[items[1L]]][seq(3, 90, 3)] <- NA_real_
    s$items <- d
    s$raw[items] <- d[items]
    s$meta$type[s$meta$item %in% items] <- "ordered"
  })
  # The page's background job, replaced by a finished one whose DMAR call
  # (a complete-case categorical omega, for speed) also gives a message, as
  # DMAR's categorical omega does for its coding note or an undetermined
  # item pair; the run is read exactly as the background process writes it.
  local_mocked_bindings(sieve_ordinal_omega_mi_start = function(call) {
    engine <- function(data, ...) {
      message("The data do not determine the pair (x4, x5); its residual correlation is reported as NA.")
      DMAR::reliability_omega_categorical(stats::na.omit(data[items]), ci_method = "none")
    }
    run <- sieve_dmar_boot_run(call, fun = engine)
    job <- new.env(parent = emptyenv())
    job$results <- list(run)
    job$progress <- list(call = 1L, done = 0L, total = NA_integer_)
    job$poll <- function() FALSE
    job$failure <- function() NULL
    job$kill <- function() invisible(TRUE)
    job$stop <- function() invisible(TRUE)
    job
  }, .env = test_frame)
  shiny::testServer(mod_reliab_server, args = list(state = state), {
    session$setInputs(ordinal_mi_construct = "Textual", ordinal_mi_B = 1000L,
      ordinal_mi_M = 20L, ordinal_mi_seed = 1L, ordinal_mi_go = 1L)
    session$elapse(600)
    note <- paste(as.character(output$ordinal_mi_note$html), collapse = " ")
    expect_match(note, "Seed: 1.", fixed = TRUE)
    expect_match(note, paste0("The engine noted: \"The data do not determine the pair (x4, x5); ",
      "its residual correlation is reported as NA.\""), fixed = TRUE)
  })
})
