# The Dimensionality page and its engines: the correlation basis applies
# its documented listwise deletion and refuses a request it does not know;
# the model fit by k reads no number of factors from fit-index cutoffs and
# draws no reference lines; and the exploratory and DMAR workers write
# their output to files, say what a dead worker last wrote, and the fit by
# k refuses an unusable item by name before any worker starts.
#
# Author: Ken Kelley

rq_hs <- function(cols = paste0("x", 1:6)) lavaan::HolzingerSwineford1939[, cols]

# Four category versions of continuous scores, cut at their quartiles.
rq_ordered <- function(X) {
  as.data.frame(lapply(X, function(v) {
    as.integer(cut(v, stats::quantile(v, c(0, .25, .5, .75, 1), na.rm = TRUE), include.lowest = TRUE))
  }))
}

rq_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", paste(as.character(unlist(x)), collapse = " "))
}

# ---- The correlation basis ---------------------------------------------------

test_that("sieve_retention_cor() applies its listwise default and reports the rows it set aside", {
  X <- rq_hs()
  X$x1[1:20] <- NA
  # Incomplete rows are set aside and counted, not reported as an item
  # without variance.
  expect_message(
    b <- sieve_retention_cor(X),
    paste0(
      "20 of 301 rows have a missing item response and were set aside (listwise deletion); ",
      "the correlations use the remaining 281 complete rows."
    ),
    fixed = TRUE
  )
  expect_true(b$ok)
  expect_identical(b$used, "pearson")
  expect_null(b$reason)
  expect_identical(b$n, 281L)
  expect_identical(b$n_rows, 301L)
  expect_identical(b$n_dropped, 20L)
  expect_equal(b$R, stats::cor(X[stats::complete.cases(X), ]), tolerance = 1e-14)
  # The recorded R uses the same complete rows and gives the same matrix.
  env <- new.env(parent = globalenv())
  env$active_items <- X
  eval(parse(text = sieve_retention_cor_code(b)), envir = env)
  expect_equal(env$R_items, b$R, tolerance = 1e-14)
  expect_identical(env$n_complete, b$n)
  # Complete data: nothing is set aside, and nothing is said.
  expect_no_message(b0 <- sieve_retention_cor(rq_hs(), missing = "listwise"))
  expect_identical(c(b0$n, b0$n_rows, b0$n_dropped), c(301L, 301L, 0L))
  # Saturated FIML keeps every row and reports the complete-case N.
  f <- sieve_retention_cor(X, missing = "fiml")
  expect_identical(c(f$n, f$n_rows), c(281L, 301L))
  expect_identical(f$missing, "fiml")
})

test_that("polychoric and mixed correlations use the complete rows, and their n is the number of those rows", {
  O <- rq_ordered(rq_hs())
  O[1:25, 1] <- NA
  O[26:40, 2] <- NA
  complete <- stats::complete.cases(O)
  # The matrix is of the complete rows, and its n counts them (not the 301
  # rows of a pairwise matrix).
  expect_message(
    b <- sieve_retention_cor(O, rep("ordered", 6), "polychoric"),
    "40 of 301 rows have a missing item response", fixed = TRUE
  )
  expect_true(b$ok)
  expect_identical(b$used, "polychoric")
  expect_identical(b$n, sum(complete))
  reference <- withr::with_options(list(mc.cores = 1L), suppressWarnings(
    psych::polychoric(O[complete, ], correct = 0.5, progress = FALSE)$rho
  ))
  expect_equal(unname(b$R), unname(reference), tolerance = 1e-10)
  expect_match(sieve_basis_n_text(b), sprintf("n = %d complete cases", sum(complete)), fixed = TRUE)

  # Mixed: parallel analysis takes the matrix with the complete-case N.
  M <- cbind(rq_hs()[, 1:3], rq_ordered(rq_hs()[, 4:6]))
  M[1:30, 4] <- NA
  expect_message(
    bm <- sieve_retention_cor(M, c(rep("continuous", 3), rep("ordered", 3)), "mixed"),
    "30 of 301 rows", fixed = TRUE
  )
  expect_true(bm$ok)
  expect_identical(bm$n, 271L)
  calls <- sieve_pa_dmar_calls(M, bm, 200L, 1L)
  expect_identical(calls$route$form, "matrix")
  expect_identical(calls$calls[[1L]]$args$N, 271L)
})

test_that("sieve_retention_cor() refuses a request, missing data rule, types or aux it does not know, and names it", {
  X <- rq_hs()
  # A request that is not one of the four is refused, never answered with
  # Pearson correlations in its place.
  expect_error(sieve_retention_cor(X, requested = "Polychoric"), paste0(
    "`requested` must be one of \"pearson\", \"polychoric\", \"tetrachoric\" or \"mixed\", spelled in ",
    "lower case; it was \"Polychoric\", and no other correlations were substituted."
  ), fixed = TRUE)
  expect_error(sieve_retention_cor(X, requested = "auto"),
    "it was \"auto\", and no other correlations were substituted. To have the item types choose the correlations (\"auto\"), call sieve_retention_evidence(X, types, cor = \"auto\")",
    fixed = TRUE
  )
  expect_error(sieve_retention_cor(X, requested = c("pearson", "mixed")), "it was c(\"pearson\", \"mixed\")", fixed = TRUE)
  expect_error(sieve_retention_cor(X, requested = NA_character_), "it was NA_character_", fixed = TRUE)
  # A missing data rule it does not know is refused, never read as listwise.
  expect_error(sieve_retention_cor(X, missing = "FIML"),
    "`missing` must be \"listwise\" or \"fiml\", spelled in lower case; it was \"FIML\".", fixed = TRUE
  )
  expect_error(sieve_retention_cor(X, missing = "pairwise"), "it was \"pairwise\"", fixed = TRUE)
  # FIML with polychoric correlations is refused, with the routes that
  # exist.
  expect_error(sieve_retention_cor(rq_ordered(X), rep("ordered", 6), "polychoric", missing = "fiml"), paste0(
    "missing = \"fiml\" estimates Pearson correlations of continuous items by saturated FIML (EM); ",
    "polychoric correlations have no FIML route here, so they are computed from the complete rows. ",
    "Use missing = \"listwise\" with requested = \"polychoric\", or requested = \"pearson\" with FIML."
  ), fixed = TRUE)
  # A basis passed as `types` is refused.
  expect_error(sieve_retention_cor(X, "pearson"),
    "`types` must give one measurement level per item column: X has 6 item columns, and 1 type was given (\"pearson\").",
    fixed = TRUE
  )
  expect_error(sieve_retention_cor(X, rep("interval", 6)),
    "`types` holds \"interval\", which is not a measurement level.", fixed = TRUE
  )
  expect_error(sieve_retention_cor(X, aux = "age"), "X has no column named 'age'", fixed = TRUE)
  # The levels SIEVER records are accepted; NA is read as continuous.
  expect_true(sieve_retention_cor(X, c(NA, rep("continuous", 5)))$ok)
})

test_that("an undefined correlation is attributed to the item without variance, and too few complete rows say so", {
  C <- rq_hs()
  C$x3 <- 4
  b <- sieve_retention_cor(C)
  expect_false(b$ok)
  expect_identical(b$reason, paste0(
    "Pearson correlations could not be computed: Item 'x3' takes a single value in the 301 rows ",
    "used, so its correlations are undefined."
  ))
  # Fewer complete rows than items plus one: the matrix is singular, and no
  # correlations are passed on.
  S <- rq_hs()[1:12, ]
  S$x1[1:8] <- NA
  expect_message(s <- sieve_retention_cor(S), "8 of 12 rows", fixed = TRUE)
  expect_false(s$ok)
  expect_true(is.na(s$used))
  expect_null(s$R)
  expect_identical(s$n, 4L)
  expect_match(s$reason, paste0(
    "The correlations of 6 items need at least 7 complete rows, and only 4 of the 12 rows have every ",
    "item answered. With fewer the sample correlation matrix is singular"
  ), fixed = TRUE)
  expect_match(s$reason, "Use missing = \"fiml\"", fixed = TRUE)
})

test_that("sieve_retention_evidence() asks for FIML only for Pearson correlations of continuous items", {
  X <- rq_hs()
  X$x1[1:20] <- NA
  seen <- new.env()
  local_mocked_bindings(sieve_retention_cor = function(X, types = NULL, requested = "pearson",
                                                       missing = "listwise", aux = NULL) {
    seen$requested <- requested
    seen$missing <- missing
    seen$n <- nrow(X)
    stop("stopped after the basis request")
  })
  # cor = "poly" on continuous items asks for listwise polychoric
  # correlations, never FIML ones (which have no route).
  expect_error(sieve_retention_evidence(X, rep("continuous", 6), cor = "poly", seed = 1), "stopped after the basis request")
  expect_identical(c(seen$requested, seen$missing), c("polychoric", "listwise"))
  expect_identical(seen$n, 281L)
  expect_error(sieve_retention_evidence(X, rep("continuous", 6), cor = "auto", seed = 1), "stopped after the basis request")
  expect_identical(c(seen$requested, seen$missing), c("pearson", "fiml"))
  expect_identical(seen$n, 301L)
})

# ---- Fit-index cutoffs: no count, no reference lines ----------------------------

test_that("no number of factors is read from fit-index cutoffs, in the functions or in the recorded R", {
  ns <- asNamespace("SIEVER")
  expect_false(exists("sieve_fit_conventions", envir = ns, inherits = FALSE))
  expect_false(exists("sieve_fit_cutoff_count", envir = ns, inherits = FALSE))
  expect_false("fit" %in% names(formals(sieve_retention_criteria)))
  expect_false(any(c("cutoffs") %in% c(names(formals(sieve_fit_code)), names(formals(sieve_fit_lavaan_code)))))
  X <- rq_hs(paste0("x", 1:9))
  b <- sieve_retention_cor(X, rep("continuous", 9), "pearson")
  f <- sieve_retention_fit(b, max_k = 3, data = X,
    spec = sieve_lavaan_spec(data.frame(item = names(X), type = "continuous", status = "active")))
  expect_true(f$ok)
  code <- sieve_fit_code(f)
  expect_false(any(grepl("fit_k|meets|rmsea <=|cfi >=|tli >=|Hu and Bentler", code)))
  expect_identical(code[length(code)], "fit_by_k")
  # Each index is still there, the RMSEA with its interval.
  expect_true(all(c("rmsea", "rmsea_lo", "rmsea_hi", "cfi", "tli") %in% names(f$table)))
  expect_identical(f$conf_level, 0.90)
  # The page's source draws no reference line at a conventional value and
  # grades no row against one.
  src <- paste(deparse(mod_retention_server), collapse = " ")
  expect_false(grepl("yintercept = 0.06|yintercept = 0.95|Against the conventions|meets all three|Dotted: the conventional", src))
  expect_false(grepl("sieve_fit_cutoff_count|sieve_fit_conventions|fit_k\\b", src))
})

test_that("the fit by k plot draws each RMSEA interval and no reference line, and the note grades nothing", {
  # The sequence on the nine tests is estimated just above the two seconds
  # at which it runs by itself; the estimate is pinned below that.
  test_frame <- environment()
  local_mocked_bindings(sieve_efa_plan_wait = function(...) 1, .env = test_frame)
  local_mocked_bindings(sieve_vss_wait = function(max_k, p, R = NULL) 1, .env = test_frame)
  fx <- sieve_hs9_fixture()
  st <- siever_test_study(fx, labels = FALSE)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "pearson", criterion = "p95", n_iter = 200, max_k = 3)
    expect_true(isTRUE(fit_run()$ok))
    gg <- fit_figure()
    geoms <- vapply(gg$layers, function(l) class(l$geom)[1L], character(1))
    expect_false("GeomHline" %in% geoms)
    expect_true("GeomErrorbar" %in% geoms)
    expect_match(gg$labels$subtitle, "Bars: the 90% RMSEA interval.", fixed = TRUE)
    expect_false(grepl("conventional|Dotted", gg$labels$subtitle))
    note <- rq_text(output$fit_note)
    expect_match(note, "no value of an index is applied as a cutoff, and no number of factors is read from them", fixed = TRUE)
    expect_false(grepl("Hu and Bentler|smallest k|RMSEA &lt;=|CFI &gt;=", note))
    tbl <- rq_text(output$fit_tbl)
    expect_match(tbl, "RMSEA 90% CI", fixed = TRUE)
    expect_false(grepl("Against the conventions|meets all three|does not meet", tbl))
    expect_false("fit" %in% criteria()$id)
  })
})

# ---- Background workers and unusable items ------------------------------------

test_that("sieve_efa_fit_by_k() refuses numbers of factors that are not whole numbers of at least 1, naming them", {
  X <- rq_hs()
  # 2.5 is not truncated to 2, and 0, NA and Inf are not dropped: each is
  # refused by name.
  expect_error(sieve_efa_fit_by_k(X, "ML", ks = c(1, 2.5)),
    "`ks` must hold whole numbers of factors of at least 1, and 2.5 is not", fixed = TRUE
  )
  expect_error(sieve_efa_fit_by_k(X, "ML", ks = c(0, 1)), "and 0 is not", fixed = TRUE)
  expect_error(sieve_efa_fit_by_k(X, "ML", ks = c(NA, 2)), "and NA is not", fixed = TRUE)
  expect_error(sieve_efa_fit_by_k(X, "ML", ks = c(-1, Inf)), "and -1, Inf are not", fixed = TRUE)
  expect_error(sieve_efa_fit_by_k(X, "ML", ks = "2"), "it was \"2\"", fixed = TRUE)
  expect_error(sieve_efa_fit_by_k(X, "ML", ks = numeric(0)), "it was empty", fixed = TRUE)
  expect_error(sieve_efa_fit_by_k(X, "ML", max_k = 2.5),
    "`max_k` must be a single whole number of factors of at least 1; it was 2.5", fixed = TRUE
  )
  expect_identical(sieve_efa_fit_by_k(X, "ML", ks = c(2, 1, 2))$k, 1:2)
})

test_that("sieve_efa_fit_by_k() names an item lavaan cannot fit instead of printing lavaan's variable table", {
  X <- rq_hs()
  X$x2 <- 5
  printed <- utils::capture.output(
    err <- tryCatch(sieve_efa_fit_by_k(X, "ML", ks = 1:2), error = function(e) e)
  )
  expect_s3_class(err, "sieve_unusable_items")
  expect_identical(conditionMessage(err), paste0(
    "No exploratory model was fit. Item 'x2' takes the same value (5) for every case, so it carries ",
    "no information and the model cannot be fit. Leave the item out of `data`, or check that its values ",
    "were read correctly."
  ))
  expect_identical(printed, character(0))
  X$x2 <- NA_real_
  expect_error(sieve_efa_fit_by_k(X, "ML", ks = 1L), "Item 'x2' has no observed values", fixed = TRUE)
  # An ordered factor with one observed level is screened on its codes.
  O <- rq_ordered(rq_hs())
  O[] <- lapply(O, ordered)
  O$x4 <- ordered(rep("agree", nrow(O)))
  expect_error(sieve_efa_fit_by_k(O, "WLSMV", ordered = names(O), ks = 1L), "Item 'x4' takes the same value", fixed = TRUE)
})

# Waits for a worker's process to end. One still running after 600 seconds
# (a limit that allows for a busy machine) fails the test and is stopped.
# Returns TRUE when the process ended by itself.
rq_wait_ended <- function(process, what) {
  ended <- siever_wait_until(function() !process$is_alive(), what, step = function() process$wait(500))
  if (!ended) process$kill()
  ended
}

test_that("an exploratory worker whose engine prints its whole variable table finishes, with 60 and more items", {
  skip_on_cran()
  # lavaan prints about 74 bytes per item at every k; a worker whose pipe
  # nobody read would wait forever past about 8 KB, so the output goes to
  # files.
  set.seed(1)
  n <- 300
  p <- 60
  X <- as.data.frame(matrix(stats::rnorm(n * p), n, p))
  names(X) <- sprintf("item_%03d", seq_len(p))
  X$item_001 <- 3
  key <- list(estimator = "ML", ordered = NULL, rotation = "none", missing = NULL)
  started <- proc.time()[["elapsed"]]
  h <- sieve_efa_worker_start(X, key, k = 1:6)
  withr::defer(h$kill())
  if (!rq_wait_ended(h, "The exploratory worker")) return(invisible())
  expect_false(h$is_alive())
  # On an idle machine the worker ends within a minute.
  if (siever_timing_checks()) expect_lt(proc.time()[["elapsed"]] - started, 60)
  expect_true(file.exists(file.path(h$dir, "stdout.txt")))
  expect_gt(file.size(file.path(h$dir, "stdout.txt")), 16384)
  tables <- h$get_result()
  expect_length(tables, 6L)
  expect_true(all(vapply(tables, function(t) t$status, character(1)) == "failed"))
  expect_false(dir.exists(h$dir))
})

test_that("both kinds of background worker finish after writing 200 KB to each stream, and a dead one says what it last wrote", {
  skip_on_cran()
  test_frame <- environment()
  original <- get("sieve_worker_bootstrap", asNamespace("SIEVER"))
  chatty <- original
  body(chatty) <- substitute({
    BODY
    cat(strrep("o", 200000), "\n")
    cat(strrep("e", 200000), "\n", file = stderr())
  }, list(BODY = body(original)))
  local_mocked_bindings(sieve_worker_bootstrap = chatty, .env = test_frame)
  X <- rq_hs()
  key <- list(estimator = "ML", ordered = NULL, rotation = "none", missing = NULL)
  h <- sieve_efa_worker_start(X, key, k = 1L)
  if (!rq_wait_ended(h, "The exploratory worker")) return(invisible())
  expect_false(h$is_alive())
  expect_identical(h$get_result()$status, "ok")
  job <- sieve_efa_dmar_job()
  expect_true(sieve_efa_dmar_submit(job, key = "chatty", call = list(fun = "cov_fiml", args = list(X))))
  if (!rq_wait_ended(job$process, "The DMAR worker")) return(invisible())
  expect_false(job$process$is_alive())
  expect_true(sieve_efa_dmar_poll(job))
  expect_true(job$record$ok)

  # A worker that dies: the stated cause carries the last lines of its
  # error stream, which the pipes keep rather than discard.
  dying <- original
  body(dying) <- substitute({
    BODY
    cat("siever test: first line\n\n   siever test: the worker stopped here   \n", file = stderr())
    quit(save = "no", status = 3L)
  }, list(BODY = body(original)))
  local_mocked_bindings(sieve_worker_bootstrap = dying, .env = test_frame)
  h <- sieve_efa_worker_start(X, key, k = 1L)
  if (!rq_wait_ended(h, "The exploratory worker")) return(invisible())
  cause <- tryCatch(h$get_result(), error = sieve_efa_worker_cause)
  expect_match(cause, "The worker may have been stopped or run out of memory.", fixed = TRUE)
  expect_match(cause,
    "The last lines it wrote to its error stream: \"siever test: first line\"; \"siever test: the worker stopped here\".",
    fixed = TRUE
  )
  job <- sieve_efa_dmar_job()
  sieve_efa_dmar_submit(job, key = "dying", call = list(fun = "cov_fiml", args = list(X)))
  if (!rq_wait_ended(job$process, "The DMAR worker")) return(invisible())
  expect_true(sieve_efa_dmar_poll(job))
  expect_false(job$record$ok)
  expect_match(job$record$error, "\"siever test: the worker stopped here\"", fixed = TRUE)
})

test_that("the Dimensionality page refuses the fit by k by the item's name before any worker starts", {
  test_frame <- environment()
  started <- new.env()
  started$calls <- list()
  local_mocked_bindings(sieve_efa_worker_start = function(data, key, k) {
    started$calls <- c(started$calls, list(k))
    stop("no worker may start in this test")
  }, .env = test_frame)
  set.seed(3)
  n <- 400
  p <- 60
  f <- matrix(stats::rnorm(n * 2), n, 2)
  L <- matrix(stats::runif(p * 2, .3, .7), p, 2)
  X <- as.data.frame(f %*% t(L) + matrix(stats::rnorm(n * p, sd = .7), n, p))
  items <- sprintf("q%03d", seq_len(p))
  names(X) <- items
  X$q001 <- 4
  fx <- list(name = "constant item", data = cbind(id = seq_len(n), X), item_cols = items,
    load_code = "d <- constant_item", id_vars = "id", theory_map = NULL, theory_k = NULL,
    theory_lbl = NULL, labels = NULL)
  st <- siever_test_study(fx, labels = FALSE, theory = FALSE)
  expect_identical(shiny::isolate(st$meta)$status[shiny::isolate(st$meta)$item == "q001"], "active")
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "pearson", criterion = "mean", max_k = 8, n_iter = 200, workers = 4)
    plan <- fit_plan()
    expect_match(plan$refused, "The model fit by k was not started.", fixed = TRUE)
    expect_match(plan$refused, "Item 'q001' takes the same value (4) for every case", fixed = TRUE)
    f <- fit_run()
    expect_identical(f$status, "not run")
    expect_true(isTRUE(f$refused))
    expect_match(rq_text(output$fit_control), "Item 'q001' takes the same value (4) for every case", fixed = TRUE)
    expect_false(grepl("fit_go", rq_text(output$fit_control), fixed = TRUE))
    session$setInputs(fit_go = 1)
    expect_null(fit_pending())
    expect_false(isTRUE(update_plan()$fit))
    expect_false(isTRUE(update_plan()$new_fit))
  })
  expect_length(started$calls, 0L)
})
