# The Bifactor tab's total-score line is robust omega of a one-factor model
# of every item (categorical omega when items are ordered), on the items as
# scored: it differs from the table's Omega through its numerator and its
# item metric, not only its denominator. The tab says so, always, and shows
# beside it the bifactor
# model's own Omega and omegaH with the observed denominator.

bifactor_line_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", paste(as.character(unlist(x)), collapse = " "))
}

bifactor_line_state <- function(standardize = FALSE, ordered = FALSE) {
  fx <- sieve_hs9_fixture()
  siever_test_study(fx, configure = function(s) {
    s$roles$role[s$roles$role == "auxiliary"] <- "criterion"
    items <- s$meta$item
    d <- s$items
    if (standardize) d[items] <- lapply(d[items], function(v) as.numeric(scale(v)))
    if (ordered) {
      # The nine ability tests cut into categories give an improper WLSMV
      # bifactor solution, so the ordered case is four-category responses
      # simulated from a bifactor structure on the same three groups.
      groups <- match(s$meta$construct, unique(s$meta$construct))
      n <- nrow(d)
      sim <- withr::with_seed(1L, {
        g <- stats::rnorm(n)
        f <- matrix(stats::rnorm(n * 3L), n, 3L)
        sapply(seq_along(items), function(j) .7 * g + .5 * f[, groups[j]] + sqrt(1 - .49 - .25) * stats::rnorm(n))
      })
      d[items] <- lapply(seq_along(items), function(j) as.integer(cut(sim[, j], c(-Inf, -1, 0, 1, Inf))))
      s$meta$type <- "ordered"
    }
    s$items <- d
    s$raw[items] <- d[items]
  })
}

test_that("the Bifactor tab's explanation says the total line differs through its numerator and metric", {
  ui <- gsub("\\s+", " ", paste(as.character(mod_bifactor_ui("bf")), collapse = " "))
  expect_false(grepl("takes that variance from the data instead", ui, fixed = TRUE))
  expect_match(ui, paste0("The omega of the raw total score under the table is a different coefficient: ",
    "robust omega (Kelley &amp; Pornprasertmanit, 2016) of a one-factor model of every item."), fixed = TRUE)
  expect_match(ui, "Its numerator is the squared sum of that model's loadings", fixed = TRUE)
  expect_match(ui, "on the items as scored, over the observed variance of their unweighted total", fixed = TRUE)
  expect_match(ui, "through its numerator and its item metric, not only its denominator", fixed = TRUE)
  expect_match(ui, "usually falls between the omegaH and the omega of that same total", fixed = TRUE)
  expect_match(ui, "show how much of any gap the denominator alone accounts for", fixed = TRUE)
  expect_match(ui, "categorical omega (Green &amp; Yang, 2009) of a one-factor model of every item, for the sum of the categories as scored", fixed = TRUE)
})

test_that("with equal item SDs the note still names the numerator, and the observed-denominator row is DMAR's", {
  st <- bifactor_line_state(standardize = TRUE)
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    suppressWarnings(session$setInputs(fit = 1))
    r <- res()
    expect_true(is.data.frame(r$indices))
    text <- bifactor_line_text(output$essential)
    expect_match(text, "Omega of the raw total score under a one-factor model of every item:", fixed = TRUE)
    expect_match(text, sprintf("Robust omega (observed denominator) = %.2f (point estimate", r$total_omega$estimate), fixed = TRUE)
    # Standardized items: the SD ratio is 1, and the note is still shown,
    # naming the numerator.
    expect_match(text, "Note on the two omegas.", fixed = TRUE)
    expect_match(text, "differ by a factor of 1.0 across this pool.", fixed = TRUE)
    expect_false(grepl("dominated by the widest-scaled item", text, fixed = TRUE))
    expect_match(text, "Its numerator is the squared sum of the loadings of a one-factor model of every item", fixed = TRUE)
    expect_match(text, "differ through the numerator and the item metric, not only the denominator", fixed = TRUE)
    expect_match(text, "It is computed on the items as scored", fixed = TRUE)
    # The bifactor model's own Omega and omegaH with the observed denominator.
    general <- names(r$name_map)[r$name_map == "general"]
    ref <- suppressWarnings(DMAR::bifactor_indices(r$fit, general = general, denominator = "observed"))
    ref <- ref[ref$factor == general, ]
    expect_match(text, sprintf("Omega (observed denominator, total) = %.2f, omegaH (general) = %.2f",
      ref$omega, ref$omega_H), fixed = TRUE)
    expect_match(text, "differ from the table's row by the denominator alone", fixed = TRUE)
    # The gap is the numerator's, not the denominator's: the denominator
    # moves the bifactor omega far less than the one-factor total line sits
    # below it.
    gen <- r$indices[r$indices$factor == "general", ]
    expect_lt(abs(ref$omega - gen$omega), abs(r$total_omega$estimate - gen$omega))
  })
})

test_that("with widely different item SDs the note adds the metric to the numerator", {
  st <- bifactor_line_state()
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    suppressWarnings(session$setInputs(fit = 1))
    text <- bifactor_line_text(output$essential)
    expect_match(text, "dominated by the widest-scaled item", fixed = TRUE)
    expect_match(text, "Its numerator is the squared sum of the loadings of a one-factor model of every item", fixed = TRUE)
    expect_match(text, "standardizing the items first removes the difference in metric but not the one in the numerator", fixed = TRUE)
  })
})

test_that("with ordered items the note and the observed-denominator row speak of categorical omega and polychoric correlations", {
  st <- bifactor_line_state(ordered = TRUE)
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    suppressWarnings(session$setInputs(fit = 1))
    r <- res()
    expect_true(is.data.frame(r$indices))
    expect_identical(r$total_omega$coefficient, "omega_categorical")
    text <- bifactor_line_text(output$essential)
    expect_match(text, "It is categorical omega for the sum of the categories as scored", fixed = TRUE)
    expect_match(text, "describes the sum of the latent responses", fixed = TRUE)
    expect_match(text, "over the observed polychoric correlations of the items", fixed = TRUE)
    general <- names(r$name_map)[r$name_map == "general"]
    ref <- suppressWarnings(DMAR::bifactor_indices(r$fit, general = general, denominator = "observed"))
    ref <- ref[ref$factor == general, ]
    expect_match(text, sprintf("Omega (observed denominator, total) = %.2f", ref$omega), fixed = TRUE)
  })
})
