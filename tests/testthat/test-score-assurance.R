# Every score display names the specification it was computed from. The
# composite, boundary, SEM Monte Carlo and IRT diagnostic panels compare
# their own settings as well as the parent fit's identity, so an edited
# weight, B, seed, pair, bound, theta grid, item or level does not leave an
# earlier result on screen as current; the operational score card does not
# say "No specification kept" while one is; the Validity tab's score
# control does not keep its last value under a mixed specification; and
# the factor-score preview says that it is not the operational score.
#
# Author: Ken Kelley

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

sa_text <- function(x) {
  if (is.null(x)) return("")
  html <- if (inherits(x, c("shiny.tag", "shiny.tag.list"))) as.character(x) else paste(as.character(unlist(x)), collapse = " ")
  gsub("&amp;", "&", gsub("\\s+", " ", gsub("<[^>]+>", " ", html)), fixed = TRUE)
}

sa_nine <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = TRUE, name = fx$name, configure = function(s) {
  s$roles <- sieve_declared_roles(fx$data, fx$item_cols)
  s$estimator <- "MLR"

  })
  s
}

# ---- Each panel compares its own controls -----------------------------------

test_that("the CFA page's composite reliability is from before once its weights, replications or seed change", {
  st <- sa_nine()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(scope = "construct", structure = "first", ident = "varone", min_path = 0)
    session$setInputs(fit_go = 1L)
    session$setInputs(composite_weights = "", composite_B = 0, composite_seed = NA)
    session$setInputs(composite_go = 1L)
    first <- sa_text(output$composite_note)
    expect_false(grepl("other settings", first, fixed = TRUE))
    session$setInputs(composite_weights = "1, 1, 2, 1, 1, 1, 1, 1, 1")
    expect_match(sa_text(output$composite_note), "computed with other settings", fixed = TRUE)
    # The controls back as they were: the kept result is the current one.
    session$setInputs(composite_weights = "")
    expect_identical(sa_text(output$composite_note), first)
    session$setInputs(composite_B = 200)
    expect_match(sa_text(output$composite_note), "computed with other settings", fixed = TRUE)
    session$setInputs(composite_B = 0, composite_seed = 11)
    expect_match(sa_text(output$composite_note), "computed with other settings", fixed = TRUE)
  })
})

test_that("the Validity page's boundary comparison is from before once its pair or bound changes", {
  st <- sa_nine()
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(estimator = "study", score_method = "mean")
    session$setInputs(fit_go = 1L)
    lv <- lavaan::lavNames(cfa_run()$fit, "lv")
    expect_length(lv, 3L)
    session$setInputs(latent_pair = lv[1:2], latent_bound = .9)
    session$setInputs(latent_bound_go = 1L)
    note <- sa_text(output$latent_bound_note)
    expect_false(grepl("another pair or bound", note, fixed = TRUE))
    expect_true(nzchar(trimws(note)))
    session$setInputs(latent_bound = .8)
    expect_match(sa_text(output$latent_bound_note), "computed for another pair or bound than the one shown", fixed = TRUE)
    session$setInputs(latent_bound = .9)
    expect_identical(sa_text(output$latent_bound_note), note)
    # The same pair chosen in the other order is the same request.
    session$setInputs(latent_pair = rev(lv[1:2]))
    expect_identical(sa_text(output$latent_bound_note), note)
    session$setInputs(latent_pair = lv[c(1L, 3L)])
    expect_match(sa_text(output$latent_bound_note), "another pair or bound", fixed = TRUE)
  })
})

test_that("the SEM Monte Carlo intervals are from before once the replications or the seed change", {
  test_frame <- environment()
  testthat::local_mocked_bindings(
    sieve_indirect_mc = function(fit, B, seed) {
      list(table = data.frame(effect = "a*b", estimate = .2, B = B), method = sprintf("Monte Carlo, B = %d.", B),
        B = B, rng = list(seed = seed %||% 1L), counts = NULL, warnings = character(0))
    },
    sieve_sem_code = function(spec, fit_name = "fit", ...) "# the structural model",
    sieve_register_inference = function(...) invisible("A1"),
    .env = test_frame
  )
  st <- sa_nine()
  run <- structure(
    list(fit = "a fitted model", spec = list(key = "k1"), status = list(code = "ok", message = "Converged.")),
    class = "sieve_result"
  )
  shiny::testServer(function(input, output, session) {
    mc <- sieve_sem_evidence_server(input, output, session, st,
      parent = function() run, mediation = function() run, current = function() TRUE
    )
  }, {
    session$setInputs(indirect_B = 1000, indirect_seed = 5)
    session$setInputs(indirect_mc_go = 1L)
    expect_match(sa_text(output$indirect_mc_note), "Monte Carlo, B = 1000.", fixed = TRUE)
    session$setInputs(indirect_B = 2000)
    expect_match(sa_text(output$indirect_mc_note), "computed with other replications or another seed", fixed = TRUE)
    session$setInputs(indirect_B = 1000, indirect_seed = 6)
    expect_match(sa_text(output$indirect_mc_note), "computed with other replications or another seed", fixed = TRUE)
    session$setInputs(indirect_seed = 5)
    expect_match(sa_text(output$indirect_mc_note), "Monte Carlo, B = 1000.", fixed = TRUE)
  })
})

# ---- The operational score card ---------------------------------------------

test_that("the operational score card states the specification in force, previews unkept settings, and never claims none is kept", {
  st <- sa_nine()
  sieve_set_score_specification(st, "Visual", method = "sum", min_count = 3L)
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(policy_construct = "Textual", policy_method = "mean", policy_count = 1, policy_prop = 0)
    note <- sa_text(output$policy_note)
    expect_false(grepl("No specification kept", note, fixed = TRUE))
    expect_match(note, paste(
      "In force: Visual: prorated sum of the answered items, at least 3 answered (kept).",
      "Textual and Speed: mean of the answered items, at least 1 answered (the default)."
    ), fixed = TRUE)
    expect_false(grepl("Not yet kept", note, fixed = TRUE))
    # Settings that differ from the stored ones are previewed, not kept.
    session$setInputs(policy_count = 2)
    note <- sa_text(output$policy_note)
    expect_match(note, "Not yet kept for Textual: mean of the answered items, at least 2 of 3 answered items; 301 respondents would be eligible now. Keep applies it.", fixed = TRUE)
    expect_null(shiny::isolate(state$scoring_policies$Textual))
    session$setInputs(policy_apply = 1L)
    note <- sa_text(output$policy_note)
    expect_match(note, "Specification kept for Textual: mean of the answered items; at least 2 of 3 answered items", fixed = TRUE)
    expect_match(note, "Textual: mean of the answered items, at least 2 answered (kept).", fixed = TRUE)
    expect_false(grepl("Not yet kept", note, fixed = TRUE))
    # A change made elsewhere (the Validity tab's score control) retires the
    # last-action line, and the sentence in force follows it.
    sieve_set_score_specification(state, "Textual", method = "regression")
    session$flushReact()
    note <- sa_text(output$policy_note)
    expect_false(grepl("Specification kept for Textual", note, fixed = TRUE))
    expect_match(note, "Textual: regression factor score, at least 2 answered (kept).", fixed = TRUE)
  })
  # The sentence on its own.
  spec <- sieve_score_spec_build(shiny::isolate(st$meta), list(), available = names(shiny::isolate(st$items)))
  expect_identical(sieve_score_policy_sentence(spec, list()),
    "Visual, Textual and Speed: mean of the answered items, at least 1 answered (the default).")
  expect_identical(sieve_score_policy_sentence(list(), list()), "")
})

# ---- The Validity tab's score control under a mixed specification ----------

test_that("the Validity tab's score control offers the mixed state, names the scores in force, and applies nothing when it is chosen", {
  html <- paste(as.character(mod_validity_ui("validity")), collapse = "")
  expect_match(html, "As specified per construct", fixed = TRUE)
  st <- sa_nine()
  sieve_set_score_specification(st, "Visual", method = "sum")
  before <- shiny::isolate(st$scoring_policies)
  shiny::testServer(mod_validity_server, args = list(state = st), {
    line <- sa_text(output$score_method_state)
    expect_match(line, "In force, construct by construct: Visual: prorated sum of the answered items, at least 1 answered (kept).", fixed = TRUE)
    expect_match(line, "Choosing a score above sets it for every construct.", fixed = TRUE)
    session$setInputs(score_method = "mixed")
    expect_identical(shiny::isolate(state$scoring_policies), before)
    # Choosing one of the scores sets every construct, and the line goes.
    session$setInputs(score_method = "bartlett")
    methods <- vapply(sieve_score_specification(state), `[[`, character(1), "method")
    expect_true(all(methods == "bartlett"))
    expect_identical(sa_text(output$score_method_state), "")
  })
})

# ---- The factor-score preview beside the operational score -----------------

test_that("the factor-score preview says it applies no minimum answered rule and names the operational score in force", {
  st <- sa_nine()
  sieve_set_score_specification(st, "Visual", method = "sum", min_count = 2L)
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(denominator = "model_implied", ci_method = "mlr", missing = "listwise",
      conf_level = 0.95, B = 1000, boot_seed = NA, show_alpha = TRUE, score_method = "Bartlett")
    note <- sa_text(output$scores_note)
    expect_match(note, "This preview applies no minimum answered rule and is not the operational score in force (Operational scores): Visual: prorated sum of the answered items, at least 2 answered (kept).", fixed = TRUE)
    for (cn in c("Visual", "Textual", "Speed")) sieve_set_score_specification(state, cn, method = "bartlett")
    session$flushReact()
    note <- sa_text(output$scores_note)
    expect_match(note, "Every construct's operational score is this factor score; the preview applies no minimum answered rule", fixed = TRUE)
    session$setInputs(score_method = "regression")
    expect_match(sa_text(output$scores_note), "This preview applies no minimum answered rule and is not the operational score in force", fixed = TRUE)
  })
})
