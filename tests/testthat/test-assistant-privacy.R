assistant_privacy_state <- function() {
  fx <- sieve_hs9_fixture()
  state <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = FALSE, name = NULL, configure = function(state) {
  state$estimator <- "MLR"

  })
  state
}

test_that("assistant tasks obey the same sampling design and reject edited plans", shiny::isolate({
  state <- assistant_privacy_state()
  state$sampling_design <- list(unit = "repeated")
  expect_error(sieve_assistant_plan(state, "Inspect reliability", "reliability"), "declared sampling unit")
  expect_error(sieve_assistant_plan(state, "Inspect structure", "cfa"), "Repeated observations")
  state$sampling_design <- list(unit = "independent")
  plan <- sieve_assistant_plan(state, "Inspect structure", "cfa")
  changed <- plan
  # An edit is a different estimator from the plan's (ML with FIML is the
  # default, so the edit names another one).
  changed$specs[[1L]]$estimator <- if (identical(plan$specs[[1L]]$estimator, "MLR")) "ML" else "MLR"
  expect_error(sieve_assistant_execute(state, changed), "project or plan changed")
  state$sampling_design <- list(unit = "clustered", cluster = "school")
  expect_error(sieve_assistant_execute(state, plan), "project or plan changed")
  expect_error(sieve_assistant_plan(state, "Inspect reliability", "reliability"), "declared sampling unit")
  cluster <- sieve_assistant_plan(state, "Inspect structure", "cfa")
  direct <- shiny::isolate(sieve_sampling_apply(sieve_cfa_spec(state$meta,
    reserved = names(state$items), estimator = sieve_estimator_effective("study", state)
  ), state, state$items))
  expect_identical(cluster$specs[[1L]], direct$spec)
  expect_identical(cluster$context$sampling_design, list(unit = "clustered", cluster = "school"))
}))

test_that("claim and evaluation edits cannot be hidden by recomputing a project content key", shiny::isolate({
  state <- assistant_privacy_state()
  claim <- sieve_freeze_claim(state, "A positive relation in the declared population")
  sieve_evaluate_claim(state, claim$id, "inconclusive", "The interval is wide")
  expect_true(all(sieve_claims_check(state)$intact))
  snapshot <- sieve_project_snapshot(state)
  snapshot$other$frozen_claims[[1L]]$prediction <- "A negative relation"
  snapshot$keys$content <- sieve_content_key(snapshot)
  check <- sieve_claims_check(c(snapshot$other, list(codelog = snapshot$codelog)))
  expect_true(all(check$intact %in% FALSE))
  expect_true(any(grepl("claim|evaluation", sieve_project_check(snapshot), ignore.case = TRUE)))
  state$validation_events[[1L]]$outcome <- "supported within stated limits"
  expect_error(sieve_evaluate_claim(state, claim$id, "contradicted", "A new result"), "no longer match")
  expect_error(sieve_freeze_claim(state, "Another claim"), "no longer match")
}))

test_that("verified claim export includes a checkable SHA-256 and no registration claim", shiny::isolate({
  state <- assistant_privacy_state()
  claim <- sieve_freeze_claim(state, "A prospective prediction", "prospective_declaration")
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path <- file.path(dir, "claim.txt")
  written <- sieve_claim_write(state, claim$id, path)
  expect_true(all(file.exists(written)))
  # The expected digest comes from an implementation other than the one that
  # wrote the checksum file: tools::sha256sum() from R 4.5.0, where it
  # first exists, and digest before it, since SIEVER runs on R 4.1.0 and
  # later.
  expected <- if (getRversion() >= "4.5.0") {
    unname(tools::sha256sum(path))
  } else {
    skip_if_not_installed("digest")
    digest::digest(path, algo = "sha256", file = TRUE)
  }
  expect_match(readLines(written[[2L]])[1L], paste0("^", expected, "[[:space:]]+claim[.]txt$"))
  # Where both exist they agree, so the oracle below R 4.5.0 is the same.
  if (getRversion() >= "4.5.0" && requireNamespace("digest", quietly = TRUE)) {
    expect_identical(digest::digest(path, algo = "sha256", file = TRUE), expected)
  }
  expect_match(paste(readLines(path), collapse = "\n"), "does not establish independent registration", fixed = TRUE)
}))

test_that("default study bundles contain no withdrawn identifiers raw rows or private history", shiny::isolate({
  state <- assistant_privacy_state()
  raw <- shiny::isolate(state$raw)
  raw$id <- paste0("IDENTIFIER_", seq_len(nrow(raw)))
  second_item <- names(shiny::isolate(state$items))[2L]
  raw[[second_item]][7L] <- 987654.321
  state$items[[second_item]][7L] <- 987654.321
  attr(raw[[names(shiny::isolate(state$items))[1L]]], "label") <- "PRIVATE_ATTRIBUTE_WORDING"
  rownames(raw) <- paste0("PRIVATE_ROWNAME_", seq_len(nrow(raw)))
  state$raw <- raw
  state$assistant_records <- list(list(kind = "proposal", response = "PRIVATE_PROVIDER_PROPOSAL"))
  state$meta$note <- "PRIVATE_LEDGER_NOTE"
  state$meta$wording <- "PRIVATE_ITEM_WORDING"
  sieve_record_construct(state, "Private definition", "PRIVATE_CONSTRUCT_NARRATIVE")
  sieve_apply_reverse(state, names(shiny::isolate(state$items))[1L])
  sieve_set_case_status(state, 7L, "excluded", reason = "Participant withdrew consent")
  snapshot <- sieve_project_snapshot(state)
  path <- tempfile(fileext = ".tar.gz")
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(c(path, dir), recursive = TRUE), add = TRUE)
  manifest <- sieve_export_study_bundle(snapshot, path)
  utils::untar(path, exdir = dir, tar = "internal")
  shared <- sieve_project_read(file.path(dir, "project.siever.rds"))
  expect_equal(nrow(shared$raw), nrow(raw) - 1L)
  expect_identical(names(shared$raw), names(shiny::isolate(state$items)))
  expect_false(any(shared$raw[[second_item]] == 987654.321, na.rm = TRUE))
  expect_false(any(shared$items[[second_item]] == 987654.321, na.rm = TRUE))
  expect_length(shared$other$removed_cases, 0L)
  expect_length(shared$other$assistant_records, 0L)
  expect_length(shared$other$construct_records, 0L)
  expect_length(shared$analyses, 0L)
  expect_false(manifest$complete_working_project)
  contents <- paste(capture.output(dput(shared)), collapse = "\n")
  text_files <- setdiff(list.files(dir, full.names = TRUE), file.path(dir, "project.siever.rds"))
  contents <- paste(
    contents,
    paste(
      vapply(
        text_files,
        function(f) paste(
          readLines(f,
            warn = FALSE
          ),
          collapse = "\n"
        ),
        character(1)
      ),
      collapse = "\n"
    )
  )
  for (sentinel in c(
    "IDENTIFIER_", "PRIVATE_ATTRIBUTE_WORDING", "PRIVATE_ROWNAME_", "PRIVATE_PROVIDER_PROPOSAL",
    "PRIVATE_LEDGER_NOTE", "PRIVATE_ITEM_WORDING", "PRIVATE_CONSTRUCT_NARRATIVE"
  )) {
    expect_false(grepl(sentinel, contents, fixed = TRUE), info = sentinel)
  }
  # A normal private backup retains the excluded row for authorized restoration.
  expect_identical(snapshot$other$removed_cases[[1L]]$raw$id, "IDENTIFIER_7")
}))

test_that("payload minimization excludes old analyses and private ledger notes", shiny::isolate({
  state <- assistant_privacy_state()
  state$meta$note <- "PRIVATE_NOTE"
  state$meta$wording <- "PRIVATE_WORDING"
  sieve_freeze_claim(state, "A claim")
  revision <- sieve_revision(state)
  old <- revision
  old$data_key <- "older-revision"
  state$analyses <- list(
    A1 = list(id = "A1", kind = "cfa", revision = old, estimates = c(cfi = .9)),
    A2 = list(id = "A2", kind = "cfa", revision = revision, estimates = c(cfi = .95)),
    A3 = list(id = "A3", kind = "cfa", revision = revision, estimates = c(cfi = .97))
  )
  context <- sieve_assistant_context(state)
  expect_identical(vapply(context$analyses, function(x) x$id, character(1)), c(A3 = "A3"))
  expect_false(grepl("PRIVATE_NOTE|PRIVATE_WORDING", paste(capture.output(dput(context)), collapse = "\n")))
  expect_length(sieve_assistant_context(state, analysis_ids = character())$analyses, 0L)
  expect_length(sieve_assistant_context(state, claim_ids = character())$claims, 0L)
  expect_error(sieve_assistant_request("OpenAI", "explicit-model", strrep("x", 1001), max_chars = 1000L), "exceeds")
  expect_error(sieve_assistant_request("OpenAI", "model\ncat('injected')", "Synthetic payload"), "line breaks")
}))

test_that("provider credentials are redacted for both request formats", shiny::isolate({
  skip_if_not_installed("httr2", "1.1.0")
  key <- "DUMMY-NOT-A-REAL-KEY-ASSISTANT"
  for (provider in c("OpenAI", "Anthropic")) {
    metadata <- sieve_assistant_request(provider, "explicit-model", "Synthetic payload")
    request <- sieve_assistant_http_request(metadata, key)
    expect_false(grepl(key, paste(capture.output(print(request)), collapse = "\n"), fixed = TRUE))
    serialized <- serialize(request, NULL, ascii = FALSE)
    expect_length(grepRaw(charToRaw(key), serialized, fixed = TRUE), 0L)
  }
}))

test_that("provider failures are categorized and incomplete text is labeled without a live call", shiny::isolate({
  skip_if_not_installed("httr2", "1.1.0")
  request <- sieve_assistant_request("OpenAI", "explicit-model", "Synthetic payload")
  for (status in c(401L, 403L, 404L, 413L, 429L, 503L)) {
    reply <- httr2::response(status,
      headers = list(`content-type` = "application/json"),
      body = charToRaw('{"error":{"type":"synthetic_failure"}}')
    )
    category <- switch(as.character(status),
      `401` = "authentication or permission",
      `403` = "authentication or permission",
      `404` = "unknown model",
      `413` = "invalid or oversized payload",
      `429` = "rate or billing limit",
      `503` = "provider outage"
    )
    httr2::with_mocked_responses(list(reply), {
      expect_error(sieve_assistant_send(request, "dummy-secret", TRUE), category, class = "siever_provider_error")
    })
  }
  html <- httr2::response(200L,
    headers = list(`content-type` = "text/html"),
    body = charToRaw("<html>dummy-secret</html>")
  )
  httr2::with_mocked_responses(
    list(html),
    expect_error(
      sieve_assistant_send(
        request,
        "dummy-secret",
        TRUE
      ),
      "unreadable response"
    )
  )
  incomplete <- httr2::response(200L,
    headers = list(`content-type` = "application/json"),
    body = charToRaw('{"status":"incomplete","output":[{"type":"message","content":[{"type":"output_text","text":"Synthetic proposal"}]}]}')
  )
  httr2::with_mocked_responses(list(incomplete), {
    text <- sieve_assistant_send(request, "dummy-secret", TRUE)
    expect_false(attr(text, "complete"))
    expect_match(text, "Truncated at the output limit", fixed = TRUE)
  })
}))

test_that("assistant reliability results render one row per construct with interval reasons", shiny::isolate({
  state <- assistant_privacy_state()
  result <- sieve_assistant_execute(state, sieve_assistant_plan(state, "Inspect reliability", "reliability"))
  tables <- sieve_assistant_result_tables(result)
  expect_length(tables, 1L)
  expect_equal(nrow(tables[[1L]]), 3L)
  expect_true(all(is.finite(tables[[1L]]$omega)))
  expect_true(all(nzchar(tables[[1L]]$omega_CI)))
  numeric <- sieve_readable_result_tables(c(CFI = .987654321))
  expect_true(is.numeric(numeric[[1L]]$value))
  expect_false(grepl("0.987654321", as.character(sieve_plain_table_ui(numeric[[1L]])), fixed = TRUE))
}))

test_that("external sends leave an automatic disclosure and unrelated proposals remain current", shiny::isolate({
  state <- assistant_privacy_state()
  testthat::local_mocked_bindings(sieve_assistant_send = function(...) "Synthetic proposal only")
  shiny::testServer(mod_project_server, args = list(state = state), {
    for (i in 1:2) {
      session$setInputs(
        assist_goal = paste("Distinct research goal", i), assist_task = "decision",
        assist_provider = "OpenAI", assist_model = "explicit-model", assist_key = "dummy-secret"
      )
      session$setInputs(assist_plan = i, assist_prepare_external = i)
      session$setInputs(assist_consent = FALSE)
      session$setInputs(assist_consent = TRUE, assist_send = i)
      expect_length(Filter(function(x) identical(x$kind, "disclosure"), state$assistant_records), i)
      session$setInputs(assist_disposition = "adopted", assist_revision = paste("My judgment", i), assist_record = i)
    }
    session$setInputs(assist_record = 3)
    expect_length(Filter(function(x) identical(x$kind, "proposal"), state$assistant_records), 2L)
    expect_true(all(sieve_decisions_table(state)$status == "current"))
    report <- sieve_assistant_disclosure_report(state)
    expect_match(report, "explicit-model", fixed = TRUE)
    expect_match(report, "My judgment 1", fixed = TRUE)
    expect_false(grepl("dummy-secret",
      paste(capture.output(dput(sieve_project_snapshot(state))),
        collapse = "\n"
      ),
      fixed = TRUE
    ))
  })
}))


test_that("a failed authorized send is disclosed and consent cannot retry it", shiny::isolate({
  state <- assistant_privacy_state()
  calls <- 0L
  testthat::local_mocked_bindings(sieve_assistant_send = function(...) {
    calls <<- calls + 1L
    stop("Synthetic timeout; no scientific setting was changed.", call. = FALSE)
  })
  shiny::testServer(mod_project_server, args = list(state = state), {
    session$setInputs(
      assistant = 1, assist_goal = "External review of a goal", assist_task = "decision",
      assist_provider = "Anthropic", assist_model = "explicit-model", assist_key = "dummy-secret"
    )
    session$setInputs(assist_plan = 1, assist_prepare_external = 1)
    session$setInputs(assist_consent = TRUE, assist_send = 1)
    expect_length(state$assistant_records, 1L)
    expect_identical(state$assistant_records[[1L]]$outcome, "failed")
    expect_match(paste(state$codelog, collapse = "\n"), "External disclosure to Anthropic/explicit-model", fixed = TRUE)
    session$setInputs(assist_send = 2)
    expect_identical(calls, 1L)
    expect_length(state$assistant_records, 1L)
  })
}))

test_that("the payload authorization includes liability at the consent control", shiny::isolate({
  state <- assistant_privacy_state()
  shown <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    showModal = function(ui, ...) shown$ui <- ui,
    .package = "shiny"
  )
  shiny::testServer(mod_project_server, args = list(state = state), {
    session$setInputs(assistant = 1)
    html <- as.character(shown$ui)
    at <- regexpr(paste0('id="', session$ns("assist_consent"), '"'), html, fixed = TRUE)[1L]
    expect_gt(at, 0)
    consent <- substring(html, at, at + 600L)
    expect_match(consent, "liability", fixed = TRUE)
    expect_match(consent, "exact disclosed payload", fixed = TRUE)
  })
}))


test_that(
  "dependent sampling declarations cannot leak into independent-row drafts or saved-context summaries",
  shiny::isolate({
    state <- assistant_privacy_state()
    revision <- sieve_revision(state)
    state$analyses <- list(A1 = list(
      id = "A1", kind = "reliability", revision = revision,
      spec = list(kind = "reliability"), estimates = list()
    ))
    state$sampling_design <- list(unit = "clustered", cluster = "school")
    expect_length(sieve_assistant_context(state)$analyses, 0L)
    testthat::local_mocked_bindings(sieve_run_cfa = function(...) stop("Engine must not run"))
    expect_error(
      sieve_methods_draft(state$items,
        state$meta,
        sampling_design = state$sampling_design
      ),
      "declared sampling unit"
    )
    shiny::testServer(mod_repro_server, args = list(state = state), {
      session$setInputs(draft_go = 1)
      expect_match(output$draft, "Draft not computed", fixed = TRUE)
      expect_match(output$draft, "declared sampling unit", fixed = TRUE)
    })
  })
)
