# The interface and the package surface: frozen claims and their external
# payload, reverse-keyed balance, the wording checks and their reproducible
# code, the words for excluding items, the reference index and publication
# workflow, the public functions, child processes, what confirms
# automatically, and unused code.
#
# Author: Ken Kelley

iface_hs9_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = FALSE, name = fx$name, configure = function(s) {
s$case_index <- seq_len(nrow(fx$data))
  shiny::isolate(sieve_confirm_types(s, origin = "button"))
  })
  s
}

iface_text <- function(html) {
  gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(html), collapse = " ")))
}

# ---- A frozen claim holding a saved specification ----------------------------

test_that("a claim frozen after a CFA serializes, and Prepare builds the external payload", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("lavaan")
  s <- iface_hs9_state()
  plan <- sieve_assistant_plan(s, "Is the three-factor model adequate?", "cfa")
  invisible(sieve_assistant_execute(s, plan))
  claim <- sieve_freeze_claim(s, "Visual correlates with Textual below .8")
  # The claim keeps the saved specification as it was saved.
  expect_gte(length(claim$models), 1L)
  expect_s3_class(claim$models[[1]]$spec, "sieve_spec")
  ids <- vapply(shiny::isolate(s$analyses), function(r) r$id, character(1))
  context <- sieve_assistant_context(s, analysis_ids = ids)
  expect_length(context$claims, 1L)
  # The disclosed copy is plain data, and it serializes the way the dialog
  # serializes it.
  expect_null(oldClass(context$claims[[1]]$models[[1]]$spec))
  json <- as.character(jsonlite::toJSON(context, auto_unbox = TRUE, digits = NA, na = "null", null = "null"))
  expect_match(json, claim$models[[1]]$spec$key, fixed = TRUE)
  expect_match(json, "Visual correlates with Textual below .8", fixed = TRUE)

  # The dialog: every claim is selected by default; Prepare makes the payload.
  shiny::testServer(mod_project_server, args = list(state = s), {
    session$setInputs(assistant = 1)
    session$setInputs(
      assist_goal = "Review the construct definitions", assist_task = "understand",
      assist_provider = "Anthropic", assist_model = "a-model", assist_max_chars = 50000,
      assist_wordings = FALSE, assist_analysis_ids = ids,
      assist_claim_ids = claim$id, assist_decision_ids = character(0)
    )
    session$setInputs(assist_plan = 1)
    session$setInputs(assist_prepare_external = 1)
    shown <- output$assist_payload
    expect_false(identical(shown, "No external payload prepared."))
    expect_match(shown, "Anthropic", fixed = TRUE)
    expect_match(shown, "Visual correlates with Textual below .8", fixed = TRUE)
  })
})

test_that("a failure while the payload is assembled is reported, and nothing is prepared", {
  skip_if_not_installed("jsonlite")
  s <- iface_hs9_state()
  said <- new.env()
  said$messages <- character(0)
  testthat::local_mocked_bindings(
    sieve_assistant_payload_counts = function(payload) stop("assembly failed here", call. = FALSE),
    .package = "SIEVER"
  )
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      said$messages <- c(said$messages, paste(as.character(ui), collapse = " "))
      invisible("id")
    },
    .package = "shiny"
  )
  shiny::testServer(mod_project_server, args = list(state = s), {
    session$setInputs(assistant = 1)
    session$setInputs(
      assist_goal = "Review the construct definitions", assist_task = "understand",
      assist_provider = "Anthropic", assist_model = "a-model", assist_max_chars = 50000,
      assist_wordings = FALSE
    )
    session$setInputs(assist_plan = 1)
    session$setInputs(assist_prepare_external = 1)
    expect_identical(output$assist_payload, "No external payload prepared.")
  })
  expect_true(any(grepl("assembly failed here", said$messages, fixed = TRUE)))
})

# ---- Reverse-keyed balance with an item without a construct ------------------

test_that("an item without a construct is counted in its own group and leaves the others exact", {
  m <- data.frame(
    item = paste0("q", 1:7), label = paste0("Q", 1:7),
    construct = c("A", "A", "A", NA, "", "B", "B"),
    status = c(rep("active", 6), "parked"),
    reverse = c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  out <- sieve_reverse_balance(m)
  expect_identical(out$construct, c("A", "Unassigned", "B"))
  expect_identical(out$k, c(3L, 2L, 1L))
  expect_identical(out$reversed, c(1L, 1L, 0L))
  expect_false(anyNA(out$k))
  # No included item: an empty table, not an error.
  m$status <- "parked"
  expect_identical(nrow(sieve_reverse_balance(m)), 0L)
})

# ---- Wording checks on pasted and non-English text ---------------------------

test_that("typographic apostrophes, 'can not', and letters of any script are read as words", {
  m <- data.frame(
    item = paste0("q", 1:6), label = paste0("Q", 1:6), construct = "A",
    status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
  m$wording <- c(
    "I don\u2019t enjoy meeting new people.",
    "I don't enjoy meeting new people.",
    "I can not relax.",
    "I cannot relax.",
    "Je ne suis pas d\u00e9tendu.",
    "I can\u2019t not worry."
  )
  w <- sieve_wording_checks(m)
  # The pasted contraction is the typed one.
  expect_identical(w$negation[1:2], c(TRUE, TRUE))
  expect_identical(w$found[1], "don't")
  # Both spellings of cannot are one negation, reported the same way.
  expect_identical(w$found[3:4], c("cannot", "cannot"))
  expect_identical(w$double_negative[3:4], c(FALSE, FALSE))
  # A letter outside ASCII does not split its word.
  expect_identical(w$words[5], 5L)
  expect_identical(SIEVER:::sieve_tokens(m$wording[5]), c("je", "ne", "suis", "pas", "d\u00e9tendu"))
  # A pasted double negative is a double negative.
  expect_true(w$double_negative[6])
})

# ---- The wording table and the reproducible code -----------------------------

test_that("the wording checks are logged once per input, as a call that reproduces the table", {
  s <- iface_hs9_state()
  stamps <- function() length(grep("^# \\[", shiny::isolate(s$codelog)))
  # The table and its logged step are on the Content Validity tab's Item
  # Writing Checks sub-tab (mod_content_server), and no Ingest panel holds
  # them.
  expect_false(exists("mod_ingest_prepare_server", envir = asNamespace("SIEVER"), inherits = FALSE))
  shiny::testServer(mod_content_server, args = list(state = s), {
    n0 <- stamps()
    invisible(output$wording_tbl)
    expect_identical(stamps(), n0 + 1L)
    # Another step, then an edit the checks do not read, then the table again.
    sieve_log(s, "Some other step", "x <- 1")
    m <- shiny::isolate(s$meta)
    m$facet[1] <- "Edited facet"
    s$meta <- m
    session$flushReact()
    invisible(output$wording_tbl)
    expect_identical(stamps(), n0 + 2L)
    # An edit the checks read writes the new call.
    m <- shiny::isolate(s$meta)
    m$label[1] <- "Edited name"
    s$meta <- m
    session$flushReact()
    invisible(output$wording_tbl)
    expect_identical(stamps(), n0 + 3L)
  })
  log <- shiny::isolate(s$codelog)
  at <- tail(grep("^# \\[[0-9]+\\] Wording checks", log), 1L)
  end <- at + which(!nzchar(log[(at + 1L):length(log)]))[1L] - 1L
  block <- log[(at + 1L):end]
  expect_true(any(grepl("SIEVER::sieve_wording_checks(wording_meta)", block, fixed = TRUE)))
  # The logged call computes the table the tab shows.
  env <- new.env(parent = asNamespace("SIEVER"))
  eval(parse(text = sub("SIEVER::", "", block, fixed = TRUE)), env)
  expect_identical(env$wording_checks, sieve_wording_checks(shiny::isolate(s$meta)))
})

# ---- One vocabulary for excluding items --------------------------------------

test_that("the item dialog says Exclude and Include, as the navbar link does", {
  st <- iface_hs9_state()
  it <- shiny::isolate(st$meta)$item
  shown <- new.env()
  shown$notes <- character(0)
  testthat::local_mocked_bindings(
    showModal = function(ui, session = NULL) assign("ui", ui, envir = shown),
    # Every notification goes through sieve_notify().
    sieve_notify = function(message, ...) {
      shown$notes <- c(shown$notes, paste(as.character(message), collapse = " "))
      invisible(NULL)
    },
    .package = "SIEVER"
  )
  link <- iface_text(mod_drop_link_ui("drop"))
  expect_match(link, "Exclude / Include items", fixed = TRUE)
  shiny::testServer(mod_drop_server, args = list(state = st), {
    session$setInputs(open = 1L)
    title <- iface_text(shown$ui)
    expect_match(title, "Exclude / Include items", fixed = TRUE)
    session$setInputs(drop_pick = it[4:5], drop_reason = "low item-total r")
    session$setInputs(drop_go = 1L)
    body <- iface_text(output$body$html)
    session$setInputs(restore_pick = it[5])
    session$setInputs(restore_go = 1L)
    session$setInputs(drop_pick = character(0))
    session$setInputs(drop_go = 2L)
    for (word in c("Items to exclude", "Exclude the selected items", "Excluded items (2)",
      "Items to include again", "Include the selected items")) {
      expect_match(body, word, fixed = TRUE)
    }
    expect_false(grepl("\\b(Drop|drop|Dropped|dropped|Restore|restore)\\b", paste(title, body), perl = TRUE))
  })
  expect_match(shown$notes[1], "Excluded 2 items", fixed = TRUE)
  expect_match(shown$notes[2], "Included 1 item again", fixed = TRUE)
  expect_match(shown$notes[3], "Pick at least one item to exclude.", fixed = TRUE)
  expect_false(any(grepl("\\b(Drop|Dropped|Restored)\\b", shown$notes, perl = TRUE)))
})

# ---- The reference index and the publication workflow ------------------------

test_that("every exported topic is indexed, with bundled docs and a configured publication workflow", {
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  skip_if_not(file.exists(file.path(root, "_pkgdown.yml")), "_pkgdown.yml is not in this tree")
  yml <- readLines(file.path(root, "_pkgdown.yml"), warn = FALSE)
  listed <- trimws(sub("^\\s*-\\s*", "", grep("^\\s+-\\s+[A-Za-z]", yml, value = TRUE)))
  ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
  expect_true("sieve_efa_factor_plot" %in% listed)
  # Shared help pages are indexed once, under their own topic.
  shared <- c(
    sieve_project_decisions = "sieve_project_analyses", sieve_project_report = "sieve_project_analyses",
    sieve_default_rmsea_conf_level = "sieve_default_conf_level"
  )
  topics <- ifelse(exports %in% names(shared), shared[exports], exports)
  expect_identical(setdiff(topics, listed), character(0))
  # An indexed topic is an exported one (or a shared page, the package page,
  # or an article).
  topic_names <- sub("[.]Rd$", "", list.files(file.path(root, "man"), pattern = "[.]Rd$"))
  expect_identical(setdiff(intersect(listed, topic_names), c(exports, "SIEVER-package", "print.siever_analysis",
    "print.sieve_diagram", "plot.sieve_diagram", "print.sieve_project")), character(0))
  # Active metadata names the source repository and README directs readers
  # to the bundled vignettes while the hosted documentation is unavailable.
  # The configured workflow remains ready to build on a push to the main
  # branch, on a pull request (without deploying), on a published release
  # and on request; its presence does not establish a published site.
  # A project site of GitHub Pages is served at the owner's login in lower
  # case, so the address is derived from the repository's owner, not typed
  # (a typed address can misspell the login, giving an address no one
  # serves).
  bugs <- unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "BugReports")[1L, 1L])
  owner <- sub("^https://github[.]com/([^/]+)/SIEVER/issues$", "\\1", bugs)
  expect_identical(owner, "yelleKneK")
  site <- sprintf("https://%s.github.io/SIEVER/", tolower(owner))
  repository <- sprintf("https://github.com/%s/SIEVER", owner)
  expect_identical(site, "https://yelleknek.github.io/SIEVER/")
  expect_identical(grep("^url:", yml, value = TRUE), paste("url:", site))
  expect_identical(unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "URL")[1L, 1L]), repository)
  # The README says the package from CRAN carries the articles (another
  # distribution may be named beside it, and a line may break anywhere), and
  # every article it opens by name is a registered vignette. The pattern
  # allows variants of the sentence rather than one literal wording.
  readme <- paste(readLines(file.path(root, "README.md"), warn = FALSE), collapse = "\n")
  readme_flat <- gsub("\\s+", " ", readme)
  expect_match(readme_flat, 'vignette("user-guide", package = "SIEVER")', fixed = TRUE)
  expect_match(readme_flat, "package from CRAN(?: and (?:(?!\\bnot\\b)[^.])+?)? includes? these articles", perl = TRUE)
  expect_match(readme_flat, 'install.packages("SIEVER")', fixed = TRUE)
  opened <- regmatches(readme_flat, gregexpr('vignette\\("[^"]+", package = "SIEVER"\\)', readme_flat))[[1]]
  opened <- unique(sub('^vignette\\("([^"]+)".*$', "\\1", opened))
  # The Standards crosswalk is opened by name as well.
  expect_true(all(c("user-guide", "getting-started", "standards-crosswalk") %in% opened))
  for (name in opened) {
    rmd <- file.path(root, "vignettes", paste0(name, ".Rmd"))
    expect_true(file.exists(rmd), info = rmd)
    header <- if (file.exists(rmd)) readLines(rmd, n = 40L, warn = FALSE) else character(0)
    expect_true(any(grepl("%\\\\VignetteIndexEntry\\{", header)), info = rmd)
  }
  # Every public surface that names a site of the owner's (SIEVER's or
  # DMAR's), with either scheme, names it on the owner's host.
  public <- c(
    file.path(root, c("DESCRIPTION", "README.md", "NEWS.md", "CITATION.cff", "_pkgdown.yml")),
    list.files(file.path(root, "man"), pattern = "[.]Rd$", full.names = TRUE),
    list.files(file.path(root, "vignettes"), pattern = "[.]Rmd$", full.names = TRUE),
    list.files(file.path(root, ".github", "workflows"), pattern = "[.]ya?ml$", full.names = TRUE)
  )
  public <- public[file.exists(public)]
  hosts <- unlist(lapply(public, function(f) {
    text <- readLines(f, warn = FALSE)
    found <- unlist(regmatches(text, gregexpr("https?://[A-Za-z0-9-]+[.]github[.]io/(SIEVER|DMAR)\\b", text)))
    sub("^https?://([^.]+)[.].*$", "\\1", found)
  }))
  expect_true(length(hosts) > 0L)
  expect_identical(unique(hosts), tolower(owner))
  cff <- readLines(file.path(root, "CITATION.cff"), warn = FALSE)
  expect_identical(grep("^url:", cff, value = TRUE), sprintf("url: \"%s\"", repository))
  wf <- file.path(root, ".github", "workflows", "pkgdown.yaml")
  skip_if_not(file.exists(wf), "the workflow is not in this tree")
  lines <- readLines(wf, warn = FALSE)
  on_block <- lines[seq(grep("^on:", lines), grep("^permissions:", lines) - 1L)]
  expect_identical(trimws(on_block[-1]), c(
    "push:", "branches: [main, master]", "pull_request:", "branches: [main, master]",
    "release:", "types: [published]", "workflow_dispatch:"
  ))
  # A pull request builds the site but never deploys it.
  expect_true(any(grepl("if: github.event_name != 'pull_request'", lines, fixed = TRUE)))
})

test_that("every release archive the README and the articles link is the archive of this version", {
  # The README and the user guide pin the URL of the archive attached to
  # the GitHub release, so a later version must not ship a link to an
  # earlier one.
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  version <- unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[1L, 1L])
  files <- c(file.path(root, "README.md"), list.files(file.path(root, "vignettes"), pattern = "[.]Rmd$", full.names = TRUE))
  files <- files[file.exists(files)]
  found <- unlist(lapply(files, function(f) {
    text <- readLines(f, warn = FALSE)
    hits <- unlist(regmatches(text, gregexpr("releases/download/[^[:space:]\"'/]+/[^[:space:]\"'/]+[.]tar[.]gz", text)))
    if (length(hits)) paste0(basename(f), ": ", hits) else character(0)
  }))
  # The README and the user guide each link the archive.
  expect_true(any(startsWith(found, "README.md: ")))
  expect_true(any(startsWith(found, "user-guide.Rmd: ")))
  expected <- sprintf("releases/download/v%s/SIEVER_%s.tar.gz", version, version)
  expect_identical(found[!endsWith(found, paste0(": ", expected))], character(0))
})

# ---- The public functions ----------------------------------------------------

test_that("no export needs the session's private state, and every export has an example", {
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
  methods <- sub("^S3method\\(([^,]+),([^)]+)\\)$", "\\1.\\2", grep("^S3method\\(", ns, value = TRUE))
  # The shared state store is not exported, so no export may require it.
  # sieve_session_write() also takes a checked project snapshot, which
  # sieve_project_read() returns.
  first <- vapply(exports, function(f) names(formals(get(f, envir = asNamespace("SIEVER"))))[1] %||% "", character(1))
  expect_identical(setdiff(exports[first == "state"], "sieve_session_write"), character(0))
  unexported <- c(
    "sieve_assistant_context", "sieve_assistant_disclosure_report", "sieve_assistant_execute",
    "sieve_assistant_plan", "sieve_claim_write", "sieve_claims_check", "sieve_evaluate_claim",
    "sieve_freeze_claim", "sieve_record_construct"
  )
  expect_identical(intersect(exports, unexported), character(0))
  for (f in unexported) expect_true(exists(f, envir = asNamespace("SIEVER"), inherits = FALSE))
  # Every exported function and method has a help page with a runnable example.
  rd <- lapply(list.files(file.path(root, "man"), pattern = "[.]Rd$", full.names = TRUE), tools::parse_Rd)
  tag <- function(x) vapply(x, function(node) attr(node, "Rd_tag") %||% "", character(1))
  aliases <- lapply(rd, function(x) unlist(lapply(x[tag(x) == "\\alias"], as.character)))
  has_examples <- vapply(rd, function(x) "\\examples" %in% tag(x), logical(1))
  for (f in c(exports, methods)) {
    at <- which(vapply(aliases, function(a) f %in% a, logical(1)))
    expect_length(at, 1L)
    expect_true(isTRUE(has_examples[at[1]]), info = f)
  }
  # And none is wrapped in \donttest.
  ex <- unlist(lapply(rd, function(x) paste(as.character(x[tag(x) == "\\examples"]), collapse = "")))
  expect_false(any(grepl("donttest", ex, fixed = TRUE)))
})

# ---- Child processes run the source under test -------------------------------

test_that("a child process started for a replay loads the source under test", {
  skip_on_cran()
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  libs <- compat_source_libs()
  out <- compat_fresh_r(
    c(
      "cat('VERSION=', as.character(utils::packageVersion('SIEVER')), '\\n', sep = '')",
      "cat('PATH=', normalizePath(find.package('SIEVER')), '\\n', sep = '')",
      "cat('HAS=', exists('sieve_json_plain', envir = asNamespace('SIEVER')), '\\n', sep = '')"
    ),
    libs = libs
  )
  source_version <- unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[1, 1])
  expect_true(paste0("VERSION=", source_version) %in% out, info = paste(out, collapse = "\n"))
  expect_true(paste0("PATH=", normalizePath(file.path(libs[1], "SIEVER"))) %in% out)
  # A function that exists only in this source tree is there.
  expect_true("HAS=TRUE" %in% out)
  # The copy is installed once per R session and reused.
  expect_identical(compat_source_libs(), libs)
})

test_that("the installed copy of a source package keeps its datasets", {
  skip_on_cran()
  # A dependency installed beside the source under test (DMAR, when it is
  # loaded from a source tree) must keep data/: the bundled example reads
  # DMAR's Holzinger and Swineford data, and an app process given a copy
  # without them cannot load the example.
  root <- withr::local_tempdir()
  pkg <- file.path(root, "sievercopycheck")
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  dir.create(file.path(pkg, "data"))
  writeLines(c(
    "Package: sievercopycheck", "Version: 0.0.1", "Title: A Copy Check",
    "Description: Checks that an installed copy keeps its datasets.",
    "Author: SIEVER tests", "Maintainer: SIEVER tests <tests@example.org>",
    "License: GPL-3", "Encoding: UTF-8", "LazyData: true"
  ), file.path(pkg, "DESCRIPTION"))
  writeLines("export(copy_check_one)", file.path(pkg, "NAMESPACE"))
  writeLines("copy_check_one <- function() 1L", file.path(pkg, "R", "one.R"))
  copy_check_rows <- data.frame(a = 1:3)
  save(copy_check_rows, file = file.path(pkg, "data", "copy_check_rows.rda"))
  lib <- compat_install_copy(pkg, env = environment(), byte_compile = FALSE)
  expect_true(dir.exists(file.path(lib, "sievercopycheck", "data")))
  # Read through `::`, which the fixture's LazyData: true makes possible, so
  # the fresh process proves the installed data/ copy and nothing else.
  out <- compat_fresh_r(
    "cat('ROWS=', nrow(sievercopycheck::copy_check_rows), '\\n', sep = '')",
    libs = lib
  )
  expect_true("ROWS=3" %in% out, info = paste(out, collapse = "\n"))
})

# ---- What confirms automatically ---------------------------------------------

test_that("the help pages say only a bundled example confirms on load", {
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  # Only a bundled example whose items are all continuous is confirmed on
  # load; a file of the researcher's own waits for Confirm the study.
  for (topic in c("SIEVER-package.Rd", "run_app.Rd")) {
    rd <- gsub("[[:space:]]+", " ", paste(readLines(file.path(root, "man", topic), warn = FALSE), collapse = " "))
    expect_false(grepl("Continuous items confirm automatically", rd, fixed = TRUE), info = topic)
    expect_match(rd, "Confirm the study", fixed = TRUE, info = topic)
  }
  src <- paste(readLines(file.path(root, "R", "utils_state.R"), warn = FALSE), collapse = " ")
  expect_false(grepl("most users never see the second message", src, fixed = TRUE))
})

# ---- Unused code -------------------------------------------------------------

test_that("the namespace holds no unused navbar divider or placeholder panel", {
  expect_false(exists("sieve_nav_divider", envir = asNamespace("SIEVER"), inherits = FALSE))
  expect_false(exists("sieve_placeholder_panel", envir = asNamespace("SIEVER"), inherits = FALSE))
  # The navbar builds and marks the optional tabs.
  ui <- as.character(app_ui(NULL))
  expect_match(ui, "sieve-nav-optional", fixed = TRUE)
})
