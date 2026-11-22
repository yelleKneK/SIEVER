# test-rd_hygiene.R
#
# Documentation and run-time hygiene that R CMD check reports only with the
# manuals built, or not at all: Rd markup inside \eqn{} (the PDF manual stops
# on it), \donttest{} and \dontrun{}, commented-out code in examples, and
# functions that write to the global environment, set options(warn = ), or
# carry a default file path. CRAN's pretest or its reviewers return a
# package for each of these even when its check is clean.
#
# The detectors inspect executable expressions rather than string literals,
# and the default-path check is restricted to actual file writers; positive
# controls below check both distinctions. It finds the package under test
# by itself and it runs on CRAN as well as locally. Each detector proves
# itself on a known-bad snippet first, so a detector that cannot fire never
# passes vacuously.

package_under_test <- function() {
  pkg <- tryCatch(testthat::testing_package(), error = function(e) "")
  if (!nzchar(pkg)) {
    desc <- file.path("..", "..", "DESCRIPTION")
    if (file.exists(desc)) pkg <- unname(read.dcf(desc, "Package")[1L])
  }
  pkg
}

rd_sources <- function() {
  man <- file.path("..", "..", "man")
  if (dir.exists(man)) tools::Rd_db(dir = file.path("..", "..")) else tools::Rd_db(package_under_test())
}

math_text <- function(rd) {
  out <- character()
  walk <- function(x) {
    tag <- attr(x, "Rd_tag")
    if (!is.null(tag) && tag %in% c("\\eqn", "\\deqn"))
      out <<- c(out, paste(unlist(x), collapse = ""))
    if (is.list(x)) for (el in x) walk(el)
  }
  walk(rd)
  out
}

rd_tags <- function(rd) {
  tags <- character()
  walk <- function(x) {
    tag <- attr(x, "Rd_tag")
    if (!is.null(tag)) tags <<- c(tags, tag)
    if (is.list(x)) for (el in x) walk(el)
  }
  walk(rd)
  unique(tags)
}

examples_text <- function(rd) {
  ex <- Filter(function(x) identical(attr(x, "Rd_tag"), "\\examples"), rd)
  if (!length(ex)) return("")
  paste(unlist(ex), collapse = "")
}

markup_in_math <- "\\\\(code|emph|link|strong|pkg|bold|verb|var|samp|file|env|dQuote|sQuote|href|url)\\{"

test_that("the math-markup detector flags Rd markup inside \\eqn{} and passes plain math", {
  bad <- tools::parse_Rd(textConnection(paste0(
    "\\name{x}\\alias{x}\\title{x}\\description{The region is ",
    "\\eqn{(-\\code{delta_lower}, \\code{delta_upper})}.}"
  )))
  expect_true(any(grepl(markup_in_math, math_text(bad))))
  good <- tools::parse_Rd(textConnection(paste0(
    "\\name{x}\\alias{x}\\title{x}\\description{The region is ",
    "\\eqn{(-\\delta_L, +\\delta_U)}, with \\code{delta_lower} as ",
    "\\eqn{\\delta_L}.}"
  )))
  expect_false(any(grepl(markup_in_math, math_text(good))))
})

test_that("no help page carries Rd markup inside \\eqn{} or \\deqn{}", {
  db <- rd_sources()
  skip_if(length(db) == 0L, "no Rd sources available")
  offenders <- names(Filter(function(rd) any(grepl(markup_in_math, math_text(rd))), db))
  expect_identical(offenders, character(0))
})

test_that("no example uses \\donttest{} or \\dontrun{}, and only the listed examples wait for an interactive session", {
  db <- rd_sources()
  skip_if(length(db) == 0L, "no Rd sources available")
  dontrun <- names(Filter(function(rd) "\\dontrun" %in% rd_tags(rd), db))
  expect_identical(dontrun, character(0))
  # No example is in \donttest{}, as none of DMAR's is. R CMD check --as-cran
  # runs every example a second time when one page uses it (58 of the 269
  # seconds of a local check, beside 31 for the first pass), so every
  # example has inputs that run in about a second and runs once.
  donttest <- names(Filter(function(rd) "\\donttest" %in% rd_tags(rd), db))
  expect_identical(donttest, character(0))
  # Five calls no input makes quick, because the function holds a minimum:
  # 100 bootstrap replications (DMAR's minimum for an 80 percent interval of
  # the bifactor indices and for a Bollen-Stine p-value), twenty pilot
  # imputations, and two anchor sets with an invariance ladder each, which
  # the ordinal impact example needs as well. Measured at 1.3 to 3.8 seconds
  # each. Their examples run the setup everywhere and the call itself in an
  # interactive session, as the example of run_app() does. A page joins this list on a measured time and a minimum
  # the function states; a quick example runs everywhere.
  gated <- names(Filter(function(rd) grepl("interactive()", examples_text(rd), fixed = TRUE), db))
  allowed <- c(
    "run_app.Rd", "sieve_anchor_sensitivity.Rd", "sieve_bifactor_bootstrap.Rd",
    "sieve_bollen_stine.Rd", "sieve_mi_model.Rd", "sieve_ordinal_fit_impact.Rd"
  )
  expect_identical(setdiff(gated, allowed), character(0))
  # The bifactor bootstrap example shows a request below DMAR's minimum
  # refused, then requests the minimum for the level it states (100
  # replications, 80 percent) and says so. That request is run by the live
  # test in test-dmar-results.R, beside the 400 replication one
  # for a 95 percent interval.
  bifactor <- "sieve_bifactor_bootstrap.Rd"
  if (bifactor %in% names(db)) {
    example <- examples_text(db[[bifactor]])
    expect_match(example, "sieve_bifactor_bootstrap(", fixed = TRUE)
    expect_true(grepl("\\bB\\s*=\\s*100L?\\b", example))
    expect_true(grepl("\\bB\\s*=\\s*50L?\\b", example))
    expect_true(grepl("\\bconf_level\\s*=\\s*0?\\.80?\\b", example))
    expect_true(grepl("\\bseed\\s*=\\s*1L?\\b", example))
  }
})

test_that("no example block gates on requireNamespace()", {
  db <- rd_sources()
  skip_if(length(db) == 0L, "no Rd sources available")
  offenders <- names(Filter(function(rd) grepl("requireNamespace", examples_text(rd), fixed = TRUE), db))
  expect_identical(offenders, character(0))
})

# Comment lines inside \examples whose text is R code, which CRAN does not
# accept in an example.
commented_code <- function(rd) {
  ex <- examples_text(rd)
  if (!nzchar(ex)) return(character())
  lines <- strsplit(ex, "\n", fixed = TRUE)[[1]]
  cm <- grep("^\\s*#", lines)
  if (!length(cm)) return(character())
  is_code <- function(txt) {
    parsed <- tryCatch(parse(text = txt, keep.source = FALSE), error = function(e) NULL)
    if (is.null(parsed) || !length(parsed)) return(FALSE)
    is_call <- vapply(parsed, is.call, logical(1))
    is_formula <- vapply(parsed, function(e)
      is.call(e) && identical(e[[1L]], as.name("~")), logical(1))
    any(is_call & !is_formula)
  }
  hits <- integer()
  for (run in split(cm, cumsum(c(1L, diff(cm) != 1L)))) {
    txt <- sub("^\\s*#+\\s?", "", lines[run])
    n <- length(run)
    for (a in seq_len(n)) for (b in a:n) {
      window <- paste(txt[a:b], collapse = "\n")
      if (nzchar(trimws(window)) && is_code(window)) hits <- c(hits, run[a:b])
    }
  }
  lines[sort(unique(hits))]
}

test_that("the commented-code detector separates code from prose", {
  rd <- tools::parse_Rd(textConnection(paste0(
    "\\name{x}\\alias{x}\\title{x}\\examples{\n",
    "# The fit is shown for comparison.\n",
    "# fit_model(X, Y, M, outcome = \"continuous\")\n",
    "# start <- Sys.time()\n",
    "# Sys.sleep(0.2)\n",
    "default_grid(300, 500)\n}"
  )))
  hits <- commented_code(rd)
  expect_length(hits, 3L)
  expect_false(any(grepl("shown for comparison", hits)))
})

test_that("no help page carries commented-out code in its examples", {
  db <- rd_sources()
  skip_if(length(db) == 0L, "no Rd sources available")
  offenders <- Filter(length, lapply(db, commented_code))
  expect_identical(names(offenders), character(0),
    info = paste(unlist(lapply(names(offenders), function(f)
      paste(f, offenders[[f]], sep = ": "))), collapse = "\n")
  )
})

# Run-time policies CRAN applies: no writing to the global
# environment (any assign() or rm() aimed at .GlobalEnv, `.Random.seed`, and
# any <<-), no options(warn = -1), and no default path in a function that
# writes a file. The namespace is deparsed and inspected.
namespace_functions <- function() {
  ns <- asNamespace(package_under_test())
  nms <- ls(ns, all.names = TRUE)
  fns <- Filter(is.function, mget(nms, envir = ns, inherits = FALSE))
  fns[!vapply(fns, is.primitive, logical(1))]
}

# Keep syntax while excluding quoted source text and diagnostic labels.
hygiene_code <- function(x) {
  if (is.character(x)) return("")
  if (is.call(x) || is.expression(x) || is.pairlist(x)) {
    return(paste(vapply(as.list(x), hygiene_code, character(1)), collapse = " "))
  }
  if (is.symbol(x)) as.character(x) else ""
}
hygiene_global <- function(f) {
  code <- hygiene_code(body(f))
  grepl("\\.GlobalEnv|globalenv|\\.Random\\.seed|<<-", code) ||
    grepl("options[ (]+warn", code)
}
hygiene_writes <- function(f) {
  code <- hygiene_code(body(f))
  grepl("(^| )(write[A-Za-z.]*|saveRDS|save|dir.create|file.copy|file.rename)( |$)", code)
}

test_that("global-state and file-writer detectors reject executable violations", {
  expect_true(hygiene_global(function() assign("x", 1, envir = .GlobalEnv)))
  expect_true(hygiene_global(function() {
    x <<- 1
  }))
  expect_false(hygiene_global(function() c("<<-", ".GlobalEnv")))
  expect_true(hygiene_writes(function(file = "fixed.txt") writeLines("text", file)))
  expect_false(hygiene_writes(function(path = "snapshot") paste(path, "diagnostic")))
  expect_false(hygiene_writes(function(file = "config.yml") readLines(file)))
})

test_that("no function touches the global environment, uses <<-, or sets options(warn)", {
  fns <- namespace_functions()
  offenders <- names(fns)[vapply(fns, hygiene_global, logical(1))]
  expect_identical(offenders, character(0))
})

test_that("no function that writes a file has a default path", {
  fns <- namespace_functions()
  fns <- fns[vapply(fns, hygiene_writes, logical(1))]
  bad <- character()
  for (nm in names(fns)) {
    fm <- formals(fns[[nm]])
    for (arg in intersect(names(fm), c("file", "filename", "path", "dir", "directory"))) {
      if (!is.null(fm[[arg]]) && !identical(fm[[arg]], quote(expr = )))
        bad <- c(bad, sprintf("%s(%s = %s)", nm, arg, deparse(fm[[arg]])))
    }
  }
  expect_identical(bad, character(0))
})

# ---- what the help pages name and cite -----------------------------------------
#
# R CMD check does not look inside \code{}, so a help page could name a
# function that does not exist, state a rule only as the name of an internal
# helper the reader cannot call or look up (B "at least
# sieve_bootstrap_min_draws(conf_level)", an estimator "one of
# sieve_estimator_choices()"), or list a reference its text never cites.

# The text of a help page's sections, its markup dropped and its white space
# collapsed, without the sections named in `drop`.
rd_page_text <- function(rd, drop = character()) {
  keep <- Filter(function(x) !(attr(x, "Rd_tag") %in% drop), rd)
  gsub("[[:space:]]+", " ", paste(unlist(keep), collapse = ""))
}

# The description of one argument of a help page, or "".
rd_argument_text <- function(rd, arg) {
  out <- character()
  for (section in Filter(function(x) identical(attr(x, "Rd_tag"), "\\arguments"), rd)) {
    for (item in section) {
      if (identical(attr(item, "Rd_tag"), "\\item") && length(item) == 2L &&
        identical(trimws(paste(unlist(item[[1L]]), collapse = "")), arg)) {
        out <- c(out, paste(unlist(item[[2L]]), collapse = ""))
      }
    }
  }
  gsub("[[:space:]]+", " ", paste(out, collapse = " "))
}

test_that("every function a help page names exists, and no help page leaves a rule to an internal helper's name", {
  db <- rd_sources()
  skip_if(length(db) == 0L, "no Rd sources available")
  ns <- asNamespace(package_under_test())
  named <- lapply(db, function(rd) {
    text <- rd_page_text(rd)
    unique(regmatches(text, gregexpr("\\bsieve_[A-Za-z0-9_.]+(?=\\()", text, perl = TRUE))[[1L]])
  })
  expect_true(length(unlist(named)) > 100L)
  gone <- unlist(lapply(names(named), function(page) {
    absent <- named[[page]][!vapply(named[[page]], exists, logical(1), envir = ns, inherits = FALSE)]
    if (length(absent)) paste0(page, ": ", absent) else character(0)
  }))
  expect_identical(gone, character(0))
  # The internal helpers whose rule a help page states in words.
  internal <- c(
    "sieve_bootstrap_min_draws", "sieve_estimator_choices", "sieve_build_meta", "sieve_item_keying",
    "sieve_item_omega_note", "sieve_reliability_keying", "sieve_reliability_coding", "sieve_cores",
    "sieve_with_seed", "sieve_cfa_syntax", "sieve_fit_sem", "sieve_fit_path", "sieve_fit_mediation"
  )
  expect_identical(intersect(internal, getNamespaceExports(ns)), character(0))
  pointers <- unlist(lapply(names(named), function(page) {
    hit <- intersect(named[[page]], internal)
    if (length(hit)) paste0(page, ": ", hit) else character(0)
  }))
  expect_identical(pointers, character(0))
  # Every page with an estimator argument lists the values the argument
  # takes, which are those of the Estimator control.
  choices <- unname(get("sieve_estimator_choices", envir = ns)())
  estimator_pages <- Filter(function(rd) grepl("auto", rd_argument_text(rd, "estimator"), fixed = TRUE), db)
  expect_true(length(estimator_pages) >= 10L)
  for (page in names(estimator_pages)) {
    text <- rd_argument_text(estimator_pages[[page]], "estimator")
    for (value in choices) expect_match(text, sprintf("\"%s\"", value), fixed = TRUE, info = paste(page, value))
  }
  # The fewest draws a percentile interval needs is given as its rule and
  # its numbers, which are the helper's.
  min_draws <- get("sieve_bootstrap_min_draws", envir = ns)
  expect_identical(c(min_draws(0.95), min_draws(0.90)), c(39L, 19L))
  bands <- rd_argument_text(db[["sieve_irt_information_bands.Rd"]], "B")
  expect_match(bands, "ceiling(2 / (1 - conf_level)) - 1 (39 at 0.95, 19 at 0.90)", fixed = TRUE)
  impact <- rd_page_text(db[["sieve_ordinal_fit_impact.Rd"]])
  expect_match(impact, "at least ceiling(2 / (1 - conf_level)) - 1 draws are usable (39 at 0.95)", fixed = TRUE)
})

test_that("every work a help page lists in its references is cited in its text", {
  db <- rd_sources()
  skip_if(length(db) == 0L, "no Rd sources available")
  checked <- 0L
  uncited <- character()
  for (page in names(db)) {
    rd <- db[[page]]
    refs <- Filter(function(x) identical(attr(x, "Rd_tag"), "\\references"), rd)
    if (!length(refs)) next
    body <- rd_page_text(rd, drop = c("\\references", "\\examples", "\\alias", "\\name", "\\keyword", "\\concept", "\\docType", "\\encoding"))
    entries <- unlist(lapply(refs, function(r) strsplit(paste(unlist(r), collapse = ""), "\n[[:space:]]*\n")[[1L]]))
    entries <- trimws(gsub("[[:space:]]+", " ", entries))
    for (entry in entries[nzchar(entries)]) {
      author <- sub("^([^,(]+?)(,| \\().*$", "\\1", entry)
      if (startsWith(author, "American Educational Research Association")) author <- "AERA"
      year <- gsub("[()]", "", regmatches(entry, regexpr("\\((19|20)[0-9]{2}[a-z]?\\)", entry)))
      expect_length(year, 1L)
      checked <- checked + 1L
      if (!(grepl(author, body, fixed = TRUE) && grepl(year, body, fixed = TRUE))) {
        uncited <- c(uncited, sprintf("%s: %s (%s)", page, author, year))
      }
    }
  }
  expect_true(checked >= 20L)
  expect_identical(uncited, character(0))
  # The two invariance pages cite the works they list where they apply.
  anchor <- rd_page_text(db[["sieve_anchor_sensitivity.Rd"]], drop = "\\references")
  expect_match(anchor, "the cumulative order of Meredith (1993)", fixed = TRUE)
  expect_match(anchor, "the order Wu and Estabrook (2016) recommend", fixed = TRUE)
  interpret <- rd_page_text(db[["sieve_invariance_interpret.Rd"]], drop = "\\references")
  expect_match(interpret, "the strict invariance of Meredith (1993)", fixed = TRUE)
})
