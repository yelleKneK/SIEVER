# The five workspaces (fct_workspaces.R). Every page is kept under its own
# name and value in exactly one of five workspaces, the page on screen is
# the selected page of the selected workspace, every reader of that page
# (the automatic fit rule, the page guide, the confirm nudge) and every jump
# to a page (sieve_go()) follow it, a page value given directly also
# resolves, and the study goal changes only the suggested route.

skip_if_not_installed("shiny")

workspaces_html <- function() paste(as.character(app_ui(list())), collapse = "")
workspaces_values <- function(html) {
  values <- regmatches(html, gregexpr('data-value="[a-z]+"', html))[[1]]
  sub('data-value="([a-z]+)"', "\\1", values)
}
workspaces_count <- function(html, text) {
  sum(lengths(regmatches(html, gregexpr(text, html, fixed = TRUE))))
}
# The seventeen destinations of the tab bar, in its order.
workspaces_tab_values <- c(
  "ingest", "screen", "content", "retention", "efa", "winnow", "cfa", "reliab",
  "validity", "finalize", "optimize", "bifactor", "network", "irt", "sem", "repro"
)

# ---- (a) every page in exactly one workspace ---------------------------------

test_that("every page sits in exactly one workspace and no page is lost", {
  ws <- sieve_workspaces()
  expect_identical(names(ws), c("study", "structure", "scores", "evidence", "report"))
  pages <- sieve_workspace_pages()
  expect_false(anyDuplicated(pages) > 0)
  # Every page the guide knows (and nothing else) has a place: the sixteen
  # destinations of the tab bar and the Scoring and Norms page.
  expect_setequal(pages, setdiff(names(sieve_page_catalog()), "scoring"))
  expect_setequal(setdiff(pages, "scoring"), workspaces_tab_values)
  for (p in pages) {
    expect_true(p %in% ws[[sieve_page_workspace(p)]]$pages, info = p)
  }
  # One workspace each, by name; a name that is not a page has none.
  expect_identical(sieve_page_workspace("scoring"), "scores")
  expect_identical(sieve_page_workspace("cfa"), "structure")
  expect_true(is.na(sieve_page_workspace("not_a_page")))
  # Each workspace states a question.
  for (w in ws) expect_true(nzchar(w$question))
})

# ---- (b) the page on screen ---------------------------------------------------

test_that("workspace navigation also accepts a page value given directly", {
  expect_identical(sieve_active_page(list()), "ingest")
  expect_identical(sieve_active_page(list(nav = "structure")), "retention")
  expect_identical(sieve_active_page(list(nav = "structure", ws_structure = "cfa")), "cfa")
  expect_identical(sieve_active_page(list(nav = "scores", ws_scores = "scoring")), "scoring")
  expect_identical(sieve_active_page(list(nav = "unknown")), "ingest")
  expect_identical(sieve_active_page(list(nav = "cfa")), "cfa")
  expect_identical(sieve_page_navset("optimize"), "ws_structure")
  expect_identical(sieve_page_navset("not_a_page"), "nav")
  expect_identical(sieve_page_path_text("repro"), "Report → Reproducible Code")
  expect_identical(sieve_page_path_text("cfa"), "Structure → CFA")
  # run_app() has no workspaces argument; one given stops the call with the
  # reason (the run_app() refusal test below).
  expect_false("workspaces" %in% names(formals(run_app)))
  expect_identical(sieve_active_page(list(nav = "structure", ws_structure = "cfa")), "cfa")
})

# ---- (c) the interface --------------------------------------------------------

test_that("five workspaces hold every page under its own value, and the Scoring and Norms card sits under Scores", {
  html <- workspaces_html()
  ws <- sieve_workspaces()
  for (id in names(ws)) {
    expect_match(html, sprintf('data-value="%s"', id), fixed = TRUE)
    expect_match(html, sprintf('id="ws_%s"', id), fixed = TRUE)
    expect_match(html, ws[[id]]$question, fixed = TRUE)
  }
  # The pages, by their values, in document order: each appears twice (its
  # link in its workspace and its pane), workspace by workspace.
  pages <- sieve_workspace_pages()
  values <- workspaces_values(html)
  expect_identical(unique(values[values %in% pages]), pages)
  expect_identical(
    as.vector(table(factor(values[values %in% pages], levels = pages))),
    rep(2L, length(pages))
  )
  expect_false(grepl('data-value="More"', html, fixed = TRUE))
  # The optional pages keep the lighter label in their workspaces.
  for (l in sieve_tab_paths()$optional) {
    expect_match(html, paste0('<span class="sieve-nav-optional" title="Optional tab: nothing in another tab depends on it.">', l, "</span>"), fixed = TRUE, info = l)
  }
  # The Scoring and Norms card: once, as the page of Scores, with the module
  # ids of CV and Invariance (finalize-), and not a panel of that page.
  expect_match(html, 'data-value="scoring"', fixed = TRUE)
  expect_identical(workspaces_count(html, 'id="finalize-norms_tbl"'), 1L)
  expect_identical(workspaces_count(html, 'id="finalize-norm_freeze"'), 1L)
  finalize <- paste(as.character(mod_finalize_ui("finalize")), collapse = "")
  expect_false(grepl("finalize-norms_tbl", finalize, fixed = TRUE))
  scoring <- paste(as.character(mod_finalize_scoring_ui("finalize")), collapse = "")
  expect_match(scoring, 'id="finalize-norms_tbl"', fixed = TRUE)
  expect_match(scoring, 'id="finalize-scoring_code"', fixed = TRUE)
  # The study goal on Study, and nowhere else.
  expect_identical(workspaces_count(html, 'id="study_goal"'), 1L)
  goals <- sieve_study_goals()
  expect_identical(names(goals$routes), unname(goals$choices))
  for (choice in goals$choices) {
    expect_match(html, sprintf('<option value="%s"', choice), fixed = TRUE)
  }
})

test_that("the About dialog describes the workspace layout", {
  note <- gsub("\\s+", " ", paste(as.character(sieve_workspaces_note()), collapse = ""))
  expect_match(note, "The pages are grouped into five workspaces", fixed = TRUE)
  expect_match(note, "Study (Ingest, Content Validity, and Screening)", fixed = TRUE)
  expect_match(note, "Report (Reproducible Code)", fixed = TRUE)
  expect_match(note, "directly from the Scores heading", fixed = TRUE)
  view <- gsub("\\s+", " ", paste(as.character(sieve_code_view(NULL)), collapse = ""))
  expect_match(view, "Report → Reproducible Code", fixed = TRUE)
})

# ---- (d) the automatic fit rule reads the page of the workspace --------------

test_that("the automatic CFA runs on the CFA page of the Structure workspace and not on another page", {
  skip_if_not_installed("DMAR")
  skip_if_not_installed("lavaan")
  seen <- new.env(parent = emptyenv())
  seen$cfa <- 0L
  seen$validity <- 0L
  testthat::local_mocked_bindings(sieve_demand_note = function(...) invisible(NULL), .package = "SIEVER")
  shiny::testServer(function(input, output, session) {
    state <- sieve_new_state()
    state$confirmed <- TRUE
    wire <- function(module) {
      scoped <- session$makeScope(module)
      sieve_demand_wire(scoped$input, scoped, state, "fit_go", "A small CFA",
        key = shiny::reactive(list(choices = list(module = module))),
        plan = shiny::reactive(list(seconds = .1, p = 3L, estimator = "MLR")),
        compute = function(...) {
          seen[[module]] <- seen[[module]] + 1L
          list(value = module)
        }, kind = "cfa"
      )
    }
    primary <- wire("cfa")
    validity <- wire("validity")
  }, {
    session$setInputs(nav = "study", ws_study = "ingest")
    expect_identical(seen$cfa, 0L)
    # Structure opens on Dimensionality: not the CFA page.
    session$setInputs(nav = "structure", ws_structure = "retention")
    expect_identical(seen$cfa, 0L)
    session$setInputs(ws_structure = "cfa")
    expect_identical(seen$cfa, 1L)
    expect_identical(seen$validity, 0L)
    # Leaving and returning refits nothing; another workspace's page runs
    # no hidden workflow.
    session$setInputs(nav = "evidence", ws_evidence = "validity")
    expect_identical(seen$cfa, 1L)
    expect_identical(seen$validity, 0L)
    session$setInputs(`validity-fit_go` = 1L)
    expect_identical(seen$validity, 1L)
    session$setInputs(nav = "structure")
    expect_identical(seen$cfa, 1L)
  })
})

# ---- (e) sieve_go() ----------------------------------------------------------

test_that("sieve_go() selects the page's tab, or its workspace and then the page", {
  calls <- list()
  testthat::local_mocked_bindings(
    nav_select = function(id, selected = NULL, session = NULL) {
      calls[[length(calls) + 1L]] <<- c(id = id, selected = selected)
      invisible(NULL)
    },
    .package = "bslib"
  )
  fake <- list(rootScope = function() "root")
  sieve_go(fake, "efa")
  expect_identical(calls, list(
    c(id = "nav", selected = "structure"),
    c(id = "ws_structure", selected = "efa")
  ))
  calls <- list()
  sieve_go(fake, "scoring")
  expect_identical(calls, list(
    c(id = "reliab-panels", selected = "scoring"),
    c(id = "nav", selected = "scores"),
    c(id = "ws_scores", selected = "reliab")
  ))
  calls <- list()
  sieve_go(fake, "not_a_page")
  expect_length(calls, 0L)
})

test_that("the Optimizer's link is shown or hidden in the navset that holds it", {
  shown <- list()
  testthat::local_mocked_bindings(
    nav_show = function(id, target, select = FALSE, session = NULL) {
      shown[[length(shown) + 1L]] <<- c(what = "show", id = id, target = target)
      invisible(NULL)
    },
    nav_hide = function(id, target, session = NULL) {
      shown[[length(shown) + 1L]] <<- c(what = "hide", id = id, target = target)
      invisible(NULL)
    },
    .package = "bslib"
  )
  shiny::testServer(function(input, output, session) {
    state <- sieve_new_state()
    sieve_workflow_wire(state, session)
  }, {
    session$flushReact()
    # A new session has a theoretical basis: the Optimizer is hidden.
    expect_identical(shown[[1]][["what"]], "hide")
    expect_identical(shown[[1]][["target"]], "optimize")
    expect_identical(shown[[1]][["id"]], "ws_structure")
    sieve_set_construct_basis(state, "empirical")
    session$flushReact()
    last <- shown[[length(shown)]]
    expect_identical(last[["what"]], "show")
    expect_identical(last[["id"]], "ws_structure")
  })
})

# ---- the application server: the guide, the nudge and the study goal ---------

test_that("the page guide follows the page of the workspace, and the study goal only suggests a route", {
  shiny::testServer(app_server, {
    session$setInputs(nav = "structure", ws_structure = "cfa")
    expect_identical(current_page(), "cfa")
    expect_match(paste(as.character(output$page_workflow$html), collapse = " "), "CFA: what's needed here", fixed = TRUE)
    session$setInputs(nav = "scores", ws_scores = "reliab", `reliab-panels` = "scoring")
    expect_identical(current_page(), "scoring")
    expect_match(paste(as.character(output$page_workflow$html), collapse = " "), "Scoring and Norms: what's needed here", fixed = TRUE)
    # The study goal: a route, nothing computed, nothing recorded, and not
    # part of the project record.
    log_before <- shiny::isolate(state$codelog)
    expect_match(paste(as.character(output$study_route$html), collapse = " "), "Choose a goal", fixed = TRUE)
    session$setInputs(study_goal = "evaluate")
    expect_identical(study_goal(), "evaluate")
    expect_match(paste(as.character(output$study_route$html), collapse = " "), "CFA directly", fixed = TRUE)
    expect_identical(shiny::isolate(state$codelog), log_before)
    expect_false("study_goal" %in% names(shiny::isolate(shiny::reactiveValuesToList(state))))
    # A value outside the choices is ignored.
    session$setInputs(study_goal = "something else")
    expect_identical(study_goal(), "evaluate")
  })
})

test_that("the page guide follows a page value given directly", {
  shiny::testServer(app_server, {
    expect_identical(current_page(), "ingest")
    session$setInputs(nav = "cfa")
    expect_identical(current_page(), "cfa")
    expect_match(paste(as.character(output$page_workflow$html), collapse = " "), "CFA: what's needed here", fixed = TRUE)
    session$setInputs(nav = "repro")
    expect_identical(current_page(), "repro")
    expect_match(paste(as.character(output$page_workflow$html), collapse = " "), "Reproducible Code: what's needed here", fixed = TRUE)
  })
})

# ---- (d) run_app() refuses what it does not take ------------------------------

test_that("run_app() refuses an argument it does not take, with where the setting belongs", {
  # An argument run_app() does not take is refused with where it belongs;
  # stored as a golem option, which nothing reads, it would be lost without
  # a word.
  refusal <- function(expr) {
    tryCatch({
      expr
      NULL
    }, sieve_run_app_argument = function(e) conditionMessage(e))
  }
  ws <- refusal(run_app(workspaces = "flat"))
  expect_type(ws, "character")
  expect_match(ws, "run_app() takes onStart, options, enableBookmarking and uiPattern only, and was given 'workspaces'.", fixed = TRUE)
  expect_match(ws, "There is no workspaces argument: the five workspaces", fixed = TRUE)
  expect_match(ws, "only arrangement of the pages", fixed = TRUE)
  expect_match(refusal(run_app(siever.workspaces = TRUE)), "There is no workspaces argument", fixed = TRUE)
  # Shiny's run settings go inside `options`, with the value given.
  port <- refusal(run_app(port = 7654, launch.browser = FALSE))
  expect_match(port, "was given 'port', 'launch.browser'.", fixed = TRUE)
  expect_match(port, "run_app(options = list(port = 7654, launch.browser = FALSE))", fixed = TRUE)
  # SIEVER's settings are options() set before the call, under their
  # sieve. names whichever way they were spelled.
  cores <- refusal(run_app(sieve.cores = 4))
  expect_match(cores, "options(sieve.cores = 4)", fixed = TRUE)
  expect_false(grepl("workspaces", cores, fixed = TRUE))
  expect_match(refusal(run_app(rmsea_conf_level = 0.95)), "options(sieve.rmsea_conf_level = 0.95)", fixed = TRUE)
  expect_match(refusal(run_app(siever.auto_run = "never")), 'options(sieve.auto_run = "never")', fixed = TRUE)
  # A sieve. or siever. name that is none of the settings is not presented
  # as one (options() would hold it and nothing would read it): the message
  # says SIEVER has no such option, names the nearest setting when one is
  # close, and lists the real ones.
  core <- refusal(run_app(sieve.core = 4))
  expect_false(grepl("options(sieve.core = 4)", core, fixed = TRUE))
  expect_match(core, "SIEVER has no option 'sieve.core' (perhaps sieve.cores); its options, set with options() before run_app(), are sieve.rmsea_conf_level, sieve.cores, sieve.auto_run, sieve.memory_limit, sieve.wlsmv_limit, sieve.efa_memory, sieve.efa_share.", fixed = TRUE)
  foo <- refusal(run_app(siever.foo = 1))
  expect_false(grepl("options(siever.foo = 1)", foo, fixed = TRUE))
  expect_match(foo, "SIEVER has no option 'siever.foo'; its options", fixed = TRUE)
  expect_false(grepl("workspaces", foo, fixed = TRUE))
  workspace <- refusal(run_app(sieve.workspace = TRUE))
  expect_false(grepl("options(sieve.workspace", workspace, fixed = TRUE))
  expect_match(workspace, "SIEVER has no option 'sieve.workspace' (perhaps a workspaces argument)", fixed = TRUE)
  expect_match(workspace, "There is no workspaces argument: the five workspaces", fixed = TRUE)
  # An unnamed extra is named by its position; an unknown name is listed
  # with no hint it does not need.
  expect_match(refusal(run_app(NULL, list(), NULL, "/", 3)), "an unnamed argument in position 5", fixed = TRUE)
  other <- refusal(run_app(colour = "blue"))
  expect_identical(other, "run_app() takes onStart, options, enableBookmarking and uiPattern only, and was given 'colour'.")
  # The value is not evaluated to name the argument.
  expect_match(refusal(run_app(sieve.cores = stop("not evaluated"))), "options(sieve.cores = stop(\"not evaluated\"))", fixed = TRUE)
  # The arguments it takes still build the app, and no golem option is stored.
  app <- run_app(options = list(port = 7654L))
  expect_s3_class(app, "shiny.appobj")
  expect_identical(app$options$port, 7654L)
  expect_length(app$appOptions$golem_options, 0L)
  # The options the refusal lists are the ones SIEVER reads with getOption().
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  code <- unlist(lapply(list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE),
    readLines, warn = FALSE))
  read <- unique(unlist(regmatches(code, gregexpr('getOption[(]"sieve[.][a-z_]+', code))))
  listed <- sub(".*its options, set with options[(][)] before run_app[(][)], are ", "", core)
  listed <- regmatches(listed, gregexpr("sieve[.][a-z_]+", listed))[[1L]]
  expect_setequal(listed, sub('^getOption[(]"', "", read))
})
