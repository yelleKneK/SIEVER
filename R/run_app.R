#' Run the SIEVER Application
#'
#' Launches the Scale Item Evaluation and Validation Environment in R, an
#' action-oriented workspace for applied psychometrics. Open a dataset
#' (or the bundled Holzinger and Swineford example), confirm the
#' measurement level of each item, then winnow the item pool with live
#' per-item diagnostics through factor analysis, reliability with
#' confidence intervals, validity evidence, and measurement invariance.
#' Every action records the equivalent R.
#'
#' @details
#' # Your First Five Minutes
#'
#' No data of your own is needed to learn the environment:
#'
#' 1. Run `run_app()` and stay on the **Ingest** tab.
#' 2. Keep the bundled example selected (the Holzinger and Swineford
#'    primary nine tests, n = 301, three theorized ability factors) and
#'    press **Load**. The example's nine items are declared continuous,
#'    so its measurement levels are confirmed when it loads and every
#'    analysis tab unlocks. (A file of your own is confirmed with
#'    **Confirm the study** after you review the proposed roles and
#'    types.)
#' 3. Visit **Screening** to see the univariate lay of the land, then
#'    **Dimensionality**: parallel analysis retains three factors here,
#'    matching the theory.
#' 4. Open **Items**, the winnowing workspace. Click an item's row to see
#'    its loadings, cross loadings, and what omega would be without it;
#'    exclude an item and watch every diagnostic update. Nothing is
#'    deleted; re-include it whenever you like.
#' 5. Open **Reliability** for omega with its confidence interval per
#'    construct, and **CFA** for the measurement model and its fit.
#' 6. Finish on **Reproducible Code**: the R equivalent of everything you
#'    just did, annotated, ready to download, beside a methods draft with
#'    selected saved evidence retained without an implicit refit.
#'
#' With your own data, prefer an SPSS `.sav` file when one exists: the
#' variable labels arrive as item wording, which the item-review and
#' wording-check panels put to work. The getting-started vignette
#' (`vignette("getting-started", package = "SIEVER")`) narrates a full
#' session, including what the recorded R looks like at each step.
#'
#' # Options You Can Set
#'
#' The arguments below go to Shiny: `run_app(options = list(port = 7654,
#' launch.browser = TRUE))` chooses the port and whether a browser opens.
#' `run_app()` takes no other argument. One it does not take stops the call
#' before the app starts, with the reason and the place the setting
#' belongs: `run_app(port = 7654)` says the port goes inside `options`, and
#' `run_app(sieve.cores = 4)` says to set `options(sieve.cores = 4)` first.
#' The pages are grouped in five workspaces, Study, Structure, Scores,
#' Evidence and Report, the only arrangement of the pages.
#'
#' The following are set with `options()` before the app starts, or before
#' a SIEVER function is called, and hold for the R session. Each has a
#' default, so none needs to be set.
#'
#' * `sieve.rmsea_conf_level`: the level of every RMSEA interval and of the
#'   interval of the unbiased SRMR beside the SRMR, one number strictly
#'   between 0 and 1, 0.90 by default (the next section).
#' * `sieve.cores`: the number of worker processes for the work that runs
#'   on several: the bootstraps, the Items page's diagnostics, and the
#'   exploratory fits by number of factors, whose Workers input on the
#'   Dimensionality page starts at it (the parallel analysis runs in one
#'   process). It holds in the pages and in the functions called without
#'   them: `sieve_reliability_construct()` and `sieve_analyze("reliability")`
#'   run their bootstraps on it unless their `workers` argument says
#'   otherwise. A setting is honored up to 64. Unset, it is the machine's
#'   cores less one, at most 8, and 1 on Windows (at most 2 under
#'   `R CMD check`'s `_R_CHECK_LIMIT_CORES_`). The Workers input goes up to
#'   the option's value when it is set, and otherwise to the same default
#'   computed with a ceiling of 64 rather than 8. The environment variable
#'   `SIEVER_CORES` is read when the option is not set. The results do not
#'   depend on it.
#' * `sieve.auto_run`: what runs without being asked. By default a first
#'   result runs by itself, on its own page, when it is estimated at two
#'   seconds or less, and anything longer waits for its button, which
#'   states the wait. `"never"` makes every result wait for its button;
#'   `"always"` runs a first result on its page whatever its estimate.
#' * `sieve.memory_limit`: the memory, in bytes, that one fit may plan to
#'   hold at its peak, estimated before it starts (for maximum likelihood
#'   from the number of items, for ordered items from the number of sample
#'   statistics and of respondents). A larger fit is refused with its
#'   estimate and the ways forward. The default is three quarters of the
#'   machine's physical memory.
#' * `sieve.wlsmv_limit`: the estimated peak memory, in bytes, that one fit
#'   of ordered items (WLSMV and the other least squares estimators) may
#'   need, when it should differ from `sieve.memory_limit`, its default.
#'   The estimate counts about twenty working copies of the weight matrix
#'   of the thresholds and polychoric correlations and lavaan's casewise
#'   scores, which grow with the number of respondents.
#' * `sieve.efa_memory`: the memory, in bytes, that the exploratory workers
#'   running at once may plan to hold; 8e9 by default. It limits how many
#'   run at once, never whether a solution is fit.
#'
#' For example, `options(sieve.cores = 4, sieve.auto_run = "never")`
#' before `run_app()` gives four workers and a session in which nothing is
#' fit until its button is pressed. One further switch is for diagnosis
#' only: `options(sieve.efa_share = FALSE)` makes each exploratory solution
#' compute its own sample statistics instead of sharing them.
#'
#' # The Level of the RMSEA Interval
#'
#' Every RMSEA interval the pages show is at 90 percent, the level whose two
#' limits decide the one-sided tests of close fit and of not close fit at
#' .05 (MacCallum, Browne & Sugawara, 1996), DMAR's default for the RMSEA
#' interval and lavaan's. The interval of the unbiased SRMR of
#' Maydeu-Olivares (2017), shown beside the SRMR on the CFA and SEM pages
#' and in the methods draft, is at the same level, DMAR's default for it
#' (`DMAR::sem_residual_fit()`). Every other interval (loadings, omega, H,
#' factor correlations, HTMT) is at 95 percent unless the page's confidence
#' level input sets another, and every label names its level. To show
#' these fit measure intervals at another level, set the option before
#' starting the app, `options(sieve.rmsea_conf_level = 0.95)`; the recorded
#' R passes the level in force to `lavaan::fitMeasures()` and to DMAR. A
#' value that is not one number strictly between 0 and 1 (95, say) is not
#' used, and SIEVER warns once in the session that the intervals stay at 90
#' percent. [sieve_default_rmsea_conf_level()] states the policy in full.
#'
#' @param onStart A function that runs before the app starts, passed to
#'   [shiny::shinyApp()].
#' @param options A named list of options passed to [shiny::shinyApp()].
#' @param enableBookmarking One of `"url"`, `"server"`, or `"disable"`,
#'   passed to [shiny::shinyApp()].
#' @param uiPattern A regular expression matched against the request path,
#'   passed to [shiny::shinyApp()].
#' @param ... Not used. `run_app()` takes the four arguments above and
#'   nothing else; any other argument stops the call before the app starts,
#'   with a message that says where the setting belongs: Shiny's run
#'   settings (`port`, `host`, `launch.browser` and the like) go inside
#'   `options`, SIEVER's own settings (`sieve.cores`,
#'   `sieve.rmsea_conf_level`, and the others above) go in [options()]
#'   before the call (another `sieve.` name, such as `sieve.core`, is no
#'   option of SIEVER's, and the message says so and lists the real
#'   ones), and there is no `workspaces` argument, since the five
#'   workspaces are the only arrangement of the pages.
#' @return A Shiny application object; called for its side effect of
#'   starting the app.
#' @author Ken Kelley
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  # Refuse, before anything is built, what the call cannot use. The
  # arguments are read unevaluated, so a misplaced setting is named even
  # when its value could not be computed.
  if (...length() > 0L) {
    sieve_run_app_refuse(match.call(expand.dots = FALSE)$...)
  }
  # golem's wrapper is kept for its maintenance page (the
  # GOLEM_MAINTENANCE_ACTIVE environment variable of a hosted deployment);
  # SIEVER reads no golem option.
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list()
  )
}

#' The refusal of an argument run_app() does not take
#'
#' `run_app()` has four arguments, all passed to [shiny::shinyApp()].
#' Stored as a golem option, which SIEVER never reads, any other setting
#' given in `...` (a port, a SIEVER option, a `workspaces` argument) would
#' be dropped without a word. This builds the one refusal for all of them:
#' it names each argument given and says where each kind belongs. Shiny's
#' run settings go inside `options`; SIEVER's settings (named with or
#' without the `sieve.` prefix, or with the package name's `siever.`
#' prefix) are R options set before the call, shown with the value given
#' under the `sieve.` name `getOption()` reads; another
#' name with either prefix (`sieve.core`, `sieve.workspace`) is said to be
#' no option of SIEVER's, with the setting it is nearest to when that is
#' one or two edits away and the list of the real ones, since an option
#' set under it would be dropped without a word; and `workspaces` (or
#' `siever.workspaces`) is explained as no argument, since the five
#' workspaces are the only arrangement.
#'
#' @param dots The unevaluated arguments of `...`, as
#'   `match.call(expand.dots = FALSE)$...` gives them.
#' @return Does not return; stops with class `sieve_run_app_argument`.
#' @author Ken Kelley
#' @noRd
sieve_run_app_refuse <- function(dots) {
  nm <- names(dots)
  if (is.null(nm)) nm <- rep("", length(dots))
  shown <- ifelse(
    nzchar(nm), sQuote(nm, FALSE),
    sprintf("an unnamed argument in position %d", 4L + seq_along(dots))
  )
  value_text <- function(expr) {
    text <- paste(deparse(expr, width.cutoff = 60L), collapse = " ")
    if (nchar(text) > 40L) "..." else text
  }
  # Shiny's own run settings, which shinyApp() takes inside `options`.
  shiny_run <- c(
    "port", "host", "launch.browser", "display.mode", "quiet", "test.mode",
    "workerId", "width", "height"
  )
  # SIEVER's settings, read with getOption() under the sieve. prefix.
  sieve_settings <- c(
    "rmsea_conf_level", "cores", "auto_run", "memory_limit", "wlsmv_limit",
    "efa_memory", "efa_share"
  )
  is_workspaces <- nm %in% c("workspaces", "siever.workspaces", "sieve.workspaces")
  bare <- sub("^siever?\\.", "", nm)
  is_shiny <- nm %in% shiny_run
  is_setting <- !is_workspaces & bare %in% sieve_settings
  # A sieve. or siever. name that is not one of the settings is no option of
  # SIEVER's: options() would hold it and nothing would read it.
  is_unknown <- !is_workspaces & !is_setting & grepl("^siever?\\.", nm)
  hints <- character(0)
  if (any(is_shiny)) {
    given <- nm[is_shiny]
    hints <- c(hints, sprintf(
      "%s %s how Shiny runs the app and %s inside options: run_app(options = list(%s)).",
      paste(sQuote(given, FALSE), collapse = ", "),
      if (length(given) == 1L) "sets" else "set",
      if (length(given) == 1L) "goes" else "go",
      paste(sprintf("%s = %s", given, vapply(dots[is_shiny], value_text, "")), collapse = ", ")
    ))
  }
  if (any(is_setting)) {
    set_as <- paste0("sieve.", bare[is_setting])
    hints <- c(hints, sprintf(
      "SIEVER's settings are R options, set before run_app() and held for the session: options(%s); ?run_app lists them under Options You Can Set.",
      paste(sprintf("%s = %s", set_as, vapply(dots[is_setting], value_text, "")), collapse = ", ")
    ))
  }
  if (any(is_unknown)) {
    # The setting nearest an unknown name, when it is one or two edits
    # away, is named as the one probably meant; a name near workspaces
    # (sieve.workspace) also gets the note on the workspaces argument.
    known <- c(sieve_settings, "workspaces")
    near <- vapply(bare[is_unknown], function(b) {
      d <- utils::adist(b, known)[1L, ]
      if (min(d) <= 2L) known[which.min(d)] else NA_character_
    }, "", USE.NAMES = FALSE)
    meant <- ifelse(is.na(near), "", ifelse(near == "workspaces",
      " (perhaps a workspaces argument)", sprintf(" (perhaps sieve.%s)", near)))
    hints <- c(hints, sprintf(
      "SIEVER has no option %s; its options, set with options() before run_app(), are %s.",
      paste0(sQuote(nm[is_unknown], FALSE), meant, collapse = ", "),
      paste(paste0("sieve.", sieve_settings), collapse = ", ")
    ))
    is_workspaces[is_unknown] <- near %in% "workspaces"
  }
  if (any(is_workspaces)) {
    hints <- c(hints, paste(
      "There is no workspaces argument: the five workspaces (Study, Structure, Scores,",
      "Evidence and Report) are the only arrangement of the pages,",
      "so there is nothing to choose; remove the argument."
    ))
  }
  stop(errorCondition(paste(c(
    sprintf(
      "run_app() takes onStart, options, enableBookmarking and uiPattern only, and was given %s.",
      paste(shown, collapse = ", ")
    ),
    hints
  ), collapse = " "), class = "sieve_run_app_argument", call = NULL))
}
