#' The DataTables extensions bundled with the installed DT
#'
#' DT ships the DataTables extensions as directories inside its own library
#' tree and refuses an extension whose directory is absent ("The extension
#' RowGroup does not exist"). The CRAN build has carried RowGroup since DT 0.3,
#' but a repackaged build need not: the Ubuntu 24.04 `r-cran-dt` package
#' (0.31+dfsg-1) removes several bundled extensions, RowGroup among them. The
#' question the app has to ask is therefore whether the extension is present,
#' not which version of DT is installed.
#'
#' @return A character vector of extension names (possibly empty).
#' @author Ken Kelley
#' @noRd
sieve_dt_extensions <- function() {
  path <- system.file("htmlwidgets", "lib", "datatables-extensions", package = "DT")
  if (!nzchar(path)) {
    return(character(0))
  }
  list.dirs(path, full.names = FALSE, recursive = FALSE)
}

#' Does the installed DT bundle a DataTables extension?
#'
#' @param extension Extension name as DT spells it, for example `"RowGroup"`.
#' @return Logical, length one.
#' @author Ken Kelley
#' @noRd
sieve_dt_has_extension <- function(extension) {
  extension %in% sieve_dt_extensions()
}

#' What the item grid needs in order to draw its row groups
#'
#' The Ingest item grid separates "Items in use" from "Unused items" with the
#' DataTables RowGroup extension. When the installed DT has the extension the
#' grid asks for it and hides the column that carries the group label. When it
#' does not, the grid is still drawn and still ordered by group, the group
#' label is shown as an ordinary first column, and a caption says what is
#' missing and how to restore it. The alternative, passing an extension DT
#' does not have, is an error that leaves the tab without its central control.
#'
#' @param column Zero based index of the column holding the group label.
#' @return A list with `available` (logical), `extensions` (the `extensions`
#'   argument for [DT::datatable()]), `options` (a list of DataTables options
#'   to append), and `caption` (`NULL`, or the note to pass as `caption`).
#' @author Ken Kelley
#' @noRd
sieve_dt_row_group <- function(column) {
  available <- sieve_dt_has_extension("RowGroup")
  list(
    available = available,
    extensions = if (available) "RowGroup" else character(0),
    options = if (available) list(rowGroup = list(dataSrc = column)) else list(),
    caption = if (available) NULL else sieve_dt_extension_note("RowGroup")
  )
}

#' The note shown when a DataTables extension is missing
#'
#' @param extension Extension name, for example `"RowGroup"`.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_dt_extension_note <- function(extension) {
  paste0(
    "This installation of the DT package does not include the DataTables ",
    extension, " extension, so the grid is shown without its \"Items in use\" ",
    "and \"Unused items\" dividers; the Group column carries the same ",
    "information. Some repackaged builds of DT (Linux distribution packages, ",
    "for example) leave bundled extensions out. Installing DT from CRAN ",
    "restores them: install.packages(\"DT\")."
  )
}

#' The optional packages and what each one is for
#'
#' SIEVER declares these under Suggests and tests for each at the point of use
#' with `sieve_have()`. This table is the one place that says, in the words
#' used in the interface, what a missing package costs the user and where.
#' lavaan and psych are not here: they are Imports, installed with SIEVER,
#' because every analysis tab needs one or both. GPArotation is not needed:
#' every rotated exploratory solution is fitted by `DMAR::fa_exploratory()`
#' or `lavaan::efa()`, whose rotations are lavaan's own gradient projection,
#' and no psych function SIEVER calls rotates.
#'
#' @return A data frame with character columns `package`, `provides`, and
#'   `tabs`, one row per optional package.
#' @author Ken Kelley
#' @noRd
sieve_optional_packages <- function() {
  data.frame(
    # callr is imported: the EFA page's longer DMAR solutions and its factor
    # count comparison, the Dimensionality page's model fit by k and its
    # longer VSS, the Items tab's longer factor analysis and the Network
    # search run in a separate process. Every rotation is lavaan's own,
    # through DMAR::fa_exploratory() or lavaan::efa(), so GPArotation is not
    # needed. semTools is not needed: the missing-data Bollen-Stine bootstrap
    # and the two-stage residual statistic are DMAR's. ggplot2 is imported:
    # it draws the scree, parallel analysis, fit by k and structures figures,
    # which have no fallback.
    package = c("svglite", "haven", "psychonetrics", "mice", "lavaan.mi"),
    provides = c(
      "path diagrams written as SVG where R's own SVG device is not available (PDF and PNG need nothing)",
      "reading SPSS (.sav) and Stata (.dta) files and writing a labeled .sav",
      "the psychometric network models of the Network tab, a trial (the item, residual and latent partial correlation networks)",
      "multiple imputation for the ordered-item sensitivity analysis",
      "pooled categorical model estimates and intervals after multiple imputation"
    ),
    tabs = c(
      "CFA, SEM",
      "Ingest",
      "Network", "CFA, SEM", "CFA, SEM"
    ),
    stringsAsFactors = FALSE
  )
}

#' Which optional packages are not installed
#'
#' Read with `sieve_installed()`, which loads nothing: the notice is built
#' at every app launch, and asking whether a package is installed must not
#' load it. (`sieve_have()` loads each namespace, so asking it would load
#' every optional package at each launch; lavaan.mi, loaded so while lavaan
#' is not attached, would keep its methods from dispatching in the user's
#' session for the rest of it; see `sieve_mi_model()`.)
#'
#' @return A character vector of package names, in the order of
#'   `sieve_optional_packages()`; empty when everything is installed.
#' @author Ken Kelley
#' @noRd
sieve_missing_packages <- function() {
  pkgs <- sieve_optional_packages()$package
  pkgs[!vapply(pkgs, sieve_installed, logical(1))]
}

#' The R command that installs a set of packages
#'
#' @param packages Character vector of package names.
#' @return A length one character string of runnable R.
#' @author Ken Kelley
#' @noRd
sieve_install_command <- function(packages) {
  if (length(packages) == 1L) {
    return(sprintf("install.packages(\"%s\")", packages))
  }
  sprintf("install.packages(c(%s))", paste0("\"", packages, "\"", collapse = ", "))
}

#' A notice naming the optional packages that are missing
#'
#' The analysis tabs test for their optional packages where they use them, and
#' some of those tests simply render nothing when a package is absent (an
#' SVG diagram without svglite, for example). So that no
#' tab is ever silently empty for want of a package, the page carries one
#' notice above every tab that names each missing package, what it provides,
#' the tabs that need it, and the command that installs it. With everything
#' installed there is no notice and the page is unchanged.
#'
#' @param missing Character vector of missing package names (defaults to the
#'   packages actually missing).
#' @return `NULL` when nothing is missing, otherwise a [shiny::div()].
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny tags div strong
sieve_missing_packages_notice <- function(missing = sieve_missing_packages()) {
  if (!length(missing)) {
    return(NULL)
  }
  info <- sieve_optional_packages()
  info <- info[match(missing, info$package), , drop = FALSE]
  # One line on every page, with what each package provides and the
  # install command behind it (the full notice would take 138 pixels above
  # every page's first result at 1280 by 800).
  div(
    class = "alert alert-warning alert-dismissible fade show mb-1 py-1 small sieve-missing-packages",
    role = "alert",
    tags$details(
      tags$summary(
        strong(if (length(missing) == 1L) "An optional package is not installed: "
        else "Some optional packages are not installed: "),
        paste(sprintf("%s (%s)", info$package, info$tabs), collapse = "; "),
        ". What they provide, and the install command"
      ),
      "The parts of the environment that depend on them are unavailable until they are:",
      tags$ul(
        class = "mb-2",
        lapply(seq_len(nrow(info)), function(i) {
          tags$li(
            tags$code(info$package[i], .noWS = "after"),
            sprintf(": %s (%s).", info$provides[i], info$tabs[i])
          )
        })
      ),
      "Install from the R console, then restart SIEVER: ",
      tags$code(sieve_install_command(missing))
    ),
    tags$button(
      type = "button", class = "btn-close", `data-bs-dismiss` = "alert",
      `aria-label` = "Close"
    )
  )
}

#' The cache of graphics device probes for this R session
#'
#' One entry per device format, written by `sieve_device_works()`.
#'
#' @author Ken Kelley
#' @noRd
sieve_device_cache <- new.env(parent = emptyenv())

#' Open a graphics device on a file, for the probe
#'
#' The one place the probe names a device function, so that a test can
#' stand in for it. Nothing is drawn.
#'
#' @param format `"svg"` (`grDevices::svg()`, which needs cairo),
#'   `"svglite"` (the svglite package), `"pdf"`, or `"png"`.
#' @param file The file the device writes.
#' @return The value of the device function, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_device_open <- function(format, file) {
  switch(format,
    svg = grDevices::svg(file, width = 2, height = 2),
    svglite = svglite::svglite(file, width = 2, height = 2),
    pdf = grDevices::pdf(file, width = 2, height = 2),
    png = grDevices::png(file, width = 96, height = 96),
    stop("unknown device format: ", format)
  )
}

#' Can a graphics device of this format be opened here?
#'
#' The question matters for the SVG download of a path diagram, which falls
#' back on `grDevices::svg()` when the svglite package is not installed.
#' `capabilities("cairo")` says whether R was built with cairo, not whether
#' cairo loads: on a build whose X11 module is missing, `capabilities()`
#' answers `TRUE` and `svg()` fails to open the device. So the question is
#' answered by trying. The device is opened once on a temporary file inside
#' `sieve_capture()`, the attempt counts as working only when it raised no
#' error and exactly one new device appeared, that device is closed, the
#' device that was current before is made current again, and the answer is
#' kept for the rest of the R session, so the interface asks once and not on
#' every render. `svglite` is reported as not working when the package is not
#' installed; `pdf` and `png` are expected to work on every build and are
#' offered for symmetry.
#'
#' The SVG download of a diagram is possible when
#' `sieve_device_works("svglite") || sieve_device_works("svg")`: the first
#' is the device the export prefers and is `FALSE` without the package, the
#' second is R's own device. Neither draws anything, and a module that asks
#' on every render costs one probe per format per session.
#'
#' @param format `"svg"`, `"svglite"`, `"pdf"`, or `"png"`.
#' @param refresh `TRUE` probes again instead of answering from the cache.
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_device_works <- function(format = c("svg", "svglite", "pdf", "png"), refresh = FALSE) {
  format <- match.arg(format)
  if (!isTRUE(refresh)) {
    known <- get0(format, envir = sieve_device_cache, inherits = FALSE)
    if (is.logical(known) && length(known) == 1L) {
      return(known)
    }
  }
  works <- sieve_device_probe(format)
  assign(format, works, envir = sieve_device_cache)
  works
}

#' Try the device once (the uncached part of `sieve_device_works()`)
#'
#' @inheritParams sieve_device_works
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_device_probe <- function(format) {
  if (identical(format, "svglite") && !sieve_have("svglite")) {
    return(FALSE)
  }
  file <- tempfile("sieve_device_probe_", fileext = paste0(".", sub("svglite", "svg", format)))
  on.exit(unlink(file), add = TRUE)
  before <- grDevices::dev.list()
  current <- grDevices::dev.cur()
  tried <- sieve_capture(sieve_device_open(format, file))
  opened <- setdiff(grDevices::dev.list(), before)
  closed <- sieve_capture(for (dev in opened) grDevices::dev.off(dev))
  # Only the probe's devices were closed; whatever was current before the
  # probe is current again (dev.off() moves to the next open device).
  if (current %in% grDevices::dev.list()) {
    grDevices::dev.set(current)
  }
  isTRUE(tried$ok) && length(opened) == 1L && isTRUE(closed$ok)
}

#' The versions the running environment is made of
#'
#' What the About dialog reports beside the SIEVER version: the R version
#' and, for each package the analyses depend on, the version of the
#' namespace that is loaded in this session. A package that is installed
#' but has not been loaded yet (lavaan and psych load on first use) is
#' reported with its installed version and said to be not loaded, without
#' loading it; a package that is not installed is said to be missing.
#'
#' @param packages The packages to report, in the order to show them.
#' @return A data frame with character columns `component`, `version`, and
#'   `status` (`"loaded"`, `"installed, not loaded"`, or
#'   `"not installed"`; `"running"` for R).
#' @author Ken Kelley
#' @noRd
sieve_about_versions <- function(packages = c("DMAR", "lavaan", "psych", "shiny", "bslib", "DT")) {
  one <- function(pkg) {
    if (isNamespaceLoaded(pkg)) {
      return(c(as.character(getNamespaceVersion(pkg)), "loaded"))
    }
    installed <- tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
    if (is.na(installed)) {
      return(c("", "not installed"))
    }
    c(installed, "installed, not loaded")
  }
  rows <- vapply(packages, one, character(2))
  data.frame(
    component = c("R", packages),
    version = c(paste(R.version$major, R.version$minor, sep = "."), rows[1, ]),
    status = c("running", rows[2, ]),
    stringsAsFactors = FALSE
  )
}

#' The citation of SIEVER, as text
#'
#' Read from the package's CITATION file with the version of the running
#' code, so the text is the one `citation("SIEVER")` gives for this
#' version. When the file cannot be read, the dialog points at
#' `citation("SIEVER")` instead of showing nothing.
#'
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_citation_text <- function() {
  text <- tryCatch(
    {
      file <- app_sys("CITATION")
      desc <- app_sys("DESCRIPTION")
      if (!nzchar(file) || !nzchar(desc)) {
        stop("no CITATION file")
      }
      meta <- as.list(read.dcf(desc)[1, ])
      meta$Version <- sieve_version()
      bib <- utils::readCitationFile(file, meta = meta)
      paste(format(bib, style = "textVersion"), collapse = " ")
    },
    error = function(e) NA_character_
  )
  if (is.na(text) || !nzchar(text)) {
    return("To cite SIEVER, run citation(\"SIEVER\") in R.")
  }
  text
}

#' The table of the About dialog
#'
#' One table: the SIEVER version and how it was loaded, the R version, and
#' the versions of the packages the analyses depend on, each with whether it
#' is loaded in this session; the citation follows the table.
#'
#' @return A [shiny::tagList()].
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny tags tagList p strong
sieve_about_table <- function() {
  v <- sieve_about_versions()
  rows <- c(
    list(tags$tr(
      tags$td("SIEVER"), tags$td(sieve_version()),
      tags$td(if (sieve_is_dev_source()) "development source" else "installed package")
    )),
    lapply(seq_len(nrow(v)), function(i) {
      tags$tr(tags$td(v$component[i]), tags$td(v$version[i]), tags$td(v$status[i]))
    })
  )
  tagList(
    tags$table(
      class = "table table-sm sieve-about",
      tags$thead(tags$tr(tags$th("Component"), tags$th("Version"), tags$th("In this session"))),
      tags$tbody(rows)
    ),
    p(class = "small", strong("To cite SIEVER: "), sieve_citation_text())
  )
}

#' SHA-256 for a Local File Across Supported R Versions
#' @param file Existing file path.
#' @param base_available Whether the current tools namespace has sha256sum.
#'   The explicit flag supports verification of the older-R fallback.
#' @return A lowercase SHA-256 hexadecimal digest.
#' @author Ken Kelley
#' @noRd
sieve_sha256_file <- function(file, base_available = exists("sha256sum", asNamespace("tools"), inherits = FALSE)) {
  if (length(file) != 1L || !file.exists(file)) stop("Select an existing file to hash.", call. = FALSE)
  if (isTRUE(base_available)) {
    return(tolower(unname(get("sha256sum", asNamespace("tools"))(file))))
  }
  # macOS and Linux provide shasum/sha256sum; Windows provides certutil.
  # These are checked explicitly, and their output must contain a full digest.
  for (program in c("shasum", "sha256sum", "certutil")) {
    executable <- Sys.which(program)
    if (!nzchar(executable)) next
    args <- switch(program,
      shasum = c("-a", "256", shQuote(file)),
      sha256sum = shQuote(file),
      certutil = c("-hashfile", shQuote(file), "SHA256")
    )
    output <- suppressWarnings(system2(executable, args, stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(output, "status")) && attr(output, "status") != 0L) next
    matches <- regmatches(output, regexpr("[[:xdigit:]]{64}", output))
    matches <- matches[nchar(matches) == 64L]
    if (length(matches) == 1L) return(tolower(matches))
  }
  stop("SHA-256 is unavailable: use R with tools::sha256sum or a system SHA-256 utility.", call. = FALSE)
}

