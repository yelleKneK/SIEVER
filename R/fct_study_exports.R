# Study exports.

#' Export data and specification from one frozen project revision
#' @param snapshot Checked project snapshot.
#' @param file Destination tar.gz file.
#' @param values Raw or processed exported values.
#' @param means Include summary means.
#' @return Manifest invisibly.
#' @author Ken Kelley
#' @noRd
sieve_export_study_bundle <- function(snapshot, file, values = "raw", means = TRUE) {
  if (length(sieve_project_check(snapshot))) stop("The frozen project is not valid.", call. = FALSE)
  if (!values %in% sieve_export_kinds()) stop("Choose raw or processed values.", call. = FALSE)
  dir <- tempfile("siever_study_bundle_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # Sharing defaults are deliberately smaller than the private project backup.
  # Only item columns are eligible, even when identifiers or auxiliary columns
  # also exist in the raw frame. Removed rows have already left both frames.
  item_columns <- intersect(snapshot$meta$item, names(snapshot$items))
  clean_frame <- function(frame, columns) {
    # Strip row names and variable attributes, which can hold direct identifiers
    # or private item wording even after their columns have been removed.
    data.frame(lapply(frame[, columns, drop = FALSE], as.numeric), check.names = FALSE)
  }
  raw <- clean_frame(snapshot$raw, intersect(item_columns, names(snapshot$raw)))
  items <- clean_frame(snapshot$items, item_columns)
  # Each row keeps its number in the data as loaded.
  rows <- snapshot$other$case_index %||% seq_len(nrow(raw))
  data <- sieve_export_data(raw, items, snapshot$meta, values = values, rows = rows)
  utils::write.csv(data, file.path(dir, "data.csv"), row.names = FALSE, na = "")
  meta <- snapshot$meta
  # Specification data need analytic assignments and reversals, not private
  # labels, wording, free-text notes, or construct narratives.
  meta <- meta[, setdiff(names(meta), c("label", "wording", "question", "note")), drop = FALSE]
  sieve_spec_write(sieve_spec_table(raw, items, meta, list(), values = values), file.path(dir, "specification.csv"))
  summary <- sieve_summary_table(items, meta, means = means, basis = NULL)
  sieve_summary_write(summary, file.path(dir, "summary.csv"), dataset = "Shared study items")
  shared <- sieve_study_bundle_project(snapshot, raw, items, rows = rows)
  sieve_project_write(shared, file.path(dir, "project.siever.rds"))
  manifest <- list(
    format = "SIEVER matched study bundle", version = snapshot$siever_version,
    revision = snapshot$revision, values = values, rows = nrow(data), columns = names(data),
    complete_working_project = FALSE, shared_project_revision = shared$revision,
    sharing = "Item columns only. The fresh shared project preserves the current analytic item assignments, reversals, scoring policies, and sampling declaration. Removed rows, identifier and auxiliary columns, private wording, analyses, project history, and external-review records are excluded. After removals, data.csv begins with original_row (each row's number in the data as loaded), and the shared project keeps those row numbers.",
    summary = list(basis = "processed covariance; complete cases on included items", n = summary$n),
    response_policy = "Data and specification use the same frozen revision; processed data include recorded reversals.",
    removed_respondents = length(snapshot$other$removed_cases %||% list()), keys = snapshot$keys
  )
  dput(manifest,
    file.path(
      dir,
      "manifest.R"
    ),
    control = c(
      "keepNA",
      "keepInteger",
      "niceNames",
      "showAttributes",
      "digits17"
    )
  )
  writeLines(
    c(
      "SIEVER matched study files", paste("Values:", values),
      "data.csv and specification.csv are a pair from one frozen revision, with item columns only.",
      "summary.csv uses processed complete-case covariance, not a fitted model or an ordinal correlation matrix.",
      manifest$sharing,
      "project.siever.rds is a fresh sharing project, not the complete working project or its history.",
      "Use Save project separately for a complete private backup. A cluster identifier is not shared; the declared design remains and unsupported analyses are refused until a valid cluster column is supplied.",
      "manifest.R contains exact revision and provenance. Opening a project fits no model."
    ),
    file.path(
      dir,
      "README.txt"
    )
  )
  file <- normalizePath(file, mustWork = FALSE)
  withr::with_dir(dir, utils::tar(file, files = list.files(dir), compression = "gzip", tar = "internal"))
  invisible(manifest)
}

#' Construct a fresh sharing project without private session history
#' @param snapshot Original checked snapshot.
#' @param raw,items Attribute-free retained item frames.
#' @param rows Each row's number in the data as loaded (kept as the shared
#'   project's `case_index`), or `NULL` for 1 to n.
#' @return Checked project containing current analytic settings and items only.
#' @author Ken Kelley
#' @noRd
sieve_study_bundle_project <- function(snapshot, raw, items, rows = NULL) {
  state <- sieve_new_state()
  state$raw <- raw
  state$items <- items
  meta <- sieve_build_meta(raw, names(items))
  analytic <- intersect(c(
    "item", "construct", "facet", "type", "type_source", "status", "reverse",
    "rev_lo", "rev_hi", "rev_bounds"
  ), names(snapshot$meta))
  for (nm in analytic) meta[[nm]] <- snapshot$meta[[nm]][match(meta$item, snapshot$meta$item)]
  sieve_write_ledger(state, meta)
  state$roles <- sieve_declared_roles(raw, names(items))
  state$confirmed <- snapshot$confirmed
  state$estimator <- snapshot$estimator
  # A saved study whose setting is Follow the item types and whose
  # confirmation records MLR with FIML for continuous items keeps MLR with
  # FIML (sieve_snapshot_legacy_auto()): the shared project states how its
  # models were fit.
  state$estimator_choice <- if (sieve_snapshot_legacy_auto(snapshot)) "mlr" else snapshot$study_estimator_choice %||% "auto"
  state$construct_basis <- snapshot$construct_basis
  state$dataset_name <- "Shared study items"
  state$project_name <- "Shared study items"
  shiny::isolate(sieve_new_dataset(state))
  state$case_index <- if (length(rows) == nrow(raw)) as.integer(rows) else seq_len(nrow(raw))
  for (nm in c("sampling_design", "score_policy", "scoring_policies", "screening_spec")) {
    if (!is.null(snapshot$other[[nm]])) state[[nm]] <- snapshot$other[[nm]]
  }
  # The declared sampling unit is kept and the cluster identifier is not:
  # its column is not among the shared item columns, so the shared project
  # declares the design without it, as the README says. Analyses that need
  # the cluster column are refused until a valid one is supplied.
  design <- shiny::isolate(state$sampling_design)
  if (is.list(design) && length(design$cluster)) {
    design["cluster"] <- list(NULL)
    state$sampling_design <- design
  }
  # Only the numeric reversal recipe survives. Original transformations can
  # carry notes, identifiers, before-data stores, or private history.
  state$transforms <- lapply(which(meta$reverse %in% TRUE), function(i) list(
    dataset_id = shiny::isolate(state$dataset_id), type = "keying", item = meta$item[i],
    lo = meta$rev_lo[i], hi = meta$rev_hi[i], bounds = meta$rev_bounds[i]
  ))
  out <- sieve_project_snapshot(state)
  problems <- sieve_project_check(out)
  if (length(problems)) stop(
    paste(
      "The sharing project could not be constructed:",
      paste(problems,
        collapse = "; "
      )
    ),
    call. = FALSE
  )
  out
}
