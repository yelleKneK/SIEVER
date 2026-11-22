# Replay keeps one copy of each source dataset and changed item column.
# Logged expressions are not parsed or rewritten when the script is assembled.

#' Read Structured Script Steps, or Import Them From the Log
#' @param state Shared study state.
#' @return Named records with number, note, and source lines.
#' @author Ken Kelley
#' @noRd
sieve_script_records <- function(state) {
  shiny::isolate({
    log <- state$codelog %||% character()
    operations <- state$replay_operations %||% list()
    structured <- length(operations) && all(vapply(operations,
      function(step) !is.null(step$note) && !is.null(step$code), logical(1)))
    if (structured) {
      records <- lapply(names(operations), function(number) list(number = number,
        note = operations[[number]]$note, code = operations[[number]]$code))
      names(records) <- names(operations)
      view <- unlist(lapply(records, function(step) c(
        sprintf("# [%s] %s", step$number, step$note), step$code, "")), use.names = FALSE)
      if (identical(view, log)) return(records)
    }
    # Without structured records that reproduce the log (a project file
    # that holds only the human-readable log), the steps are read from the
    # log's comment markers; the structured records above are not parsed.
    starts <- which(grepl("^# \\[[0-9]+\\] ", log))
    if (!length(starts)) return(list())
    ends <- c(starts[-1L] - 1L, length(log))
    records <- lapply(seq_along(starts), function(i) {
      block <- log[starts[i]:ends[i]]
      code <- block[-1L]
      while (length(code) && identical(tail(code, 1L), "")) code <- head(code, -1L)
      list(number = sub("^# \\[([0-9]+)\\].*", "\\1", block[1L]),
        note = sub("^# \\[[0-9]+\\] ", "", block[1L]), code = code)
    })
    names(records) <- vapply(records, `[[`, "", "number")
    records
  })
}

#' Plain scientific inputs for a recorded computation
#' @param state Shared state.
#' @return Data, ledger, roles, respondent history, and scientific revision.
#' @author Ken Kelley
#' @noRd
sieve_replay_inputs <- function(state) {
  shiny::isolate(list(
    raw = state$raw, items = state$items, meta = state$meta,
    roles = state$roles, case_index = state$case_index,
    removed_cases = state$removed_cases %||% list(),
    construct_basis = state$construct_basis,
    sampling_design = state$sampling_design, screening_spec = state$screening_spec,
    scoring_policies = state$scoring_policies, revision = sieve_revision(state),
    # The validation reservations (rows, proportion, seed, time), without
    # their evaluations (see sieve_holdout_brief(), fct_holdout.R).
    holdout = sieve_holdout_brief(state)
  ))
}

#' Store a compact input revision
#' @param state Shared state.
#' @param x Plain inputs from sieve_replay_inputs().
#' @return Revision key.
#' @author Ken Kelley
#' @noRd
sieve_replay_store <- function(state, x) {
  if (is.null(x$raw) || is.null(x$items) || is.null(x$meta)) return(NULL)
  if (!is.data.frame(x$raw) || !is.data.frame(x$items) || nrow(x$raw) != nrow(x$items)) {
    stop("Replay inputs must hold the same number of loaded-data and item-data rows. No input revision was recorded.",
      call. = FALSE
    )
  }
  current_rows <- x$case_index %||% seq_len(nrow(x$raw))
  if (!is.numeric(current_rows) || length(current_rows) != nrow(x$raw) || anyNA(current_rows) ||
    any(current_rows < 1L | current_rows != trunc(current_rows)) || anyDuplicated(current_rows)) {
    stop("Replay respondent identities must name each current row exactly once. No input revision was recorded.",
      call. = FALSE
    )
  }
  data <- shiny::isolate(state$replay_data) %||% list()
  records <- shiny::isolate(state$replay_states) %||% list()
  rows <- x$case_index %||% seq_len(nrow(x$raw))
  source <- x$raw
  index <- as.integer(rows)
  held <- x$removed_cases %||% list()
  if (length(held)) {
    for (r in held) {
      if (!r$row %in% index && is.data.frame(r$raw) && identical(names(r$raw), names(source))) {
        source <- sieve_cases_bind(source, r$raw)
        index <- c(index, as.integer(r$row))
      }
    }
    at <- order(index)
    source <- sieve_cases_take(source, at)
    index <- index[at]
  }
  base <- list(raw = source, index = index)
  source_key <- sieve_hash(base)
  if (is.null(data[[source_key]])) data[[source_key]] <- base
  patches <- list()
  bare <- function(v) {
    v <- unclass(v)
    attributes(v) <- NULL
    v
  }
  for (nm in names(x$items)) {
    if (nm %in% names(x$raw) && identical(x$items[[nm]], x$raw[[nm]])) next
    # A changed column is kept once. Removing rows does not duplicate the
    # remaining source columns, which dominate ordinary project size.
    # The patch is the column as the analysis holds it: the source values
    # without their attributes (the value labels of an SPSS column describe
    # the values as read, not their reflection), the rows in the analysis
    # from the item data, and the item data's own attributes.
    current <- x$items[[nm]]
    values <- if (nm %in% names(source)) bare(source[[nm]]) else rep(NA_real_, nrow(source))
    # The rows of removed respondents hold the item values held with them,
    # so a later removal does not store another copy of a changed column
    # whose values did not change; the patch is read
    # only at the rows in the analysis.
    for (r in held) {
      if (is.data.frame(r$items) && nm %in% names(r$items) && r$row %in% index) {
        values[match(as.integer(r$row), index)] <- bare(r$items[[nm]])
      }
    }
    values[match(rows, index)] <- bare(current)
    kept <- attributes(current)
    attributes(values) <- kept[setdiff(names(kept), "names")]
    key <- sieve_hash(values)
    if (is.null(data[[key]])) data[[key]] <- values
    patches[[nm]] <- key
  }
  compact <- x[setdiff(names(x), c("raw", "items"))]
  compact$source <- source_key
  compact$rows <- as.integer(rows)
  compact$item_names <- names(x$items)
  compact$patches <- patches
  key <- sieve_hash(compact)
  records[[key]] <- compact
  state$replay_data <- data
  state$replay_states <- records
  key
}

#' Reconstitute an input revision without evaluating code
#' @param record Compact revision.
#' @param data Content-addressed source values.
#' @return Plain inputs.
#' @author Ken Kelley
#' @noRd
sieve_replay_expand <- function(record, data) {
  if (!is.null(record$raw)) return(record) # a record that holds a full snapshot
  base <- data[[record$source]]
  if (is.null(base) || !identical(sieve_hash(base), record$source)) {
    stop("The replay source data are absent or do not match their recorded hash.", call. = FALSE)
  }
  at <- match(record$rows, base$index)
  if (anyNA(at)) stop("A recorded respondent is absent from the replay source.", call. = FALSE)
  raw <- sieve_cases_take(base$raw, at)
  items <- raw[, record$item_names, drop = FALSE]
  for (nm in names(record$patches)) {
    key <- record$patches[[nm]]
    values <- data[[key]]
    if (is.null(values) || !identical(sieve_hash(values), key)) {
      stop("A changed replay column is absent or does not match its recorded hash.", call. = FALSE)
    }
    items[[nm]] <- values[at]
  }
  x <- record[setdiff(names(record), c("source", "rows", "item_names", "patches"))]
  x$raw <- raw
  x$items <- items
  x <- sieve_labelled_ready(x)
  x[c("raw", "items", setdiff(names(x), c("raw", "items")))]
}

#' Keep exact input and output revisions for a logged step
#' @param state Shared state.
#' @param step Step number.
#' @param inputs Exact analysis inputs or post-operation inputs.
#' @param before Pre-operation inputs for a mutation.
#' @param kind Recorded operation category.
#' @param engine_identity `NULL`, or identity records of engines that ran
#'   the step in a background R process (see `sieve_log()`).
#' @return The output revision key invisibly.
#' @author Ken Kelley
#' @noRd
sieve_replay_capture <- function(state, step, inputs = NULL, before = NULL,
                                 kind = c("analysis", "mutation", "import"), passive = FALSE,
                                 engine_identity = NULL) {
  kind <- match.arg(kind)
  shiny::isolate({
    if (is.null(state$replay_versions)) {
      packages <- c("SIEVER", "DMAR", "lavaan", "psych")
      versions <- vapply(packages, function(package) {
        if (identical(package, "SIEVER")) return(sieve_version())
        if (sieve_have(package)) as.character(utils::packageVersion(package)) else "not installed"
      }, character(1))
      state$replay_versions <- c(R = getRversion() |> as.character(), versions)
    }
    x <- inputs %||% sieve_replay_inputs(state)
    key <- sieve_replay_store(state, x)
    if (is.null(key)) return(invisible(NULL))
    steps <- state$replay_steps %||% list()
    steps[[as.character(step)]] <- key
    operations <- state$replay_operations %||% list()
    operations[[as.character(step)]] <- list(
      kind = kind, passive = isTRUE(passive), after = key,
      engine_identity = sieve_recording_engine_identity(engine_identity),
      r_version = as.character(getRversion()),
      recorded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
      before = if (is.null(before)) key else sieve_replay_store(state, before)
    )
    state$replay_steps <- steps
    state$replay_operations <- operations
    invisible(key)
  })
}

#' Read Verified Inputs for a SIEVER Session Script
#'
#' Session scripts use a separate RDS input file. Its digest is checked
#' before the file is read, and every source dataset and changed column is
#' checked again when a revision is bound. The file contains research data;
#' share it only under the study's data-sharing arrangements.
#'
#' What the file held is checked type by type before any field of it is
#' read, as [sieve_project_read()] checks a project file: through
#' primitives that neither dispatch methods nor force promises, every value
#' must be `NULL`, an atomic vector or a list, not an S4 object, with only
#' classes of data or result classes of SIEVER, DMAR or lavaan. A file that
#' holds an environment, a function, a promise, R code, an external pointer,
#' an S4 object or another class is refused with the place and the type or
#' class found, and nothing in it is run. The hash shows the file is the one
#' the script names, not who wrote it.
#' @param file Path to the session input file.
#' @param hash Expected content digest printed in the session script.
#' @return A plain replay input store.
#' @examples
#' d <- DMAR::holzinger_swineford[, c(
#'   "id", "school", "t1_visual_perception", "t2_cubes", "t4_lozenges"
#' )]
#' spec <- sieve_specification_template(d)
#' spec$construct[spec$role == "item"] <- "Visual"
#' path <- tempfile("holzinger_", fileext = ".siever.rds")
#' sieve_prepare_study(d, spec, file = path, name = "Holzinger and Swineford")
#' paths <- sieve_session_write(sieve_project_read(path), tempfile("siever_session_"))
#' # The script names the input file's digest, then each recorded revision.
#' script <- readLines(paths[["script"]])
#' keys <- regmatches(script, regexpr("[0-9a-f]{32}", script))
#' store <- sieve_replay_read(paths[["inputs"]], keys[1])
#' names(store)
#' @export
sieve_replay_read <- function(file, hash) {
  bytes <- readBin(file, "raw", n = file.size(file))
  if (!identical(sieve_bytes_digest(bytes), hash)) {
    stop("The session input file does not match its recorded hash.", call. = FALSE)
  }
  # Written compressed (zlib, see sieve_replay_bytes()); an uncompressed
  # file is the plain serialization, which begins with "X" or "A".
  if (length(bytes) >= 2L && (bytes[1] == as.raw(0x78) || identical(bytes[1:2], as.raw(c(0x1f, 0x8b))))) {
    bytes <- memDecompress(bytes, type = "gzip")
  }
  # Held in a list and checked for plain data before any field of it is
  # read (sieve_plain_unserialized() in fct_project.R).
  read <- sieve_capture(list(unserialize(bytes)))
  if (!read$ok) {
    stop(sprintf(
      "The session input file could not be read (%s), although its hash matches; it is not a SIEVER input store.",
      read$error
    ), call. = FALSE)
  }
  bad <- sieve_plain_unserialized(read$value, "store")
  if (length(bad)) {
    stop(sprintf(
      paste(
        "The session input file holds something that is not plain data (%s), so it was not read.",
        "Session input files hold only data; nothing in the file was run."
      ),
      sieve_plain_problems_text(bad)
    ), call. = FALSE)
  }
  store <- .subset2(read$value, 1L)
  if (!is.list(store) || !identical(store$format, "SIEVER replay inputs") ||
    !identical(store$version, 1L) || length(sieve_not_plain(store))) {
    stop("The session input file is not a plain SIEVER input store.", call. = FALSE)
  }
  store
}

#' Bind a Recorded Revision in a Session Script
#'
#' Rebuilds the data and item views from one verified input revision. It
#' does not fit a model or execute text from the input file.
#' @param store Input store from [sieve_replay_read()].
#' @param key Recorded revision key.
#' @param envir Environment receiving the named script objects.
#' @return The plain input revision invisibly.
#' @examples
#' d <- DMAR::holzinger_swineford[, c(
#'   "id", "school", "t1_visual_perception", "t2_cubes", "t4_lozenges"
#' )]
#' spec <- sieve_specification_template(d)
#' spec$construct[spec$role == "item"] <- "Visual"
#' path <- tempfile("holzinger_", fileext = ".siever.rds")
#' sieve_prepare_study(d, spec, file = path, name = "Holzinger and Swineford")
#' paths <- sieve_session_write(sieve_project_read(path), tempfile("siever_session_"))
#' script <- readLines(paths[["script"]])
#' keys <- regmatches(script, regexpr("[0-9a-f]{32}", script))
#' store <- sieve_replay_read(paths[["inputs"]], keys[1])
#' bound <- new.env()
#' sieve_replay_bind(store, keys[2], envir = bound)
#' dim(bound$items)
#' @export
sieve_replay_bind <- function(store, key, envir = parent.frame()) {
  record <- store$revisions[[key]]
  if (is.null(record) || !identical(sieve_hash(record), key)) {
    stop("The replay revision is absent or does not match its recorded hash.", call. = FALSE)
  }
  x <- sieve_replay_expand(record, store$data)
  meta <- x$meta
  active <- meta$item[meta$status == "active"]
  cons <- meta$construct[match(active, meta$item)]
  groups <- split(active, factor(cons, levels = unique(cons)))
  groups <- groups[lengths(groups) > 0L]
  big <- which(lengths(groups) >= 3L)
  chosen <- if (length(groups)) groups[[if (length(big)) big[1L] else 1L]] else character(0)
  inv <- x$raw[, c(active, setdiff(names(x$raw), meta$item)), drop = FALSE]
  inv[active] <- x$items[, active, drop = FALSE]
  index <- x$case_index %||% seq_len(nrow(x$raw))
  views <- list(
    d = x$raw, items = x$items, meta = meta, item_cols = meta$item,
    active_item_names = active, active_items = x$items[, active, drop = FALSE],
    X = x$items[, active, drop = FALSE], items_data = x$items,
    construct_of = cons, by_construct = groups, construct_items = chosen,
    items_df = x$items[, chosen, drop = FALSE], inv_data = inv,
    case_index = as.integer(index), sampling_design = x$sampling_design,
    screening_spec = x$screening_spec, scoring_policies = x$scoring_policies,
    values_as_read = as.list(x$raw[intersect(names(x$items), names(x$raw))])
  )
  removed <- list(case_index = integer(0))
  if (length(x$removed_cases)) {
    held <- x$removed_cases
    held <- held[order(vapply(held, function(r) as.integer(r$row), integer(1)))]
    removed$case_index <- vapply(held, function(r) as.integer(r$row), integer(1))
    raw <- Reduce(sieve_cases_bind, lapply(held, `[[`, "raw"))
    its <- Reduce(sieve_cases_bind, lapply(held, `[[`, "items"))
    for (nm in setdiff(names(views), c("meta", "case_index", "values_as_read"))) {
      v <- views[[nm]]
      if (!is.data.frame(v)) next
      src <- if (identical(nm, "d")) raw else its
      miss <- setdiff(names(v), names(src))
      src[miss] <- raw[miss]
      removed[[nm]] <- src[, names(v), drop = FALSE]
    }
  }
  views$removed_cases <- removed
  # The validation reservations recorded with this revision (fct_holdout.R).
  views$holdout <- x$holdout %||% list()
  list2env(views, envir = envir)
  invisible(x)
}

#' Verify the Result of a Recorded Session Operation
#'
#' @description
#' Checks, after one recorded session step has been replayed, that the
#' objects in `envir` match what the session held after that step: the
#' revision key of the item ledger and, on request, the analysis-relevant
#' item metadata and the loaded data with its original respondent rows. It
#' returns quietly on a match and stops with what differs, so a replayed
#' script cannot drift unnoticed from the session it records.
#'
#' @param store Input store from [sieve_replay_read()].
#' @param key Expected post-operation revision key.
#' @param envir Environment containing the replayed objects.
#' @param metadata Whether to check analysis-relevant item metadata too.
#' @param raw Whether to check the loaded data and original respondent rows.
#' @return `TRUE` invisibly; stops with a reason on a mismatch.
#' @examples
#' d <- DMAR::holzinger_swineford[, c(
#'   "id", "school", "t1_visual_perception", "t2_cubes", "t4_lozenges"
#' )]
#' spec <- sieve_specification_template(d)
#' spec$construct[spec$role == "item"] <- "Visual"
#' path <- tempfile("holzinger_", fileext = ".siever.rds")
#' sieve_prepare_study(d, spec, file = path, name = "Holzinger and Swineford")
#' paths <- sieve_session_write(sieve_project_read(path), tempfile("siever_session_"))
#' script <- readLines(paths[["script"]])
#' keys <- regmatches(script, regexpr("[0-9a-f]{32}", script))
#' store <- sieve_replay_read(paths[["inputs"]], keys[1])
#' bound <- new.env()
#' sieve_replay_bind(store, keys[2], envir = bound)
#' sieve_replay_verify(store, keys[2], envir = bound)
#' # A changed value no longer matches the recorded revision.
#' bound$items[1, 1] <- bound$items[1, 1] + 1
#' try(sieve_replay_verify(store, keys[2], envir = bound))
#' @export
sieve_replay_verify <- function(store, key, envir = parent.frame(), metadata = TRUE, raw = FALSE) {
  record <- store$revisions[[key]]
  if (is.null(record) || !identical(sieve_hash(record), key)) {
    stop("The replay revision is absent or does not match its recorded hash.", call. = FALSE)
  }
  x <- sieve_replay_expand(record, store$data)
  same <- function(name, value) exists(name, envir = envir, inherits = FALSE) &&
    identical(sieve_data_key(get(name, envir = envir)), sieve_data_key(value))
  if (isTRUE(raw) && !same("d", x$raw)) {
    stop("The replayed source data do not match this step's recorded respondent rows.", call. = FALSE)
  }
  if (!same("items", x$items)) stop("The replayed item data do not match this step's recorded revision.", call. = FALSE)
  if (isTRUE(metadata) && (!exists("meta", envir = envir, inherits = FALSE) ||
    !identical(sieve_meta_key(get("meta", envir = envir)), sieve_meta_key(x$meta)))) {
    stop("The replayed item ledger does not match this step's recorded revision.", call. = FALSE)
  }
  invisible(TRUE)
}

#' The bytes of a session input file
#'
#' The serialized store, compressed (zlib, which `memDecompress(type =
#' "gzip")` reads), so the file a session archive carries is a fraction of
#' the size of the plain serialization. The bytes are
#' the same for the same store, so the digest the displayed script names is
#' the digest of the file the download writes.
#'
#' @param store A store from `sieve_replay_bundle()`.
#' @return A raw vector.
#' @author Ken Kelley
#' @noRd
sieve_replay_bytes <- function(store) {
  memCompress(serialize(store, NULL, version = 3L), type = "gzip")
}

#' Input store used by a session script
#' @param state Shared state.
#' @return Plain store.
#' @author Ken Kelley
#' @noRd
sieve_replay_bundle <- function(state) {
  shiny::isolate({
    builds <- list()
    steps <- list()
    for (step in names(state$replay_operations %||% list())) {
      identity <- state$replay_operations[[step]]$engine_identity
      if (is.null(identity)) next
      key <- sieve_hash(sieve_engine_keys(identity))
      if (is.null(builds[[key]])) builds[[key]] <- identity
      steps[[step]] <- key
    }
    list(format = "SIEVER replay inputs", version = 1L,
      data = state$replay_data %||% list(), revisions = state$replay_states %||% list(),
      versions = state$replay_versions %||% character(0),
      engine_identity = builds, engine_steps = steps)
  })
}

#' A readable session script with hash-bound inputs
#' @param state Shared state.
#' @param data_file Optional filename of a separate input store. Without
#'   one, a self-contained inspection script binds the store once.
#' @param data_hash Digest of that file.
#' @return One character string of R source.
#' @author Ken Kelley
#' @noRd
sieve_replay_script <- function(state, data_file = NULL, data_hash = NULL) {
  shiny::isolate({
    log <- state$codelog %||% character(0)
    records <- sieve_script_records(state)
    out <- c(
      "## SIEVER session script", "## Inputs are bound to their recorded content hashes.",
      "## A theory-specified assignment is not evidence of independent validation.",
      "## The companion input file contains research data; apply the study's sharing rules.", ""
    )
    if (!length(records)) return(paste(c(out, log), collapse = "\n"))
    store <- sieve_replay_bundle(state)
    if (length(store$versions)) {
      out <- c(out, paste0("## Recorded with ", paste(names(store$versions), store$versions, collapse = "; ")))
    }
    # A study kept at MLR with FIML when it was opened (its file records
    # MLR with FIML for continuous items under Follow the item types, see
    # sieve_keep_legacy_mlr()): its SIEVER calls with estimator = "auto"
    # were fit under MLR, not the ML with FIML that "auto" gives for
    # continuous items.
    if (any(grepl(sieve_legacy_auto_note(), log, fixed = TRUE))) {
      out <- c(
        out,
        "## Steps recorded before the study was kept at MLR with FIML (see that step below) fit continuous",
        "## items under MLR with FIML in each SIEVER call with estimator = \"auto\", which gives them ML with",
        "## FIML (DMAR's default). Pass estimator = \"mlr\" in those calls to rerun them as fit."
      )
    }
    out <- c(
      out,
      "if (utils::packageVersion('SIEVER') < '0.9.0') stop('This session script requires SIEVER 0.9.0 or later.', call. = FALSE)",
      ""
    )
    if (is.null(data_file)) {
      # R source literals do not preserve every original string encoding
      # or attribute. This optional self-contained inspection copy uses one
      # compressed serialization; the downloaded script uses the sidecar.
      bytes <- memCompress(serialize(store, NULL, version = 3L), type = "gzip")
      literal <- utils::capture.output(dput(bytes))
      out <- c(
        out, "# One input store for this self-contained inspection copy.",
        paste0(
          ".siever_store <- unserialize(memDecompress(", paste(literal, collapse = "\n"),
          ", type = 'gzip'))"
        )
      )
    } else {
      if (is.null(data_hash)) data_hash <- sieve_bytes_digest(sieve_replay_bytes(store))
      out <- c(out, sprintf(
        ".siever_store <- SIEVER::sieve_replay_read(%s, %s)",
        encodeString(data_file, quote = '"'), encodeString(data_hash, quote = '"')
      ))
    }
    if (!is.null(state$replay_baseline)) {
      out <- c(
        out, "# The checked current state of the opened project; historical analyses are not rerun.",
        sprintf("SIEVER::sieve_replay_bind(.siever_store, %s)", encodeString(state$replay_baseline, quote = '"')), ""
      )
    }
    for (record in records) {
      number <- record$number
      block <- c(sprintf("# [%s] %s", number, record$note), record$code, "")
      op <- state$replay_operations[[number]]
      if (is.null(op) || isTRUE(op$legacy) || is.null(op$before)) {
        # A step recorded without its pre-operation inputs is kept from the
        # log as a record, without claiming that it is a replay.
        out <- c(
          out, "# Historical step: recorded without verifiable input revisions, so it is not replayed.",
          sieve_comment_lines("Original record:", paste(block, collapse = "\n")), ""
        )
        next
      }
      out <- c(out, block[1L], sprintf(
        "SIEVER::sieve_replay_engine_check(.siever_store, %s)", encodeString(number, quote = '"')
      ), sprintf(
        "SIEVER::sieve_replay_bind(.siever_store, %s)",
        encodeString(op$before, quote = '"')
      ))
      if (identical(op$kind, "import")) {
        out <- c(
          out, "# The original import is retained below; the verified source replaces its local path.",
          sieve_comment_lines("Original import:", paste(block[-1L], collapse = "\n"))
        )
      } else {
        out <- c(out, block[-1L])
      }
      out <- c(out, sprintf(
        "SIEVER::sieve_replay_verify(.siever_store, %s, raw = %s)",
        encodeString(op$after, quote = '"'), if (identical(op$kind, "mutation")) "TRUE" else "FALSE"
      ))
      if (identical(op$kind, "mutation")) {
        out <- c(
          out, "# Refresh descriptive summaries and named views after the verified operation.",
          sprintf("SIEVER::sieve_replay_bind(.siever_store, %s)", encodeString(op$after, quote = '"'))
        )
      }
      out <- c(out, "")
    }
    paste(out, collapse = "\n")
  })
}

#' Write a Runnable SIEVER Session and Its Verified Input File
#'
#' The script contains the recorded expressions in their recorded order.
#' Source data are stored once in the companion file and referenced by
#' hash. Mutations run from their pre-operation input and are checked
#' against their recorded output. Steps recorded without verifiable
#' inputs remain clearly labeled historical comments.
#' @param state A shared SIEVER state or a checked project snapshot.
#' @param directory Destination directory, created when needed.
#' @return Invisibly, the paths of the script and input file.
#' @examples
#' d <- DMAR::holzinger_swineford[, c(
#'   "id", "school", "t1_visual_perception", "t2_cubes", "t4_lozenges"
#' )]
#' spec <- sieve_specification_template(d)
#' spec$construct[spec$role == "item"] <- "Visual"
#' path <- tempfile("holzinger_", fileext = ".siever.rds")
#' sieve_prepare_study(d, spec, file = path, name = "Holzinger and Swineford")
#' project <- sieve_project_read(path)
#' paths <- sieve_session_write(project, tempfile("siever_session_"))
#' basename(paths)
#' head(readLines(paths[["script"]]), 12)
#' @export
sieve_session_write <- function(state, directory) {
  if (!inherits(state, "reactivevalues") && is.list(state) && identical(state$format, "SIEVER project")) {
    restored <- sieve_new_state()
    sieve_project_restore(restored, state)
    state <- restored
  }
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(directory, "siever_inputs.rds")
  store <- sieve_replay_bundle(state)
  bytes <- sieve_replay_bytes(store)
  writeBin(bytes, file)
  script <- file.path(directory, "siever_session.R")
  writeLines(sieve_replay_script(state, "siever_inputs.rds", sieve_bytes_digest(bytes)), script)
  invisible(c(script = script, inputs = file))
}

#' Compare Recorded and Executing Engines Before Replaying a Step
#'
#' A different compatible engine may be used deliberately. A mismatch is
#' reported before execution; it does not rewrite the historical record.
#' @param store A verified replay input store.
#' @param step Recorded step number.
#' @return TRUE when recorded identities agree, FALSE otherwise, invisibly.
#' @author Ken Kelley
#' @examples
#' # A historical store with no recorded engine cannot verify its build.
#' historical <- list(engine_steps = list(), engine_identity = list())
#' verified <- sieve_replay_engine_check(historical, step = 1L)
#' verified
#' @export
sieve_replay_engine_check <- function(store, step) {
  key <- store$engine_steps[[as.character(step)]]
  recorded <- if (!is.null(key)) store$engine_identity[[key]] else NULL
  if (is.null(recorded)) return(invisible(FALSE))
  known <- recorded[vapply(recorded, function(engine) !is.null(engine$id), logical(1))]
  if (!length(known)) return(invisible(FALSE))
  # Engines that have not been needed yet are loaded only when a recorded
  # computation used them; this makes the comparison about executing code.
  # lavaan.mi is loaded with lavaan attached as it loads, or its methods
  # would not dispatch in this session (see sieve_mi_model()).
  for (package in names(known)) {
    if (identical(package, "lavaan.mi")) sieve_load_lavaan_mi() else requireNamespace(package, quietly = TRUE)
  }
  current <- sieve_engine_identity(names(known))
  different <- names(known)[sieve_engine_keys(known) != sieve_engine_keys(current)]
  if (length(different)) warning(structure(list(message = paste0(
    "Replay step ", step, " was recorded under different engine builds: ",
    paste(different, collapse = ", "), ". The original identity is retained; recomputed results may differ."),
    call = NULL, step = step, packages = different),
    class = c("sieve_replay_engine_mismatch", "warning", "condition")))
  invisible(!length(different))
}
