# Compatibility is a capability contract: the interface SIEVER calls, not
# one build.
.siever_engine_identity_cache <- new.env(parent = emptyenv())

#' Refuse an incompatible engine table before it becomes a misleading result
#' @param result An engine result.
#' @param columns Required column names.
#' @param context The analysis/function whose boundary is being checked.
#' @return The unchanged result invisibly.
#' @author Ken Kelley
#' @noRd
sieve_dmar_require_columns <- function(result, columns, context = "DMAR result") {
  missing <- setdiff(columns, names(result))
  if (!is.data.frame(result) || length(missing)) {
    why <- if (!is.data.frame(result)) "did not return a data frame" else
      paste("is missing required columns", paste(missing, collapse = ", "))
    stop(structure(list(message = paste0(context, " ", why,
      ". Load a compatible DMAR >= 1.1.0 build, restart R, and rerun the analysis."),
      call = NULL, context = context, missing = missing),
      class = c("sieve_dmar_schema_error", "error", "condition")))
  }
  invisible(result)
}

#' Check required rows at a DMAR term/value boundary
#' @param result Engine table.
#' @param terms Required terms, each occurring exactly once.
#' @param context Analysis/function label.
#' @return The unchanged result invisibly.
#' @author Ken Kelley
#' @noRd
sieve_dmar_require_terms <- function(result, terms, context = "DMAR result") {
  sieve_dmar_require_columns(result, c("term", "value"), context)
  counts <- vapply(terms, function(term) sum(result$term == term, na.rm = TRUE), integer(1))
  if (any(counts != 1L)) {
    stop(structure(list(message = paste0(context, " must contain each required term exactly once: ",
      paste(terms[counts != 1L], collapse = ", "),
      ". Load a compatible DMAR >= 1.1.0 build, restart R, and rerun the analysis."),
      call = NULL, context = context, terms = terms[counts != 1L]),
      class = c("sieve_dmar_schema_error", "error", "condition")))
  }
  invisible(result)
}

#' Check the supported DMAR capabilities and inexpensive output schemas
#' @param namespace Optional namespace for controlled compatibility checks.
#' @param refresh Repeat the check for the same loaded namespace.
#' @return TRUE invisibly; an actionable error for an incompatible build.
#' @author Ken Kelley
#' @noRd
sieve_dmar_contract <- function(namespace = NULL, refresh = FALSE) {
  ns <- namespace %||% asNamespace("DMAR")
  held <- .siever_engine_identity_cache$contract
  if (!isTRUE(refresh) && !is.null(held) && identical(held, ns)) return(invisible(TRUE))
  required <- list(
    ss_aipe_reliability = c("n_items", "true_item_covariance", "true_factor_loading", "true_residual_var"),
    ss_aipe_rmsea = "true_rmsea", sem_fit_measures = "n_groups",
    irt_information = c("trait_value", "discrimination", "threshold"),
    reliability_omega = c("ci_method", "denominator", "conf_level", "B", "seed"),
    parallel_analysis = c("B", "seed"), ci_bifactor_indices = c("B", "seed")
  )
  problems <- character()
  if (isNamespace(ns)) {
    calls <- unique(c(names(required), "ci_proportion", "average_variance_extracted", "bifactor_indices",
      "cfa_1", "common_method_marker", "common_method_single_factor", "content_validity_index", "cov_fiml",
      "dmacs", "ecvi", "fa_confirmatory", "fa_exploratory", "factor_scores", "fraction_missing_information",
      "htmt", "irt_grm", "kurtosis", "measurement_alignment", "measurement_invariance", "plot_irt_information",
      "reliability_H", "reliability_alpha", "reliability_omega_categorical", "sem_exact_fit_bootstrap",
      "simple_structure", "skewness", "factor_count", "missing_patterns", "missing_coverage"))
    missing_exports <- setdiff(calls, getNamespaceExports(ns))
    if (length(missing_exports)) problems <- paste("Missing DMAR exports:", paste(missing_exports, collapse = ", "))
  }
  if (isNamespace(ns) && utils::compareVersion(as.character(getNamespaceVersion(ns)), "1.1.0") < 0) {
    problems <- "DMAR >= 1.1.0 is required"
  }
  for (name in names(required)) {
    fun <- get0(name, envir = ns, inherits = FALSE)
    missing <- if (is.function(fun)) setdiff(required[[name]], names(formals(fun))) else required[[name]]
    if (length(missing)) problems <- c(problems, paste0(name, " requires ", paste(missing, collapse = ", ")))
  }
  if (!length(problems)) {
    checked <- tryCatch({
      proportion <- get("ci_proportion", ns)(successes = 50L, N = 100L)
      sieve_dmar_require_terms(proportion, c("ci_lower", "proportion", "ci_upper"), "DMAR::ci_proportion()")
      info <- get("irt_information", ns)(discrimination = 1, threshold = 0, trait_value = c(-1, 0, 1))
      sieve_dmar_require_columns(info, c("trait_value", "test_information", "se"), "DMAR::irt_information()")
      if (!all(c("discrimination", "trait_value_max_information") %in% names(attributes(info)))) {
        stop("irt_information() is missing the discrimination/trait_value_max_information attributes", call. = FALSE)
      }
      NULL
    }, error = function(e) conditionMessage(e))
    problems <- c(problems, checked)
  }
  if (length(problems)) {
    stop(structure(list(message = paste0("This DMAR build does not provide the interface SIEVER requires: ",
      paste(problems, collapse = "; "), ". Install a current compatible DMAR >= 1.1.0 build and restart R. ",
      "The version number alone does not establish compatibility."),
      call = NULL, problems = problems), class = c("sieve_dmar_compatibility_error", "error", "condition")))
  }
  .siever_engine_identity_cache$contract <- ns
  invisible(TRUE)
}

#' Hash canonical identity content with SHA-256
#'
#' The content is hashed as bytes. A serialized R object records the version
#' of R that wrote it, so hashing one would change the digest with every R
#' release, whatever the content.
#' @param x A character vector, hashed as UTF-8 lines, or a raw vector.
#' @return A digest string.
#' @author Ken Kelley
#' @noRd
sieve_identity_digest <- function(x) {
  bytes <- if (is.raw(x)) x else charToRaw(paste(enc2utf8(as.character(x)), collapse = "\n"))
  file <- tempfile("siever_identity_")
  on.exit(unlink(file), add = TRUE)
  writeBin(bytes, file)
  sieve_sha256_file(file)
}

#' Whether an object is plain data, with no code or environment inside
#' @param x Any R object.
#' @return TRUE for atomic data and lists of atomic data, attributes included.
#' @author Ken Kelley
#' @noRd
sieve_identity_plain <- function(x) {
  if (is.null(x)) return(TRUE)
  if (isS4(x) || is.environment(x)) return(FALSE)
  if (!is.atomic(x) && !(is.list(x) && !is.function(x))) return(FALSE)
  attrs <- attributes(x)
  if (length(attrs) && !all(vapply(attrs, sieve_identity_plain, logical(1)))) return(FALSE)
  if (is.atomic(x)) return(TRUE)
  all(vapply(x, sieve_identity_plain, logical(1)))
}

#' The code and constants of a loaded namespace, as canonical bytes
#'
#' Every function contributes its name, arguments and body, and every plain
#' data object (a tolerance, a quadrature rule, a table of terms) its name
#' and value. Names are taken in an order that does not depend on the locale.
#' The bytes do not depend on where or when the package was installed, so
#' two installations of the same source give the same content. Compiled code,
#' datasets and objects that hold environments are not part of it; the
#' installed files are hashed separately as the runtime manifest.
#' @param ns A loaded namespace.
#' @return A raw vector.
#' @author Ken Kelley
#' @noRd
sieve_namespace_content <- function(ns) {
  objects <- sort(ls(ns, all.names = TRUE), method = "radix")
  objects <- objects[!startsWith(objects, ".__") & objects != ".packageName"]
  parts <- lapply(objects, function(name) {
    object <- get(name, ns, inherits = FALSE)
    text <- function(...) charToRaw(enc2utf8(paste0(paste(c(...), collapse = "\n"), "\n")))
    if (is.function(object)) {
      return(text(paste("function", name), deparse(formals(object), width.cutoff = 500L),
        deparse(body(object), width.cutoff = 500L)))
    }
    if (sieve_identity_plain(object)) {
      # The first 14 bytes of a version 2 serialization are its header, which
      # names the R version that wrote it; the value follows.
      return(c(text(paste("data", name)), serialize(object, NULL, xdr = TRUE, version = 2L)[-seq_len(14L)],
        charToRaw("\n")))
    }
    text(paste("object", name, paste(class(object), collapse = "/")))
  })
  unlist(parts, use.names = FALSE) %||% raw(0)
}

#' The key under which two engine records are the same engine
#'
#' The package, its version, and the code and constants of its namespace.
#' The installed files, the build date and the installation path are kept in
#' the record but are not part of the key: every installation rewrites them,
#' so a key that included them would call a reinstalled, unchanged engine a
#' different build.
#' @param identity One engine identity record.
#' @return A digest string.
#' @author Ken Kelley
#' @noRd
sieve_identity_id <- function(identity) {
  sieve_identity_digest(c("SIEVER engine identity, scheme 2", identity$package, identity$version,
    identity$namespace_sha256))
}

#' Identify the namespace actually executing an analysis
#' @param package Package name.
#' @return A plain record. The ID is that of the package's code (see
#'   `sieve_identity_id()`); the hash of the installed files, the build date
#'   and the installation path describe this installation and are recorded
#'   beside it.
#' @author Ken Kelley
#' @noRd
sieve_package_identity <- function(package) {
  if (!isNamespaceLoaded(package)) return(list(package = package, status = "not loaded"))
  ns <- asNamespace(package)
  held <- .siever_engine_identity_cache[[package]]
  if (!is.null(held) && identical(held$namespace, ns)) return(held$identity)
  path <- normalizePath(getNamespaceInfo(ns, "path"), mustWork = FALSE)
  description <- tryCatch(read.dcf(file.path(path, "DESCRIPTION")), error = function(e) NULL)
  metadata <- function(name) if (!is.null(description) && name %in% colnames(description)) unname(description[1L, name]) else NA_character_
  files <- unique(c(file.path(path, c("DESCRIPTION", "NAMESPACE")),
    unlist(lapply(c("R", "libs", "data"), function(root) list.files(file.path(path, root),
      recursive = TRUE, full.names = TRUE, all.files = FALSE)), use.names = FALSE)))
  files <- sort(files[file.exists(files) & !dir.exists(files)], method = "radix")
  manifest <- paste(substring(files, nchar(path) + 2L), vapply(files, sieve_sha256_file, character(1)), sep = "\t")
  identity <- list(package = package, status = "loaded", version = as.character(getNamespaceVersion(package)),
    namespace_sha256 = sieve_identity_digest(sieve_namespace_content(ns)),
    runtime_sha256 = sieve_identity_digest(manifest),
    remote_sha = metadata("RemoteSha"), packaged = metadata("Packaged"), namespace_path = path)
  identity$id <- sieve_identity_id(identity)
  .siever_engine_identity_cache[[package]] <- list(namespace = ns, identity = identity)
  identity
}

#' The installation a loaded package came from, as the session first saw it
#'
#' Read when SIEVER loads (for DMAR and SIEVER) or at the first request, and
#' kept for the life of the namespace. A background worker compares it with
#' the files it finds at the same path (`sieve_worker_bootstrap()`), so a
#' package reinstalled while the session runs is refused there instead of
#' computing part of an analysis on a different build.
#' @param package A loaded package.
#' @return The stamp, or `NA` for a package loaded from source.
#' @author Ken Kelley
#' @noRd
sieve_loaded_stamp <- function(package) {
  ns <- asNamespace(package)
  held <- .siever_engine_identity_cache$stamps[[package]]
  if (!is.null(held) && identical(held$namespace, ns)) return(held$stamp)
  path <- getNamespaceInfo(ns, "path")
  stamp <- unname(sieve_worker_bootstrap(stats::setNames(list(path), package), stamp_only = TRUE))
  .siever_engine_identity_cache$stamps[[package]] <- list(namespace = ns, stamp = stamp)
  stamp
}

#' Identity of the loaded engines, frozen in each scientific record
#' @param packages The packages whose executing namespaces are recorded.
#' @return Named plain identity records.
#' @author Ken Kelley
#' @noRd
sieve_engine_identity <- function(packages = c("SIEVER", "DMAR", "lavaan", "psych")) {
  stats::setNames(lapply(packages, sieve_package_identity), packages)
}

# The check is small and deterministic: it fits no model and draws no data.
# The stamps record which installations this session loaded. Each is the
# Built field and a checksum of the package's lazy-load database (a few
# megabytes, a few hundredths of a second for the two), and reading them
# must never keep the package from loading.
.onLoad <- function(libname, pkgname) {
  sieve_dmar_contract()
  for (package in c("DMAR", pkgname)) try(sieve_loaded_stamp(package), silent = TRUE)
}

#' Compact immutable engine keys, excluding machine-specific paths
#' @param identity Named engine identity records.
#' @return Named character keys.
#' @author Ken Kelley
#' @noRd
sieve_engine_keys <- function(identity) {
  vapply(identity, function(engine) engine$id %||% paste0("unknown:", engine$status %||% "not recorded"), character(1))
}

#' Engines a recorded application step may have used
#'
#' The identities of the namespaces loaded in this R process. A step
#' computed in a background R process passes the identities that process
#' recorded for the engines it loaded (the ordered-item MI analysis records
#' mice and lavaan.mi there, `sieve_mi_worker()`, because neither is loaded
#' in the Shiny process), and they replace this process's records of the
#' same packages, which would otherwise say "not loaded" with no ID and so
#' leave those engines out of the replay check.
#' @param executed `NULL`, or named identity records of engines that ran in
#'   another R process.
#' @return Named immutable engine identities.
#' @author Ken Kelley
#' @noRd
sieve_recording_engine_identity <- function(executed = NULL) {
  identity <- sieve_engine_identity(c("SIEVER", "DMAR", "lavaan", "psych",
    "psychonetrics", "qgraph", "igraph", "mice", "lavaan.mi"))
  if (length(executed)) identity[names(executed)] <- executed
  identity
}
