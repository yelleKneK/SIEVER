#' Resolve the declared unit of sampling
#' @param state Shared state.
#' @return A plain named design list.
#' @author Ken Kelley
#' @noRd
sieve_sampling_spec <- function(state) {
  x <- shiny::isolate(state$sampling_design) %||% list()
  unit <- x$unit %||% "independent"
  if (!unit %in% c("independent", "clustered", "repeated")) stop("Choose a supported sampling unit.", call. = FALSE)
  list(unit = unit, cluster = x$cluster %||% NULL)
}

#' The sampling design in words
#'
#' @param design A design list (`sieve_sampling_spec()`), with `unit` and
#'   `cluster`.
#' @return One phrase, such as `"clustered rows, clusters given by school"`.
#' @author Ken Kelley
#' @noRd
sieve_sampling_text <- function(design) {
  unit <- design$unit %||% "independent"
  cluster <- design$cluster %||% ""
  base <- switch(unit,
    clustered = "clustered rows",
    repeated = "repeated observations",
    "independent rows"
  )
  if (!identical(unit, "independent") && length(cluster) == 1L && !is.na(cluster) && nzchar(cluster)) {
    base <- paste0(base, sprintf(", %s given by %s", if (identical(unit, "repeated")) "respondents" else "clusters", cluster))
  } else if (!identical(unit, "independent")) {
    base <- paste0(base, ", with no cluster or respondent column chosen yet")
  }
  base
}

#' State when an independent-observation method is unsupported
#' @param state Shared state.
#' @param capability Independent, cluster, or longitudinal.
#' @return NULL or an actionable precondition.
#' @author Ken Kelley
#' @noRd
sieve_sampling_problem <- function(state, capability = "independent") {
  design <- sieve_sampling_spec(state)
  if (identical(design$unit, "independent")) return(NULL)
  if (identical(capability, "cluster") && identical(design$unit, "clustered")) return(NULL)
  if (identical(capability, "longitudinal") && identical(design$unit, "repeated")) return(NULL)
  sprintf(
    "The declared sampling unit is %s. This procedure assumes independent rows and does not supply design-correct uncertainty for that declaration. Use a supported cluster or longitudinal model; no ordinary row-based interval or bootstrap was run.",
    design$unit
  )
}

#' Apply a supported cluster design to a continuous lavaan specification
#' @param spec Resolved measurement or structural model.
#' @param state Shared state, including aligned raw/item rows.
#' @param data Analysis data before case filtering.
#' @return Data, exact spec, row flow, and any refusal reason.
#' @author Ken Kelley
#' @noRd
sieve_sampling_apply <- function(spec, state, data) {
  prepared <- sieve_auxiliary_apply(spec, state, data)
  spec <- prepared$spec
  data <- prepared$data
  design <- sieve_sampling_spec(state)
  out <- list(
    spec = spec, data = data, row_index = seq_len(nrow(data)), reason = prepared$reason,
    warnings = prepared$warnings, notes = prepared$notes,
    row_flow = list(
      input = nrow(data), eligible = nrow(data), cluster_missing = 0L,
      clusters = NA_integer_, unit = design$unit
    )
  )
  if (identical(design$unit, "independent")) return(out)
  if (identical(design$unit, "repeated")) {
    out$reason <- "Repeated observations require an explicitly specified longitudinal model. Use the longitudinal mapping on CV and Invariance; an ordinary independent-row CFA/SEM was not fitted."
    return(out)
  }
  if (length(spec$ordered %||% character()) || !spec$estimator %in% c("ML", "MLR")) {
    out$reason <- "This cluster-robust path supports continuous ML/MLR models. Categorical cluster inference is not supplied by this path."
    return(out)
  }
  column <- design$cluster
  raw <- shiny::isolate(state$raw)
  if (length(column) != 1L || is.na(column) || !nzchar(column) || !column %in% names(raw)) {
    out$reason <- "Choose the column identifying independent clusters before fitting this model."
    return(out)
  }
  if (nrow(data) != nrow(raw)) {
    out$reason <- "The analysis rows cannot be aligned to cluster identifiers. Supply the complete aligned analysis frame before filtering."
    return(out)
  }
  data[[column]] <- raw[[column]]
  ok <- !is.na(data[[column]])
  out$row_flow$cluster_missing <- sum(!ok)
  data <- data[ok, , drop = FALSE]
  out$row_index <- which(ok)
  clusters <- length(unique(data[[column]]))
  out$row_flow$eligible <- nrow(data)
  out$row_flow$clusters <- clusters
  if (clusters < 2L) {
    out$reason <- "Cluster-robust inference needs more than one observed cluster."
    return(out)
  }
  if (clusters < 50L) {
    out$warnings <- c(out$warnings, sprintf(
      "Only %d independent clusters: ordinary cluster-robust inference relies on many clusters and can be inaccurate with fewer than about 30 to 50. This path supplies no small-cluster correction (Cameron & Miller, 2015).",
      clusters
    ))
  }
  # MLR's point estimator is ML; cluster tells lavaan which robust covariance
  # to use. The entire resolved call is retained and replayed.
  spec$estimator <- "MLR"
  spec <- sieve_spec_engine_args(spec, list(cluster = column))
  spec$sampling_design <- c(
    design,
    list(
      clusters = clusters,
      cluster_missing = sum(!ok),
      values_key = sieve_hash(raw[[column]])
    )
  )
  spec$key <- NULL
  spec$key <- sieve_hash(spec)
  out$spec <- spec
  out$data <- data
  out
}

#' Check the rank available to a cluster sandwich covariance
#' @param fit A fitted or initialized lavaan model.
#' @return Cluster count, effective parameter count, and a refusal or caution.
#' @author Ken Kelley
#' @noRd
sieve_cluster_inference_check <- function(fit) {
  out <- list(clusters = NA_integer_, parameters = NA_integer_, reason = NULL, caution = NULL)
  if (!inherits(fit, "lavaan")) return(out)
  options <- lavaan::lavInspect(fit, "options")
  if (!grepl("cluster", options$se %||% "", fixed = TRUE)) return(out)
  # lavaan's public nclusters/cluster.idx inspectors (0.7-3, the version
  # SIEVER requires) return the single-level placeholder for robust.cluster
  # fits. The data object's Lp retains the actual grouping partition.
  # Verify that structure here and refuse if it is unavailable rather than
  # reporting a fictitious G = 1.
  partitions <- tryCatch(fit@Data@Lp, error = function(e) NULL)
  counts <- vapply(partitions, function(x) {
    if (length(x$nclusters) >= 2L) as.integer(x$nclusters[[2L]]) else NA_integer_
  }, integer(1))
  if (!length(counts) || anyNA(counts)) {
    out$reason <- "The independent cluster partition could not be verified; cluster-robust inference is unavailable."
    return(out)
  }
  G <- sum(counts)
  pt <- lavaan::parTable(fit)
  # Simple equality constraints remove parameter directions. This is the
  # same parameter space used by the information-matrix rank check.
  p <- lavaan::lavInspect(fit, "npar")
  equal <- pt[pt$op == "==", , drop = FALSE]
  if (nrow(equal)) {
    left <- pt$free[match(equal$lhs, pt$plabel)]
    right <- pt$free[match(equal$rhs, pt$plabel)]
    if (!anyNA(c(left, right)) && all(c(left, right) > 0L)) {
      J <- matrix(0, nrow(equal), p)
      J[cbind(seq_len(nrow(J)), left)] <- 1
      J[cbind(seq_len(nrow(J)), right)] <- J[cbind(seq_len(nrow(J)), right)] - 1
      p <- p - qr(J)$rank
    }
  }
  out$clusters <- G
  out$parameters <- p
  if (G <= p) {
    out$reason <- sprintf(
      "Cluster-robust inference refused: %d independent clusters cannot support the %d free parameter directions. The cluster sandwich has rank at most G - 1 = %d, so its covariance is singular. More independent clusters or a justified simpler model are required.",
      G,
      p,
      max(
        0L,
        G - 1L
      )
    )
  } else if (G < 50L) {
    out$caution <- sprintf(
      "Only %d independent clusters for %d parameter directions. Cluster-robust inference can be inaccurate with fewer than about 30 to 50 clusters; this path supplies no small-cluster correction (Cameron & Miller, 2015).",
      G,
      p
    )
  }
  out
}

#' Attach Declared Continuous Auxiliaries to an Available-Data Model
#'
#' Declared auxiliary variables enter a FIML (or two-stage) fit as saturated
#' correlates (Graham, 2003) only when some analyzed item value is missing in
#' the analyzed rows. With every analyzed item observed in every analyzed
#' row, the likelihood of the items does not depend on the auxiliaries, so
#' they add no information: the estimates, the chi square and the fit indices
#' are those of the model without them. They are then left out, and `notes`
#' says so, as the Bollen-Stine refit does. Entered anyway, they would make
#' the bundled example's complete-data CFA carry the age in years beside the
#' test scores, drawing lavaan's warning about unequal observed variances
#' and counting 10 observed variables for a model of 9 items.
#'
#' @param spec Resolved model specification.
#' @param state Shared state with roles and raw data.
#' @param data Aligned analysis data.
#' @return Specification, data, warnings, notes (what was declared and why
#'   it was not used), and any precondition.
#' @author Ken Kelley
#' @noRd
sieve_auxiliary_apply <- function(spec, state, data) {
  out <- list(spec = spec, data = data, reason = NULL, warnings = character(), notes = character())
  if (!spec$missing %in% c("fiml", "ml", "robust.two.stage", "two.stage") || length(spec$ordered)) return(out)
  roles <- shiny::isolate(sieve_roles_table(state$raw, state$items, state$roles))
  raw <- shiny::isolate(state$raw)
  aux <- setdiff(roles$variable[roles$role %in% "auxiliary"], spec$observed)
  design <- sieve_sampling_spec(state)
  identifier <- if (!identical(design$unit, "independent")) intersect(aux, design$cluster) else character()
  if (length(identifier)) {
    aux <- setdiff(aux, identifier)
    out$warnings <- sprintf("The declared cluster or respondent identifier (%s) is used only for the sampling design and is omitted from the FIML auxiliary set.", paste(identifier, collapse = ", "))
  }
  if (!length(aux)) return(out)
  if (!sieve_auxiliary_in_force(spec$observed, data)) {
    # A structural model's observed variables include outcomes and
    # predictors that are not items.
    out$notes <- sieve_auxiliary_unused_note(aux, if (identical(spec$fun, "sem")) "variable" else "item")
    return(out)
  }
  if (nrow(raw) != nrow(data) || any(!aux %in% names(raw))) {
    out$reason <- "Auxiliary variables must have the same rows as the item data. Restore aligned data before fitting."
    return(out)
  }
  bad <- aux[!vapply(raw[aux], function(v) is.numeric(v) || is.integer(v), logical(1))]
  if (length(bad)) {
    out$reason <- paste("FIML auxiliaries must be continuous numeric variables. Review the auxiliary roles of", paste(bad, collapse = ", "))
    return(out)
  }
  for (nm in aux) data[[nm]] <- sieve_as_number(raw[[nm]])
  complete <- sum(stats::complete.cases(data[, c(spec$observed, aux), drop = FALSE]))
  if (complete < 3L * (length(spec$observed) + length(aux))) {
    out$warnings <- c(out$warnings, sprintf("Only %d complete cases for %d items and auxiliaries. Three complete cases per variable is a caution extrapolated from MI (Hardt et al., 2012), not a FIML cutoff; review the auxiliary set.", complete, length(spec$observed) + length(aux)))
  }
  out$spec <- sieve_spec_engine_args(spec, list(aux = aux))
  out$data <- data
  out
}

#' The Sentence That Says Declared Auxiliaries Are Not Used
#'
#' Said on the method line of each page that fits a model with FIML (CFA,
#' SEM, Validity, Bifactor, Reliability and its factor scores) and in the
#' recorded R, when `sieve_auxiliary_in_force()` leaves the declared
#' auxiliary variables out.
#' @param aux The declared auxiliary variables.
#' @param noun What the fit analyzes: `"item"`, or `"variable"` for a
#'   structural model, whose observed outcomes and predictors are not items.
#' @return One sentence.
#' @author Ken Kelley
#' @noRd
sieve_auxiliary_unused_note <- function(aux, noun = "item") {
  sprintf(
    "Auxiliary variable%s %s %s not used: every analyzed %s is observed in every analyzed row, so saturated correlates add no information (Graham, 2003).",
    if (length(aux) == 1L) "" else "s", paste(aux, collapse = ", "), if (length(aux) == 1L) "is" else "are", noun
  )
}

#' Do Declared Auxiliaries Enter a Fit of These Items?
#'
#' The one rule of `sieve_auxiliary_apply()`, shared with the Reliability
#' tab: declared auxiliary variables enter only when some analyzed item value
#' is missing in the analyzed rows, read as the fit reads the values (an SPSS
#' user-missing code is missing).
#' @param observed The analyzed items.
#' @param data The analyzed rows.
#' @return `TRUE` when some analyzed item value is missing (or no analyzed
#'   item is in `data`), `FALSE` when every one is observed.
#' @author Ken Kelley
#' @noRd
sieve_auxiliary_in_force <- function(observed, data) {
  analyzed <- intersect(observed, names(data))
  !length(analyzed) || any(vapply(analyzed, function(nm) anyNA(sieve_as_number(data[[nm]])), logical(1)))
}

#' Validate Original Row Positions for Reproducible Sampling
#' @param rows Positions in the original data frame.
#' @param n Number of supplied rows.
#' @return The unchanged numeric positions.
#' @author Ken Kelley
#' @noRd
sieve_bootstrap_row_references <- function(rows, n) {
  if (!is.numeric(rows) || is.complex(rows) || !is.null(dim(rows)) || length(rows) != n ||
      any(!is.finite(rows)) || any(rows < 1 | rows != floor(rows)) || anyDuplicated(rows)) {
    stop("Bootstrap row references must be unique finite positive whole positions, one per supplied row.", call. = FALSE)
  }
  rows
}

#' Plan Independent Sampling Units for a Bootstrap
#' @param n Number of eligible rows.
#' @param cluster Optional aligned nonmissing independent cluster labels.
#' @param rows Original row references.
#' @return A plain partition with row references, blocks and cluster count.
#' @author Ken Kelley
#' @noRd
sieve_bootstrap_partition <- function(n, cluster = NULL, rows = seq_len(n)) {
  sieve_bootstrap_row_references(rows, n)
  if (is.null(cluster)) return(list(unit = "independent", n = n, rows = rows, groups = NULL, G = NA_integer_))
  if (inherits(cluster, "haven_labelled_spss")) cluster <- haven::zap_missing(cluster)
  if (length(cluster) != n || anyNA(cluster) || any(!nzchar(trimws(as.character(cluster))))) {
    stop("Whole-cluster resampling needs one nonmissing cluster identifier per eligible row. Review the cluster column and row alignment.", call. = FALSE)
  }
  if (!n) stop("Whole-cluster resampling needs eligible rows in at least two clusters.", call. = FALSE)
  id <- match(cluster, unique(cluster))
  groups <- unname(split(seq_len(n), factor(id, levels = seq_len(max(id)))))
  G <- length(groups)
  if (G < 2L) stop("Whole-cluster resampling needs at least two independent clusters.", call. = FALSE)
  out <- list(unit = "clustered", n = n, rows = rows, groups = groups, G = as.integer(G),
    caution = if (G < 50L) sprintf("Only %d independent clusters; whole-cluster bootstrap intervals can be unstable with few clusters (Field & Welsh, 2007).", G))
  out$key <- sieve_hash(out)
  out
}

#' Draw Complete Sampling Units With Replacement
#' @param partition A plain bootstrap partition.
#' @return Eligible row indices; sample size varies when cluster sizes differ.
#' @author Ken Kelley
#' @noRd
sieve_bootstrap_rows <- function(partition) {
  if (identical(partition$unit, "independent")) return(sample.int(partition$n, partition$n, replace = TRUE))
  unlist(partition$groups[sample.int(partition$G, partition$G, replace = TRUE)], use.names = FALSE)
}

#' Describe a Bootstrap's Sampling Units
#' @param partition A plain bootstrap partition.
#' @return Plain method text.
#' @author Ken Kelley
#' @noRd
sieve_bootstrap_unit_text <- function(partition) {
  if (is.null(partition) || identical(partition$unit, "independent")) return("independent rows")
  paste(sprintf("%d whole independent clusters with replacement; each selected cluster keeps every eligible row and its missing-response pattern; resampled N can vary", partition$G), partition$caution %||% "")
}

#' Hold Aligned Cluster Labels for an App-Owned Bootstrap
#' @param state Shared study state.
#' @return NULL for independent rows, or a validated full aligned label vector.
#' @author Ken Kelley
#' @noRd
sieve_bootstrap_cluster <- function(state) {
  design <- sieve_sampling_spec(state)
  if (identical(design$unit, "independent")) return(NULL)
  problem <- sieve_sampling_problem(state, capability = "cluster")
  if (!is.null(problem)) stop(problem, call. = FALSE)
  raw <- shiny::isolate(state$raw)
  n <- nrow(shiny::isolate(state$items))
  if (length(design$cluster) != 1L || !design$cluster %in% names(raw) || nrow(raw) != n) {
    stop("Choose an aligned cluster identifier column before computing clustered uncertainty.", call. = FALSE)
  }
  cluster <- raw[[design$cluster]]
  sieve_bootstrap_partition(n, cluster)
  cluster
}

#' Withhold Unsupported Supplementary Reliability Intervals
#' @param record A coefficient record.
#' @param spec Its reliability specification.
#' @return The point estimate with explicitly unavailable dependent-row limits.
#' @author Ken Kelley
#' @noRd
sieve_reliability_cluster_interval <- function(record, spec) {
  if (is.null(record) || is.null(spec$cluster)) return(record)
  record$se <- record$lower <- record$upper <- NA_real_
  if (!is.null(record$variance)) record$variance <- NA_real_
  if (!is.null(record$fit_summary)) {
    record$fit_summary$lower <- record$fit_summary$upper <- NA_real_
    record$warnings <- c(record$warnings, "One-factor fit indices are descriptive under the cluster design; independent-row RMSEA limits are withheld.")
  }
  record$uncertainty$status <- "unavailable"
  record$uncertainty$label <- "Unavailable for the declared cluster design"
  record$uncertainty$reason <- "This supplementary coefficient has no whole-cluster interval on this path. Its independent-row analytic interval is withheld; use the primary omega whole-cluster bootstrap."
  kind <- if (inherits(record, "sieve_reliability_H")) "H" else if (!is.null(record$coefficient)) "omega_model_implied" else "alpha"
  prefix <- if (kind %in% c("H", "alpha")) kind else if (isTRUE(record$spec$companion)) "omega_model_implied" else "omega"
  object <- sieve_reliability_object(prefix, spec)
  record$code <- c(record$code, paste0("# ", record$uncertainty$reason),
    if (identical(kind, "H")) sprintf('%s[c("se", "lower", "upper")] <- NA_real_', object) else
      sprintf('%s$value[%s$term %%in%% c("se", "ci_lower", "ci_upper")] <- NA_real_', object, object))
  record
}

#' Resolve a Reliability Coefficient's Eligible Sampling Units
#' @param data Full response frame before construct-specific case selection.
#' @param spec Reliability specification with aligned cluster labels.
#' @return A partition of the exact rows eligible for this coefficient.
#' @author Ken Kelley
#' @noRd
sieve_reliability_sampling <- function(data, spec) {
  X <- sieve_reliability_data(data, unique(c(spec$items, spec$aux)))
  if (!is.null(spec$cluster) && length(spec$cluster) != nrow(X)) stop("Cluster labels must match the response frame before case selection.", call. = FALSE)
  keep <- if (identical(spec$missing, "fiml")) rowSums(!is.na(X)) > 0L else
    stats::complete.cases(X[, spec$items, drop = FALSE])
  sieve_bootstrap_partition(sum(keep), if (!is.null(spec$cluster)) spec$cluster[keep], which(keep))
}

#' Summarize Eligible Cluster Counts Across Reliability Constructs
#' @param results Fitted construct records.
#' @return A design-specific method sentence.
#' @author Ken Kelley
#' @noRd
sieve_reliability_sampling_line <- function(results) {
  if (all(vapply(results, function(r) is.null(r$spec$cluster), logical(1)))) return("")
  plans <- lapply(results, function(r) r$omega$uncertainty$sampling)
  if (!length(plans) || all(vapply(plans, is.null, logical(1)))) return("")
  G <- vapply(plans, function(x) x$G %||% NA_integer_, integer(1))
  if (all(is.finite(G)) && length(unique(G)) == 1L) return(paste0(" Sampling: ", sieve_bootstrap_unit_text(plans[[1L]]), "."))
  labels <- vapply(results, function(r) as.character(r$spec$construct), character(1))
  paste0(" Sampling: whole independent clusters with replacement. Eligible clusters: ",
    paste(ifelse(is.finite(G), sprintf("%s G = %d", labels, G), paste0(labels, ": interval unavailable (see its method note)")), collapse = "; "),
    ". Counts reflect each construct's own eligible rows; resampled N can vary.",
    if (any(G < 50L, na.rm = TRUE)) " Bootstrap intervals can be unstable with few clusters (Field & Welsh, 2007)." else "")
}
