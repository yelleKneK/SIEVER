# Shared services.


#' The bifactor model a ledger implies, with what it does and does not hold
#'
#' The general factor loads every active item with a construct. The group
#' factors are built on the first-order groups of `sieve_structure_groups()`,
#' the same groups the CFA tab's Compare structures uses: the facets of the
#' construct when the included items form one construct split into two or
#' more facets, and the constructs otherwise. Each group with at least
#' `min_group` active items contributes one orthogonal group factor measured
#' by its active items; the items of a smaller group, and the items of the
#' one construct that carry no facet, load on the general factor only, and
#' are listed in `not_modeled`. One construct without facets has no group
#' to build: a group factor on the same items as the general factor is not
#' identified, so no model is written and `reason` says so. A model is
#' written only with at least `min_groups` group factors, the rule Compare
#' structures applies as well: with two group factors the covariances between the
#' groups fix the general factor's loadings only up to a scale that the
#' covariances within the groups must settle, which they cannot when each
#' group has three items, and the higher-order model with two first-order
#' factors has the same limit. The latent names come from
#' `sieve_name_plan()`: the general factor is named `general` unless an
#' observed variable has that name, and a group whose own label is
#' "general" gets a different latent name, so the two cannot be confused.
#'
#' @param meta The item ledger.
#' @param min_group Minimum active items for a group to get a group factor.
#' @param reserved Further observed variable names the latent names must
#'   avoid (the columns of the data).
#' @param min_groups Minimum number of group factors for a model.
#' @return A list with `structure`, `model` (`""` when no model can be
#'   written), `observed`, `latent`, `general` (the general factor's
#'   name), `name_map`, `not_modeled`, `names`, `level` (`"facet"` or
#'   `"construct"`: what the group factors are), `groups` (the groups that
#'   have a group factor), and `reason` (why no model was written, or `NA`).
#' @author Ken Kelley
#' @noRd
sieve_bifactor_plan <- function(meta, min_group = 3L, reserved = NULL, min_groups = 3L) {
  plan_names <- sieve_name_plan(meta, reserved = reserved)
  out <- list(
    structure = "bifactor", model = "", observed = character(0),
    latent = character(0), general = plan_names$general,
    name_map = character(0), not_modeled = sieve_not_modeled(),
    names = plan_names, level = "construct", groups = character(0),
    reason = NA_character_
  )
  active <- meta[meta$status == "active" & sieve_construct_assigned(meta$construct), , drop = FALSE]
  if (!nrow(active)) {
    out$reason <- paste(
      "No included item is assigned to a construct; a bifactor model needs group factors",
      "of at least three active items each."
    )
    return(out)
  }
  g <- sieve_structure_groups(meta)
  facets <- identical(g$level, "facet")
  out$level <- g$level
  # Small counts in words, as the messages of the app give them.
  in_words <- function(n) {
    words <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    if (n >= 1L && n <= 9L) words[n] else as.character(n)
  }
  noun <- if (facets) "facet" else "construct"
  if (length(g$ungrouped)) {
    out$not_modeled <- rbind(out$not_modeled, sieve_not_modeled(
      "item", g$scale, NA_character_, g$ungrouped,
      "they carry no facet, so they have no group factor; they load on the general factor only"
    ))
  }
  for (grp in g$groups[g$sizes < min_group]) {
    its <- g$items[g$group %in% grp]
    out$not_modeled <- rbind(out$not_modeled, sieve_not_modeled(
      noun, if (facets) g$scale else grp, if (facets) grp else NA_character_, its,
      sprintf(
        "the %s has %d included item%s, so it has no group factor (a group factor needs at least %d); %s on the general factor only",
        noun, length(its), if (length(its) == 1L) "" else "s", as.integer(min_group),
        if (length(its) == 1L) "its item loads" else "its items load"
      )
    ))
  }
  group_ids <- g$groups[g$sizes >= min_group]
  out$groups <- group_ids
  if (!length(group_ids)) {
    out$reason <- sprintf(
      "No %s has at least %s active items for a group factor.", noun, in_words(as.integer(min_group))
    )
    return(out)
  }
  if (!facets && length(g$groups) == 1L) {
    out$reason <- sprintf(
      paste(
        "The included items form one construct ('%s') that is not split into two or more",
        "facets, so its group factor would load the same items as the general factor, and two",
        "uncorrelated factors on the same items are not identified (any rotation of the pair",
        "fits equally well). Assign facets to the items to fit a bifactor model with the facets",
        "as its group factors."
      ),
      g$groups
    )
    return(out)
  }
  if (length(group_ids) < min_groups) {
    sizes <- g$sizes[group_ids]
    out$reason <- sprintf(
      paste(
        "A bifactor model needs at least %s group factors of at least %s included items each",
        "(the rule of the Bifactor tab and of Compare structures on the CFA tab), and the included items give",
        "%d (%s). With two group factors the covariances between the groups fix the general",
        "factor's loadings only up to a scale that the covariances within the groups must settle,",
        "which they cannot when each group has three items; with one, only the items outside the",
        "group tell the general factor from the group factor."
      ),
      in_words(as.integer(min_groups)), in_words(as.integer(min_group)), length(group_ids),
      paste(sprintf("%s, %d items", group_ids, sizes), collapse = "; ")
    )
    return(out)
  }
  latent_of <- function(grp) {
    if (!facets) {
      return(sieve_latent_name(plan_names, grp))
    }
    fm <- plan_names$facet
    unname(fm$name[match(paste(g$scale, grp, sep = "\r"), paste(fm$construct, fm$facet, sep = "\r"))])
  }
  lines <- sprintf("%s =~ %s", plan_names$general, paste(g$items, collapse = " + "))
  for (grp in group_ids) {
    its <- g$items[g$group %in% grp]
    lines <- c(lines, sprintf("%s =~ %s", latent_of(grp), paste(its, collapse = " + ")))
  }
  group_names <- vapply(group_ids, latent_of, character(1), USE.NAMES = FALSE)
  # The general factor is shown as "general"; a group that carries that
  # very label is told apart from it.
  group_labels <- ifelse(group_ids == "general", "general (group factor)", group_ids)
  out$model <- paste(lines, collapse = "\n")
  out$observed <- g$items
  out$latent <- c(plan_names$general, group_names)
  out$name_map <- stats::setNames(c("general", group_labels), out$latent)
  out
}

#' Fit the Bifactor Model and Assemble Loadings and Indices
#'
#' Fits the model of `sieve_bifactor_plan()` with the estimator implied by
#' the measurement level of its indicators, with `std.lv = TRUE` and
#' `orthogonal = TRUE`, so the general and group factors are uncorrelated
#' and each has variance one (which sets the latent scale; the loadings
#' reported are read from the standardized solution). The fit goes through
#' [sieve_run_model()], so the result states whether the model failed, did
#' not converge, is not identified, is inadmissible, or is ok, and keeps the
#' conditions lavaan raised. Loadings and indices are assembled only for a
#' model that converged and is identified.
#'
#' @param items_df A data frame holding the item columns.
#' @param meta The item ledger.
#' @param estimator One of `"auto"`, which follows the item types (ML
#'   with FIML when every item is continuous, WLSMV when any is ordered or
#'   binary), `"mlr"` (MLR with FIML), `"ml_fiml"` (ML with FIML), `"ml"`
#'   (ML on the complete cases) or `"wlsmv"` (WLSMV, every item treated as
#'   ordered); the tab passes its Estimator control's choice.
#' @param cluster Optional column identifying independent clusters for continuous ML/MLR.
#' @param aux Continuous auxiliary columns present in items_df, used under FIML.
#' @return A list with `syntax`, `fit`, `loadings`, `indices` (the
#'   DMAR::bifactor_indices table: one row per factor, general first, with
#'   columns `factor`, `ECV`, `omega`, `omega_H`, `omega_HS`, `PUC`, `H`;
#'   `factor` holds "general" for the general factor and the construct label
#'   for each group factor), `improper` (the Heywood flag from that call,
#'   `NA` when the indices are unavailable), `error`, `status` (see
#'   `sieve_fit_status()`), `warnings`, `spec`, and `name_map`. `error` is
#'   `NULL` when there is a fitted model and a message string otherwise.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' bf <- sieve_fit_bifactor(items, meta)
#' bf$status$code
#' bf$indices
#' @export
sieve_fit_bifactor <- function(items_df, meta, estimator = "auto", aux = NULL, cluster = NULL) {
  out <- list(
    syntax = "", fit = NULL, loadings = NULL,
    indices = NULL, improper = NA, error = NULL,
    status = sieve_fit_status(NULL), warnings = character(0),
    spec = NULL, name_map = character(0)
  )

  plan <- sieve_bifactor_plan(meta, reserved = names(items_df))
  spec <- sieve_model_spec(plan, meta,
    kind = "bifactor", fun = "cfa",
    std_lv = TRUE, orthogonal = TRUE, estimator = estimator
  )
  if (identical(spec$missing, "fiml") && !length(spec$ordered) && length(aux)) {
    spec <- sieve_spec_engine_args(spec, list(aux = aux))
  }
  if (!is.null(cluster)) {
    if (length(spec$ordered)) stop("Categorical cluster inference is not supplied by this path.", call. = FALSE)
    spec$estimator <- "MLR"
    spec <- sieve_spec_engine_args(spec, list(cluster = cluster))
  }
  out$syntax <- plan$model
  out$spec <- spec
  out$name_map <- plan$name_map
  out$data <- items_df
  if (!nzchar(plan$model)) {
    out$error <- if (is.na(plan$reason %||% NA_character_)) {
      "No construct has at least three active items for a group factor."
    } else plan$reason
    out$status <- sieve_fit_status(NULL, error = out$error)
    return(out)
  }
  run <- sieve_run_model(spec, items_df)
  out$status <- run$status
  out$warnings <- run$warnings
  if (is.null(run$fit)) {
    out$error <- run$error %||% "lavaan returned no fit."
    return(out)
  }
  fit <- run$fit
  out$fit <- fit
  if (!sieve_has_solution(run)) {
    # No loadings or indices are assembled from estimates that are not a
    # solution; the status says why.
    return(out)
  }

  loadings <- sieve_capture(sieve_bifactor_loadings(fit, general = plan$general))
  out$loadings <- loadings$value
  if (!loadings$ok) {
    out$warnings <- c(out$warnings, paste("The loadings could not be read from the fit:", loadings$error))
  }
  if (!is.null(out$loadings)) {
    out$loadings$group <- ifelse(is.na(out$loadings$group), NA_character_,
      sieve_show_names(out$loadings$group, plan$name_map)
    )
  }
  # The indices come from DMAR::bifactor_indices, the canonical implementation
  # (Rodriguez, Reise & Haviland, 2016; coefficient H per Hancock & Mueller,
  # 2001). Its Heywood warning is not repeated here because the improper flag
  # is carried on the "improper" attribute and shown in the status alert.
  idx <- sieve_capture(DMAR::bifactor_indices(fit, general = plan$general))
  if (idx$ok && !is.null(idx$value)) {
    out$improper <- isTRUE(attr(idx$value, "improper"))
    out$indices <- as.data.frame(idx$value)
    out$indices$factor <- sieve_show_names(out$indices$factor, plan$name_map)
  } else if (!idx$ok) {
    out$indices_reason <- paste("Bifactor indices are unavailable:", idx$error,
      if (grepl("two group|more than one|multiple.*group", idx$error, ignore.case = TRUE))
        "Assign each item to only one group factor, review the model, and fit it again." else
        "Review the model and fitted solution before requesting its indices again.")
  }
  out
}

#' Standardized general and group loadings per item from a bifactor fit
#'
#' Reads the standardized solution and, for each active item, records its
#' loading on the general factor, its loading on whichever group factor it
#' belongs to, and its uniqueness. Items load on at most one group factor by
#' construction.
#'
#' @param fit A fitted bifactor lavaan object.
#' @param general The name of the general factor in the fit.
#' @return A data frame with columns `item`, `group`, `general`, `group`
#'   loading, and `uniqueness`.
#' @author Ken Kelley
#' @noRd
sieve_bifactor_loadings <- function(fit, general = "general") {
  ss <- lavaan::standardizedSolution(fit)
  load <- ss[ss$op == "=~", c("lhs", "rhs", "est.std")]
  items <- unique(load$rhs)

  gen <- load[load$lhs == general, ]
  grp <- load[load$lhs != general, ]

  # Model-implied total standardized variance of an item is 1, so uniqueness
  # is one minus the sum of its squared loadings across general and group.
  general_l <- stats::setNames(rep(0, length(items)), items)
  group_l <- stats::setNames(rep(0, length(items)), items)
  group_name <- stats::setNames(rep(NA_character_, length(items)), items)

  general_l[gen$rhs] <- gen$est.std
  if (nrow(grp)) {
    group_l[grp$rhs] <- grp$est.std
    group_name[grp$rhs] <- grp$lhs
  }

  # Raw uniqueness can be negative when a solution is improper (a Heywood case:
  # the squared loadings exceed the item's standardized variance of 1). We keep
  # the RAW value so the improper-solution check can see it, and a clamped copy
  # for display and the index formulas, which assume admissible variances.
  uniq_raw <- 1 - general_l[items]^2 - group_l[items]^2
  data.frame(
    item = items,
    group = unname(group_name[items]),
    general = unname(general_l[items]),
    group_loading = unname(group_l[items]),
    uniqueness = pmax(0, unname(uniq_raw)),
    uniqueness_raw = unname(uniq_raw),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
