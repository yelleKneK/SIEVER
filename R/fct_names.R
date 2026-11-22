# fct_names.R
# The name space of a model. A researcher's construct, facet, and outcome
# labels are free text; the names a latent variable model is written in must
# be syntactic, and no latent variable may share a name with an observed
# variable (lavaan reads `x1 =~ x1 + x2` as a factor called x1 measured by x2
# alone, and returns a fit). Every model builder therefore takes its names
# from one allocation, made here, that is disjoint from the observed
# variables, from the engine's reserved words, and within itself. The
# researcher's label is kept in a name map (syntactic name -> label) that
# travels with the specification and the fit, so displays show the label.

#' Names a model variable may not take
#'
#' R's reserved words (lavaan rejects them as identifiers), the modifiers and
#' block keywords of lavaan model syntax, and `general`, which SIEVER keeps
#' for the general factor of a bifactor model.
#'
#' @return A character vector.
#' @author Ken Kelley
#' @noRd
sieve_reserved_words <- function() {
  c(
    "if", "else", "repeat", "while", "function", "for", "next", "break", "in",
    "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_",
    "NA_complex_", "NA_character_",
    "c", "start", "equal", "label", "lower", "upper", "prior", "rv", "efa",
    "group", "level", "block", "class",
    "general"
  )
}

#' Is a name usable as a model identifier?
#'
#' Usable means an ASCII letter followed by ASCII letters, digits,
#' underscores, or periods, that is not a reserved word and does not look
#' like a lavaan threshold (`t1`, `t2`, and so on).
#'
#' @param x A character vector of candidate names.
#' @return A logical vector.
#' @author Ken Kelley
#' @noRd
sieve_name_usable <- function(x) {
  grepl("^[A-Za-z][A-Za-z0-9_.]*$", x) &
    !(x %in% sieve_reserved_words()) &
    !grepl("^t[0-9]+$", x)
}

#' A syntactic stem for a label
#'
#' Every character that is not an ASCII letter, digit, or underscore becomes
#' an underscore (a run of characters outside ASCII becomes one underscore,
#' whatever the locale), so a readable stem survives where the label has
#' one. A label with no ASCII letter or digit has no stem and gets a
#' positional name instead.
#'
#' @param label A single label.
#' @param position The position of the label among those being named.
#' @param prefix The prefix that marks the kind of name.
#' @return A length one character string beginning with an ASCII letter.
#' @author Ken Kelley
#' @noRd
sieve_name_stem <- function(label, position, prefix) {
  x <- enc2utf8(as.character(label))
  x <- iconv(x, from = "UTF-8", to = "ASCII", sub = "\001")
  if (is.na(x)) {
    x <- ""
  }
  x <- gsub("\001+", "_", x)
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  if (!grepl("[A-Za-z0-9]", x)) {
    return(sprintf("%s%d", prefix, position))
  }
  if (grepl("^[0-9_]", x)) {
    x <- paste0(prefix, "_", x)
  }
  x
}

#' Allocate syntactic names for a set of labels
#'
#' Each label gets a name that is usable as a model identifier, differs from
#' every name in `taken`, and differs from the names given to the other
#' labels. A label whose stem is already usable and free keeps it, so
#' ordinary labels read as themselves (`Visual` stays `Visual`). A stem that
#' is a reserved word or belongs to an observed variable is prefixed
#' (`x1` becomes `F_x1`); labels that share a stem are numbered in order
#' (`A-B`, `A_B`, `A B` become `A_B`, `A_B_1`, `A_B_2`).
#'
#' @param labels Character vector of labels (duplicates share one name).
#' @param taken Names that are not available: observed variables, names
#'   already allocated, and anything else the model must not reuse.
#' @param prefix The prefix that marks the kind of name (`"F"` for a latent
#'   variable, `"Y"` for an outcome, `"p"` for a parameter label).
#' @return A named character vector, label -> allocated name, with one entry
#'   per distinct label, in order of first appearance.
#' @author Ken Kelley
#' @noRd
sieve_allocate_names <- function(labels, taken = character(0), prefix = "F") {
  labels <- unique(as.character(labels))
  labels <- labels[!is.na(labels)]
  taken <- unique(as.character(taken))
  out <- stats::setNames(character(length(labels)), labels)
  for (i in seq_along(labels)) {
    base <- sieve_name_stem(labels[i], i, prefix)
    if (!sieve_name_usable(base) || base %in% taken) {
      base <- paste0(prefix, "_", base)
    }
    candidate <- base
    k <- 0L
    while (candidate %in% taken || candidate %in% out[seq_len(i - 1L)] ||
      !sieve_name_usable(candidate)) {
      k <- k + 1L
      candidate <- paste0(base, "_", k)
    }
    out[i] <- candidate
  }
  out
}

#' The names of the latent variables a ledger implies
#'
#' One allocation serves every model built from the ledger, so a construct
#' has the same latent name in the correlated-factors model, the
#' higher-order model, the bifactor model, and a structural model. Names are
#' allocated for every construct and every construct and facet pair in the
#' ledger (included or not, in ledger order), so excluding an item does not
#' rename a factor. They are disjoint from every ledger item, from the names
#' in `reserved`, from each other, and from the general factor's name.
#'
#' @param meta The item ledger.
#' @param reserved Further observed variable names to keep clear of: the
#'   columns of the data the model will be fit to (outcomes, grouping and
#'   other auxiliary variables).
#' @return A list with `observed` (every name treated as observed),
#'   `construct` (named character vector, construct label -> latent name),
#'   `facet` (a data frame with `construct`, `facet`, `name`, and `label`),
#'   and `general` (the name of a bifactor general factor).
#' @author Ken Kelley
#' @noRd
sieve_name_plan <- function(meta, reserved = NULL) {
  observed <- unique(c(as.character(meta$item), as.character(reserved)))
  observed <- observed[!is.na(observed)]
  cons <- unique(as.character(meta$construct))
  cons <- cons[sieve_construct_assigned(cons)]
  construct <- sieve_allocate_names(cons, taken = observed, prefix = "F")

  facet_col <- if ("facet" %in% names(meta)) as.character(meta$facet) else rep(NA_character_, nrow(meta))
  has_facet <- !is.na(facet_col) & nzchar(facet_col) & sieve_construct_assigned(meta$construct)
  pairs <- unique(data.frame(
    construct = as.character(meta$construct)[has_facet], facet = facet_col[has_facet],
    stringsAsFactors = FALSE
  ))
  rownames(pairs) <- NULL
  # Allocate one distinct latent name per exact nested pair. The readable
  # stem may collide (A.B / C and A / B.C), so the map is positional rather
  # than indexed by a pasted label.
  facet_names <- character(nrow(pairs))
  for (i in seq_len(nrow(pairs))) {
    facet_names[i] <- unname(sieve_allocate_names(
      paste(pairs$construct[i], pairs$facet[i], sep = "."),
      taken = c(observed, construct, facet_names), prefix = "F"
    ))
  }
  pairs$name <- facet_names
  pairs$label <- sprintf("%s: %s", pairs$construct, pairs$facet)

  used <- c(observed, construct, pairs$name)
  general <- "general"
  k <- 0L
  while (general %in% used) {
    general <- if (k == 0L) "general_factor" else sprintf("general_factor_%d", k)
    k <- k + 1L
  }
  list(observed = observed, construct = construct, facet = pairs, general = general)
}

#' The latent names of constructs in a name plan
#'
#' @param plan_names A name plan from `sieve_name_plan()`.
#' @param constructs Character vector of construct labels.
#' @return The latent names, in the order of `constructs` (looked up by
#'   exact match, so an empty label is handled like any other).
#' @author Ken Kelley
#' @noRd
sieve_latent_name <- function(plan_names, constructs) {
  unname(plan_names$construct[match(constructs, names(plan_names$construct))])
}

#' The name map an object carries
#'
#' @param object A specification, a `sieve_result`, a fitted model carrying
#'   a `name_map` attribute, or a named character vector.
#' @return A named character vector, syntactic name -> label (possibly
#'   empty).
#' @author Ken Kelley
#' @noRd
sieve_name_map <- function(object) {
  map <- if (is.character(object)) {
    object
  } else if (inherits(object, "sieve_result")) {
    object$spec$name_map
  } else if (inherits(object, "sieve_spec")) {
    object$name_map
  } else if (is.list(object) && !is.null(object$name_map)) {
    object$name_map
  } else {
    attr(object, "name_map")
  }
  if (is.null(map)) character(0) else map
}

#' Show the researcher's labels for model names
#'
#' @param x Character vector of names as the engine reports them.
#' @param object Anything `sieve_name_map()` accepts.
#' @return `x` with every name found in the map replaced by its label.
#' @author Ken Kelley
#' @noRd
sieve_show_names <- function(x, object) {
  map <- sieve_name_map(object)
  if (!length(map)) {
    return(as.character(x))
  }
  hit <- match(x, names(map))
  ifelse(is.na(hit), as.character(x), unname(map[hit]))
}
