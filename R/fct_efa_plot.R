# Coefficient coordinates from an already fitted EFA; no estimation or jitter.

#' Read coordinates for a factor plot
#' @param fit A DMAR::fa_exploratory() result, the EFA page's solution
#'   (`sieve_efa_dmar_solution()`), or a psych::fa() solution.
#' @param factors One or two factor names or column positions.
#' @param matrix Pattern or structure coefficients.
#' @param items Optional item identifiers to display.
#' @return A data frame with item, x and y, and the selected factor metadata.
#' @author Ken Kelley
#' @noRd
sieve_efa_plot_coordinates <- function(fit, factors = NULL,
                                       matrix = c("pattern", "structure"), items = NULL) {
  matrix <- match.arg(matrix)
  if (inherits(fit, "dmar_fa_exploratory")) fit <- sieve_efa_dmar_solution(fit)
  pattern <- unclass(fit$loadings)
  if (!is.matrix(pattern) || !is.numeric(pattern) || !length(pattern) || any(!is.finite(pattern))) {
    stop("The EFA must contain a finite numeric loading matrix.", call. = FALSE)
  }
  ids <- rownames(pattern) %||% paste0("item", seq_len(nrow(pattern)))
  columns <- colnames(pattern) %||% paste0("Factor", seq_len(ncol(pattern)))
  if (anyNA(c(ids, columns)) || any(!nzchar(c(ids, columns))) || anyDuplicated(ids) || anyDuplicated(columns)) {
    stop("Item and factor labels must be nonempty and unique.", call. = FALSE)
  }
  dimnames(pattern) <- list(ids, columns)
  if (is.null(factors)) factors <- columns[seq_len(min(2L, length(columns)))]
  if (is.numeric(factors)) {
    if (anyNA(factors) || any(!is.finite(factors)) || any(factors %% 1 != 0) ||
        any(factors < 1L | factors > length(columns))) {
      stop("Factor positions must identify columns in the fitted loading matrix.", call. = FALSE)
    }
    factors <- columns[factors]
  }
  expected <- min(2L, length(columns))
  if (!is.character(factors) || length(factors) != expected || anyNA(factors) ||
      anyDuplicated(factors) || any(!factors %in% columns)) {
    stop(sprintf("Select %d distinct fitted factor label(s).", expected), call. = FALSE)
  }
  align <- function(value, rows, cols, what) {
    value <- unclass(value)
    if (!is.matrix(value) || !is.numeric(value) || !identical(dim(value), c(length(rows), length(cols))) ||
        any(!is.finite(value))) stop(paste(what, "must be a finite numeric matrix of the fitted dimensions."), call. = FALSE)
    for (axis in seq_len(2L)) {
      named <- dimnames(value)[[axis]]
      wanted <- if (axis == 1L) rows else cols
      if (!is.null(named) && (anyDuplicated(named) || !setequal(named, wanted))) {
        stop(paste(what, "labels do not match the fitted loading matrix."), call. = FALSE)
      }
    }
    if (!is.null(rownames(value))) value <- value[rows, , drop = FALSE]
    if (!is.null(colnames(value))) value <- value[, cols, drop = FALSE]
    dimnames(value) <- list(rows, cols)
    value
  }
  phi <- fit$Phi
  if (is.null(phi)) phi <- diag(length(columns))
  phi <- align(phi, columns, columns, "Phi")
  values <- if (identical(matrix, "pattern")) pattern else {
    if (is.null(fit$Structure)) pattern %*% phi else align(fit$Structure, ids, columns, "Structure")
  }
  if (is.null(items)) items <- ids
  if (!is.character(items) || anyNA(items) || anyDuplicated(items) || any(!items %in% ids)) {
    stop("Items must be distinct identifiers in the fitted loading matrix.", call. = FALSE)
  }
  values <- values[items, factors, drop = FALSE]
  two <- length(factors) == 2L
  out <- data.frame(item = items, x = unname(values[, 1L]),
    y = if (two) unname(values[, 2L]) else seq_along(items), stringsAsFactors = FALSE)
  attr(out, "factors") <- factors
  attr(out, "matrix") <- matrix
  attr(out, "factor_correlation") <- if (two) unname(phi[factors[1L], factors[2L]]) else NA_real_
  out
}

#' Place readable labels without moving coefficient points
#' @param data Factor coordinate data frame.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_efa_plot_labels <- function(data) {
  usr <- graphics::par("usr")
  gap <- diff(usr[1:2]) * .008
  height <- graphics::strheight("M", cex = .75) * 1.5
  widths <- graphics::strwidth(data$item, cex = .75)
  boxes <- matrix(numeric(), ncol = 4L)
  directions <- rbind(c(1, 0), c(-1, 0), c(0, 1), c(0, -1),
    c(1, 1), c(-1, 1), c(1, -1), c(-1, -1))
  for (i in seq_len(nrow(data))) {
    candidates <- do.call(rbind, lapply(0:12, function(ring) {
      cbind(data$x[i] + directions[, 1L] * (widths[i] / 2 + gap + ring * height),
        data$y[i] + directions[, 2L] * height * (.8 + ring))
    }))
    scores <- apply(candidates, 1L, function(center) {
      box <- c(center[1L] - widths[i] / 2, center[1L] + widths[i] / 2,
        center[2L] - height / 2, center[2L] + height / 2)
      outside <- sum(pmax(c(usr[1L] + gap - box[1L], box[2L] - usr[2L] + gap,
        usr[3L] + height / 2 - box[3L], box[4L] - usr[4L] + height / 2), 0))
      overlaps <- if (!nrow(boxes)) 0 else sum(
        pmax(0, pmin(box[2L], boxes[, 2L]) - pmax(box[1L], boxes[, 1L]) + gap) *
          pmax(0, pmin(box[4L], boxes[, 4L]) - pmax(box[3L], boxes[, 3L])))
      points <- sum(data$x[-i] > box[1L] - gap & data$x[-i] < box[2L] + gap &
        data$y[-i] > box[3L] - height / 2 & data$y[-i] < box[4L] + height / 2)
      1000 * outside + 100 * overlaps + points
    })
    at <- which(scores == 0)[1L]
    if (is.na(at)) at <- which.min(scores)
    center <- candidates[at, ]
    graphics::segments(data$x[i], data$y[i], center[1L], center[2L], col = "#9299a0", lwd = .6)
    graphics::text(center[1L], center[2L], labels = data$item[i], cex = .75, col = "#0c2340")
    boxes <- rbind(boxes, c(center[1L] - widths[i] / 2, center[1L] + widths[i] / 2,
      center[2L] - height / 2, center[2L] + height / 2))
  }
  invisible(NULL)
}

#' Plot the Coefficients of a Fitted Exploratory Factor Solution
#'
#' Draws the selected coefficients from an already fitted solution, without
#' refitting, reordering factors, suppressing small coefficients or jittering
#' points. With two or more factors, choose a pair of actual factor labels.
#' With one factor, coefficients are displayed by item.
#'
#' @param fit A fitted common-factor solution: a \code{DMAR::fa_exploratory()}
#'   result (the EFA page's engine; its standardized pattern coefficients and
#'   factor correlations are read) or a \code{psych::fa()} solution.
#' @param factors Two factor labels or column positions, or one for a
#'   single-factor solution. The default uses the first two fitted columns.
#' @param matrix Either \code{"pattern"} (standardized regression coefficients)
#'   or \code{"structure"} (item-factor correlations).
#' @param items Optional distinct item identifiers to display. This subsets
#'   plotted rows only and does not fit a different model.
#' @param labels Logical; label points with item identifiers. Single-factor
#'   plots always identify their item rows.
#' @return Invisibly, a data frame with \code{item}, \code{x} and \code{y}.
#'   Attributes \code{factors}, \code{matrix} and \code{factor_correlation}
#'   record the displayed factor labels, coefficient type and pair correlation.
#'   In a single-factor plot, \code{y} is an item-row position.
#' @details For oblique solutions, the structure matrix is the pattern matrix
#'   multiplied by Phi. Cartesian plot axes are coefficient coordinates, not
#'   an assertion that latent factors are uncorrelated. Pattern coefficients
#'   can exceed one in absolute value; axes include all displayed values.
#'   Labels may be placed away from points, with leader lines; coefficient
#'   points retain their original positions.
#'   A plot of one factor pair is a projection of a larger solution and cannot
#'   establish simple structure on omitted factors. See the
#'   \code{DMAR::fa_exploratory()} documentation for the definitions of the
#'   standardized pattern coefficients and factor correlations it reports.
#' @seealso \code{DMAR::fa_exploratory()}, \code{psych::fa()}
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' solution <- DMAR::fa_exploratory(items, nfactors = 3, estimator = "ML",
#'   missing = "listwise", rotation = "oblimin", seed = 113)
#' coordinates <- sieve_efa_factor_plot(solution)
#' head(coordinates)
#' @author Ken Kelley
#' @export
sieve_efa_factor_plot <- function(fit, factors = NULL, matrix = c("pattern", "structure"),
                                  items = NULL, labels = TRUE) {
  matrix <- match.arg(matrix)
  if (!is.logical(labels) || length(labels) != 1L || is.na(labels)) {
    stop("Labels must be TRUE or FALSE.", call. = FALSE)
  }
  data <- sieve_efa_plot_coordinates(fit, factors, matrix, items)
  if (!nrow(data)) stop("No items remain in this display filter.", call. = FALSE)
  factors <- attr(data, "factors")
  two <- length(factors) == 2L
  old <- graphics::par(mar = c(5, if (two) 5 else 8, 4, 2) + .1)
  on.exit(graphics::par(old), add = TRUE)
  limit <- max(1, abs(data$x), if (two) abs(data$y) else 0) * 1.16
  coefficient <- if (identical(matrix, "pattern")) "Pattern coefficient" else "Structure coefficient"
  graphics::plot(data$x, data$y, type = "n", xlim = c(-limit, limit),
    ylim = if (two) c(-limit, limit) else c(nrow(data) + .7, .3),
    asp = if (two) 1 else NA, yaxt = if (two) "s" else "n",
    xlab = paste(factors[1L], coefficient, sep = ": "),
    ylab = if (two) paste(factors[2L], coefficient, sep = ": ") else "",
    main = "EFA factor plot")
  graphics::abline(v = 0, col = "#8a9199", lty = 2)
  if (two) graphics::abline(h = 0, col = "#8a9199", lty = 2)
  graphics::points(data$x, data$y, pch = 21, col = "#0c2340", bg = "#0c2340", cex = 1)
  if (two && labels) sieve_efa_plot_labels(data)
  if (!two) graphics::axis(2, at = data$y, labels = data$item, las = 1, cex.axis = .8)
  if (two) graphics::mtext(sprintf("Factor correlation: %s with %s = %.3f",
    factors[1L], factors[2L], attr(data, "factor_correlation")), side = 3, line = .35, cex = .8)
  invisible(data)
}
