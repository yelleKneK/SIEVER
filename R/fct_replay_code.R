# Shared services.


#' Wrap R Literals Into One `c()` Call
#'
#' Lays a vector of already-deparsed elements out as the body of a `c()`
#' call, breaking lines near `width` so a long item pool does not produce one
#' unreadable line.
#'
#' @param el Character vector of deparsed elements (`"\"x\""`, `"TRUE"`, ...).
#' @param indent Spaces to indent each emitted line.
#' @param width Soft right margin.
#' @return A character vector of R source lines, including the `c(` and `)`.
#' @author Ken Kelley
#' @noRd
sieve_c_call <- function(el, indent = 2L, width = 76L) {
  pad <- strrep(" ", indent)
  if (!length(el)) {
    return(c("c(", paste0(pad, "character(0)"), ")"))
  }
  lines <- character(0)
  cur <- ""
  for (i in seq_along(el)) {
    piece <- paste0(el[i], if (i < length(el)) "," else "")
    cand <- if (nzchar(cur)) paste(cur, piece) else piece
    if (nzchar(cur) && indent + nchar(cand) > width) {
      lines <- c(lines, cur)
      cur <- piece
    } else {
      cur <- cand
    }
  }
  c("c(", paste0(pad, c(lines, cur)), ")")
}

#' Deparse a Character Vector Element by Element
#'
#' @param x A character vector.
#' @return A character vector of R literals, with `NA` written as
#'   `NA_character_` so the emitted column keeps its type.
#' @author Ken Kelley
#' @noRd
sieve_chr_lit <- function(x) {
  x <- as.character(x)
  out <- encodeString(x, quote = "\"")
  out[is.na(x)] <- "NA_character_"
  out
}

#' One Named Field of an Emitted `data.frame()` Call
#'
#' @param name The column name.
#' @param el Deparsed elements for that column.
#' @return A character vector of R source lines.
#' @author Ken Kelley
#' @noRd
sieve_df_field <- function(name, el) {
  body <- sieve_c_call(el, indent = 4L)
  c(
    paste0("  ", name, " = ", body[1]),
    body[-c(1L, length(body))],
    "  ),"
  )
}

#' Reproducible SPSS-Aware Numeric Conversion
#' @param object Name of a data frame in generated R code.
#' @param columns Optional selected columns, leaving design labels unchanged.
#' @param factor_labels Read factor labels as the model input reader does.
#' @return Self-contained R source lines.
#' @author Ken Kelley
#' @noRd
sieve_numeric_code <- function(object, columns = NULL, factor_labels = FALSE) {
  selected <- if (is.null(columns)) "" else sieve_r_literal(columns)
  c(
    sprintf("%s[%s] <- lapply(%s%s, function(v) {", object, selected, object,
      if (is.null(columns)) "" else paste0("[", selected, "]")),
    if (factor_labels) "  if (is.factor(v)) v <- as.character(v)",
    "  x <- suppressWarnings(as.numeric(unclass(v)))",
    "  recorded <- attr(v, \"spss_missing\", exact = TRUE)",
    "  values <- c(attr(v, \"na_values\", exact = TRUE), recorded$values)",
    "  range <- attr(v, \"na_range\", exact = TRUE)",
    "  if (is.null(range)) range <- recorded$range",
    "  if (length(values)) x[x %in% values] <- NA_real_",
    "  if (length(range) == 2L && all(is.finite(range))) x[!is.na(x) & x >= range[1L] & x <= range[2L]] <- NA_real_",
    "  x",
    "})"
  )
}
