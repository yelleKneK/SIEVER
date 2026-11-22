# Test-only resampling oracle for whole-cluster membership.
sieve_resample_units <- function(n, cluster = NULL) {
  if (is.null(cluster)) return(sample.int(n, n, replace = TRUE))
  if (length(cluster) != n || anyNA(cluster)) {
    stop("Resampling requires one nonmissing cluster identifier per observation.", call. = FALSE)
  }
  groups <- split(seq_len(n), factor(cluster, levels = unique(cluster)))
  unlist(groups[sample.int(length(groups), length(groups), replace = TRUE)], use.names = FALSE)
}
