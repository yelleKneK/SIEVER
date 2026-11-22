# Small numerical and authorization anchors: each expected value is computed
# without the service it checks.
test_that('Anchor: excludes the focal item from its rest', {
  d <- data.frame(a = c(1, 4, 2, 8, 5), b = c(2, 1, 6, 4, 7), c = c(8, 3, 5, 1, 4))
  got <- sieve_item_rest(d, names(d))
  expect_equal(got$r[got$item == 'a'], stats::cor(d$a, (d$b + d$c) / 2), tolerance = 1e-14)
})
test_that('Anchor: reverses within the declared bounds', {
  d <- data.frame(x = c(0, 2, 7, NA, 10))
  m <- sieve_build_meta(d, 'x')
  out <- sieve_reverse_item(data = d, meta = m, item = 'x', lo = 0, hi = 10)
  expect_identical(out$data$x, c(10, 8, 3, NA, 0))
})
test_that('Anchor: retains loading covariance in H variance', {
  lambda <- c(.6, .7, .8)
  V <- matrix(.0004, 3, 3)
  diag(V) <- .001
  # Complex-step differentiation of the scalar definition supplies a different
  # numerical route without the subtraction error of finite differences.
  definition <- function(l) {
    a <- sum(l^2 / (1 - l^2))
    a / (1 + a)
  }
  eps <- 1e-20
  g <- vapply(1:3, function(i) {
    z <- as.complex(lambda)
    z[i] <- z[i] + 1i * eps
    Im(definition(z)) / eps
  }, numeric(1))
  expected <- sum(outer(g, g) * V)
  expect_equal(sieve_H_delta(lambda, V)$variance, expected, tolerance = 1e-12)
})
test_that('Anchor: selects one eligible fit family for every model', {
  family <- function(ok) list(available = ok, complete = ok, scope = 'model', reason = if (ok) '' else 'Controlled unavailable family')
  one <- list(status = 'ok', engine_error = NULL, families = list(robust = family(FALSE), scaled = family(TRUE), standard = family(TRUE)))
  two <- one
  two$families$robust <- family(TRUE)
  expect_identical(sieve_common_family(list(first = one, second = two))$family, 'scaled')
})
test_that('Anchor: refuses transport without payload authorization', {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  request <- sieve_assistant_request('OpenAI', 'explicit-mock-model', 'Synthetic review context')
  transport <- function(request, key) {
    calls$n <- calls$n + 1L
    'Mock proposal'
  }
  expect_error(sieve_assistant_send(request, 'mock-secret', consent = FALSE, transport = transport), 'Authorize', fixed = TRUE)
  expect_identical(calls$n, 0L)
})
test_that('Anchor: discards a completed older worker generation', {
  jobs <- sieve_efa_jobs()
  key <- list(data = 'one fixed dataset')
  jobs$key <- key
  jobs$generation <- 2L
  jobs$cancelled <- FALSE
  jobs$requested <- 1L
  # The queue holds a record per running worker; this one belongs to the
  # older generation.
  slot <- new.env(parent = emptyenv())
  slot$ks <- 1L
  slot$done <- integer(0)
  slot$key <- key
  slot$generation <- 1L
  slot$started_at <- Sys.time()
  slot$process <- list(
    is_alive = function() FALSE,
    get_result = function() data.frame(
      k = 1L,
      status = 'ok',
      message = 'Controlled old completion'
    )
  )
  jobs$slots <- list(slot)
  sieve_efa_jobs_poll(jobs)
  expect_length(jobs$slots, 0L)
  expect_length(jobs$cache, 0L)
  expect_length(jobs$completed, 0L)
})
