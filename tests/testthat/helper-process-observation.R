# Process observation must never kill the process it is checking.
siever_test_pid_alive <- function(pid) {
  if (.Platform$OS.type == "windows") {
    command <- Sys.which("tasklist")
    if (!nzchar(command)) stop("Process inspection unavailable: tasklist is absent.")
    output <- suppressWarnings(system2(command,
      c("/FI", shQuote(paste("PID eq", pid)), "/FO", "CSV", "/NH"),
      stdout = TRUE, stderr = TRUE
    ))
    return(any(grepl(paste0('","', pid, '",'), output, fixed = TRUE)))
  }
  command <- Sys.which("ps")
  if (!nzchar(command)) stop("Process inspection unavailable: ps is absent.")
  output <- suppressWarnings(system2(command,
    c("-p", as.character(pid), "-o", "pid="),
    stdout = TRUE, stderr = FALSE
  ))
  any(trimws(output) == as.character(pid))
}
