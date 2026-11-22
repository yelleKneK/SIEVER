# The private profile files (a private study of 68 items, its data and
# specification, and a two-row-header file) are not part of the package and
# are not the package's to share. A test reads one from the directory named
# by SIEVER_PRIVATE_PROFILE_DIR, a location the user supplies, and skips when
# the variable is unset or the file is not there.
#
# Author: Ken Kelley

siever_profile_file <- function(name) {
  dirs <- Sys.getenv("SIEVER_PRIVATE_PROFILE_DIR", unset = "")
  dirs <- dirs[nzchar(dirs)]
  for (d in dirs) {
    path <- file.path(path.expand(d), name)
    if (file.exists(path)) return(path)
  }
  NA_character_
}
