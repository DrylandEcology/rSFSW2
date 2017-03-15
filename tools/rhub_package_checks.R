path <- getwd()

if (grepl("rSFSW2", path) && requireNamespace("rhub")) {
  temp <- strsplit(path, .Platform$file.sep, fixed = TRUE)[[1]]
  i <- which(temp == "rSFSW2")
  path_rpackage <- paste(temp[seq_len(i)], collapse = .Platform$file.sep)

  setwd(path_rpackage)

  rhub::check_on_linux()
  rhub::check_on_windows()
  rhub::check_with_sanitizers()

  setwd(path)

} else {
  stop("The source package 'rSFSW2' cannot be found in the current path")
}
